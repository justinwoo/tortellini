{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module Tortellini where

import Tortellini.Parser (parseIniDocument)

import Control.Applicative
import Control.Monad.Trans.Except
import qualified Data.Attoparsec.Text as AP
import Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits

parseIni :: forall record xs topCtor
   . Generic record
  => xs ~ GRowToList (Rep record)
  => ReadDocumentSections xs topCtor record
  => topCtor
  -> Text
  -> Either UhOhSpaghettios record
parseIni topCtor s = do
  doc <- first (pure . ErrorInParsing . T.pack) $ parseIniDocument s
  runExcept $ readDocumentSections @xs doc topCtor

data UhOhSpaghetto
  = Error Text
  | ErrorAtDocumentProperty Text UhOhSpaghetto
  | ErrorAtSectionProperty Text UhOhSpaghetto
  | ErrorInParsing Text
  deriving (Show)

type UhOhSpaghettios = NonEmpty UhOhSpaghetto

class ReadIniField a where
  readIniField :: Text -> Except UhOhSpaghettios a

instance ReadIniField Text where
  readIniField = pure

instance ReadIniField Int where
  readIniField s = case AP.parseOnly AP.decimal s of
    Left e -> throwE (pure . Error . T.pack $ e)
    Right x -> pure x

instance ReadIniField Bool where
  readIniField s
    | T.toLower s == T.pack "true" = pure True
    | T.toLower s == T.pack "false" = pure False
    | otherwise = throwE . pure . Error $
      "expected true/false, got " <> s

instance (ReadIniField a) => ReadIniField [a] where
  readIniField s = traverse readIniField $ T.splitOn "," s

class ReadDocumentSections (xs :: [(Symbol, *)]) from to
  | xs from -> to where
  readDocumentSections ::
       HashMap Text (HashMap Text Text)
    -> from
    -> Except UhOhSpaghettios to

instance ReadDocumentSections '[] from from where
  readDocumentSections _ = pure

instance
  ( KnownSymbol name
  , Generic a
  , repA ~ Rep a
  , as ~ GRowToList repA
  , ReadSection repA
  , ReadDocumentSections tail from' to
  ) => ReadDocumentSections ('(name, a) ': tail ) (a -> from') to where
  readDocumentSections hm f =
    case HM.lookup (T.toLower name) hm of
      Nothing ->
        throwE . pure . ErrorAtDocumentProperty name . Error
        $ "Missing field in document"
      Just x -> do
        value <- withExcept' $ to <$> readSection @repA x
        readDocumentSections @tail hm (f value)
    where
      name = T.pack $ symbolVal @name Proxy
      withExcept' = withExcept . fmap $ ErrorAtDocumentProperty name

class ReadSection (f :: * -> *) where
  readSection :: HashMap Text Text -> Except UhOhSpaghettios (f a)

instance ReadSection a => ReadSection (D1 meta a) where
  readSection hm = M1 <$> readSection @a hm

instance ReadSection a => ReadSection (C1 meta a) where
  readSection hm = M1 <$> readSection @a hm

instance ReadSection U1 where
  readSection _ = pure U1

instance
  ( ReadSection a
  , ReadSection b
  ) => ReadSection (a :*: b) where
  readSection hm = liftA2 (:*:) (readSection @a hm) (readSection @b hm)

instance
  ( KnownSymbol name
  , ReadIniField t
  ) => ReadSection (S1 ('MetaSel ('Just name) z x c) (K1 r t)) where
  readSection hm =
    case HM.lookup (T.toLower name) hm of
      Nothing ->
        throwE . pure . ErrorAtSectionProperty name . Error
        $ "Missing field in section"
      Just x -> do
        value <- withExcept' $ readIniField x
        pure $ M1 (K1 value)
    where
      name = T.pack $ symbolVal @name Proxy
      withExcept' = withExcept . fmap $ ErrorAtSectionProperty name

type family GRowToList (r :: * -> *) :: [(Symbol, *)] where
  GRowToList (l :*: r)
    = GRowToList l ++ GRowToList r
  GRowToList (S1 ('MetaSel ('Just name) _ _ _) (Rec0 a))
    = '[ '(name, a) ]
  GRowToList (M1 _ m a)
    = GRowToList a
  GRowToList U1 = '[]

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)
