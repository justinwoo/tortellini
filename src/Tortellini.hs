{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

parseIni ::
     Generic record
  => ReadDocumentSections (Rep record)
  => Text
  -> Either UhOhSpaghettios record
parseIni s = do
  doc <- first (pure . ErrorInParsing . T.pack) $ parseIniDocument s
  runExcept $ to <$> readDocumentSections doc

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

class ReadDocumentSections (f :: * -> *) where
  readDocumentSections ::
       HashMap Text (HashMap Text Text)
    -> Except UhOhSpaghettios (f a)

instance ReadDocumentSections a => ReadDocumentSections (D1 meta a) where
  readDocumentSections hm = M1 <$> readDocumentSections @a hm

instance ReadDocumentSections a => ReadDocumentSections (C1 meta a) where
  readDocumentSections hm = M1 <$> readDocumentSections @a hm

instance
  ( ReadDocumentSections a
  , ReadDocumentSections b
  ) => ReadDocumentSections (a :*: b) where
  readDocumentSections hm = liftA2 (:*:) (readDocumentSections @a hm) (readDocumentSections @b hm)

instance
  ( KnownSymbol name
  , Generic t
  , rep ~ Rep t
  , ReadSection rep
  ) => ReadDocumentSections (S1 ('MetaSel ('Just name) z x c) (K1 r t)) where
  readDocumentSections hm =
    case HM.lookup (T.toLower name) hm of
      Nothing ->
        throwE . pure . ErrorAtDocumentProperty name . Error
        $ "Missing field in document"
      Just x -> do
        value <- withExcept' $ to <$> readSection @rep x
        pure $ M1 (K1 value)
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
