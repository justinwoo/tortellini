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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Tortellini where

import Tortellini.Parser (parseIniDocument)

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
import GHC.Exts
import GHC.Generics
import GHC.TypeLits

-- parseIni ::
--      Generic record
--   => ReadDocumentSections (Rep record)
--   => Text
--   -> Either UhOhSpaghettios record
-- parseIni s = do
--   doc <- first (pure . ErrorInParsing . T.pack) $ parseIniDocument s
--   runExcept $ to <$> readDocumentSections doc

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

class GenericInnerOp (c :: * -> Constraint) k (f :: * -> *) where
  genericInnerOp :: forall a
     . (forall k' a'. c k' => k' -> Except UhOhSpaghettios a')
    -> HashMap Text k
    -> Except UhOhSpaghettios (f a)

instance GenericInnerOp c k a => GenericInnerOp c k (D1 meta a) where
  genericInnerOp f hm = M1 <$> genericInnerOp @c @k @a f hm

instance GenericInnerOp c k a => GenericInnerOp c k (C1 meta a) where
  genericInnerOp f hm = M1 <$> genericInnerOp @c @k @a f hm

instance GenericInnerOp c k U1 where
  genericInnerOp _ _ = pure U1

instance
  ( GenericInnerOp c k a
  , GenericInnerOp c k b
  ) => GenericInnerOp c k (a :*: b) where
  genericInnerOp f hm = (:*:) <$> genericInnerOp @c @k @a f hm <*> genericInnerOp @c @k @b f hm

instance
  ( KnownSymbol name
  , ReadIniField t
  ) => GenericInnerOp ReadIniField Text (S1 ('MetaSel ('Just name) z x v) (K1 r t)) where
  genericInnerOp f hm =
    case HM.lookup (T.toLower name) hm of
      Nothing ->
        throwE . pure . ErrorAtSectionProperty name . Error
        $ "Missing field in section"
      Just x -> do
        value <- withExcept' $ f x
        pure $ M1 (K1 value)
    where
      name = T.pack $ symbolVal @name Proxy
      withExcept' = withExcept . fmap $ ErrorAtSectionProperty name

instance
  ( KnownSymbol name
  , Generic t
  , rep ~ Rep t
  ) => GenericInnerOp c (HashMap Text Text) (S1 ('MetaSel ('Just name) z x v) (K1 r t)) where
  genericInnerOp f hm =
    case HM.lookup (T.toLower name) hm of
      Nothing ->
        throwE . pure . ErrorAtSectionProperty name . Error
        $ "Missing field in section"
      Just x -> do
        value <- withExcept' $ f x
        pure $ M1 (K1 value)
    where
      name = T.pack $ symbolVal @name Proxy
      withExcept' = withExcept . fmap $ ErrorAtSectionProperty name
