{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# LANGUAGE PolyKinds #-}

import Tortellini
import Tortellini.Parser
import Data.Either
import Data.HashMap.Strict
import Data.Text
import Control.Monad.Trans.Except
import GHC.Generics

data Config = Config
  { section1 :: Section1
  , section2 :: Section2
  , section3 :: Section3
  , section4 :: Section4
  } deriving (Show, Eq, Generic)

data Section1 = Section1
  { apple :: Text
  } deriving (Show, Eq, Generic)

data Section2 = Section2
  { watermelon :: Bool
  , kiwi :: Int
  } deriving (Show, Eq, Generic)

data Section3 = Section3
  {} deriving (Show, Eq, Generic)

data Section4 = Section4
  {} deriving (Show, Eq, Generic)

testDoc :: Text
testDoc = intercalate "\n"
  [ "[section1]"
  , "apple=banana"
  , "[section2]"
  , "watermelon=true"
  , "kiwi=1"
  , "[section3]"
  , "[section4]"
  ]

sectionTest1 :: HashMap Text Text
sectionTest1 = fromList [("apple", "banana")]

sectionTest2 :: HashMap Text Text
sectionTest2 = fromList [("missingapple", "banana")]

main :: IO ()
main = do
  putStrLn ""
  print . isRight . runExcept $ (to <$> readSection @(Rep Section1) sectionTest1 :: Except UhOhSpaghettios Section1)
  print . isRight . runExcept $ (to <$> readSection @(Rep Section2) sectionTest2 :: Except UhOhSpaghettios Section2)
  print $ parseIniDocument testDoc
  print (parseIni Config testDoc :: Either UhOhSpaghettios Config)
  putStrLn "Test suite not yet implemented"
