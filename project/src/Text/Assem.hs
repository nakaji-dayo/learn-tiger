{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Text.Assem where


import           Control.Applicative
import           Data.Attoparsec.Text as P
import           Data.String
import           Data.Text            (pack, unpack)
import           Debug.Trace

data RawAssemToken = AString String | ANum Int | ASrc Int | ADst Int | AJmp Int | ALbl String
  deriving (Show, Eq)

newtype RawAssem = RawAssem [RawAssemToken]
  deriving (Show, Eq, Semigroup, Monoid)

instance IsString RawAssem where
  fromString = RawAssem . either (error . show) id .  parseOnly p . pack
    where
      pref c prefix = do
        string prefix
        d <- digit
        pure $ c $  read [d]
      pstr = AString <$> many letter
      p = (pref ASrc "`s" <|> pref ADst "`d" <|> pref AJmp "`j" <|> pstr) `sepBy` space
