{-# LANGUAGE OverloadedStrings #-}

module Text.Domain.Parser
    ( Domain
    , domainWS
    , domain
    )
where

import Prelude hiding (takeWhile)
import Control.Applicative
import Control.Monad
import Data.Char (isAscii, isDigit)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Data.Attoparsec.Text
import qualified Data.Attoparsec.ByteString.Char8 as PB

type Domain = Text

-- | Parses preferred domain names according to RFC 3696.
--
--   Unicode characters are supported as per RFC 5890, but the parser does not
--   implement RFCs 5892 and 5893 correctly, allowing any non-ASCII character.
--   Skips whitespaces.
domainWS :: Parser Text -> Parser Domain
domainWS cws = do
    r <- label cws `sepBy1` char '.'
    when (all isDigit $ T.unpack $ last r) $ fail "Top-level domain name cannot be all-numeric"
    return (T.intercalate "." r)

domain :: Parser Domain
domain = domainWS (return "")

maybeCons :: Maybe Char -> Text -> Text
maybeCons c t = maybe t (\x -> T.cons x t) c

label :: Parser Text -> Parser Text
label cws = do
    _ <- cws
    chr <- satisfy isAlnum
    mid <- many (maybeCons <$> optional (char '-') <*> takeWhile1 isAlnum)
    _ <- cws
    return $ T.concat (T.singleton chr : mid)

isAlpha :: Char -> Bool
isAlpha x = PB.isAlpha_ascii x || not (isAscii x)

isAlnum :: Char -> Bool
isAlnum x = isDigit x || isAlpha x
