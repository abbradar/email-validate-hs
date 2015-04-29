{-# LANGUAGE OverloadedStrings #-}

module Text.Email.Header.Grammar
    ( -- | Utils
      between
    , between1
    , skipWhile1
      -- | Grammar
    , cfws
    , fws
    , crlf
    , isWsp
    , quotedPair
    , isNotAscii
    , isObsNoWsCtl
    , dottedAtoms
    , atom
    , quotedString
    )
where

import Prelude hiding (takeWhile)
import Data.Char
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T

import Data.Attoparsec.Text

-- | Sequences input between two parameters which values are ignored.
between :: Applicative f => f l -> f r -> f a -> f a
between l r x = l *> x <* r

-- | A variant of 'between' when left and right parameter are the same.
between1 :: Applicative f => f lr -> f a -> f a
between1 lr = between lr lr

-- | A variant of 'skipWhile' which requires the predicate to succeed on at least one character.
skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 x = satisfy x >> skipWhile x

-- | Matches optional CFWS according to RFC 5322. Returns FWS spaces.
cfws :: Parser Text
cfws = T.concat <$> ((:) <$> fws <*> many (comment *> fws))

-- | Matches optional FWS according to RFC 5322. Returns spaces from the beginning.
fws :: Parser Text
fws = takeWhile isWsp <* skipMany (crlf >> takeWhile1 isWsp)

-- | Matches CRLF sequence.
crlf :: Parser Text
crlf = string "\r\n"

-- | Selects WSP characters according to RFC 5234.
isWsp :: Char -> Bool
isWsp x = x == ' ' || x == '\t'

comment :: Parser ()
comment = between (char '(' >> optional fws) (char ')') $ skipMany (commentContent >> optional fws)

commentContent :: Parser ()
commentContent = skipWhile1 isCommentText <|> void quotedPair <|> comment

isCommentText :: Char -> Bool
isCommentText x = inClass "\33-\39\42-\91\93-\126" x || isObsNoWsCtl x

-- | Matches quoted pair according to RFC 5322 (matches to RFC 5321).
quotedPair :: Parser Text
quotedPair = (T.cons '\\' . T.singleton) <$> (char '\\' *> satisfy isQuotedPair)

isQuotedPair :: Char -> Bool
isQuotedPair x = inClass "\32-\126" x || isNotAscii x

-- | Selects UTF8 non-ASCII characters according to RFC 6532.
isNotAscii :: Char -> Bool
isNotAscii = not . isAscii

-- | Selects obsolete CTL characters according to RFC 5322.
isObsNoWsCtl :: Char -> Bool
isObsNoWsCtl = inClass "\1-\8\11-\12\14-\31\127"

-- | Matches dotted atom according to RFC 5322.
dottedAtoms :: Parser Text -> Parser Text
dottedAtoms cws = T.intercalate "." <$> atom `sepBy1` (between1 cws $ char '.')

-- | Matches atom according to RFC 5322.
atom :: Parser Text
atom = takeWhile1 isAtomText

isAtomText :: Char -> Bool
isAtomText x = inClass "a-zA-Z0-9!#$%&'*+/=?^_`{|}~-" x || isNotAscii x

-- | Matches quoted string according to RFC 5322.
quotedString :: Parser Text -> Parser Text
quotedString ws =
    T.concat <$>
        (\x -> ["\""] ++ x ++ ["\""]) <$>
            between (char '"' *> optional ws) (char '"')
                (many $ quotedContent <* optional ws)

quotedContent :: Parser Text
quotedContent = takeWhile1 isQuotedText <|> quotedPair

isQuotedText :: Char -> Bool
isQuotedText x = inClass "\33\35-\91\93-\126" x || isNotAscii x
