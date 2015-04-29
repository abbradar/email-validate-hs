{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}

module Text.Email.Parser
    ( address
    , addressHeader
    , localPart
    , domainPart
    , EmailAddress
    , toText
    )
where

import Data.Monoid
import Data.Maybe
import Control.Applicative
import Data.Word

import Data.Text (Text)
import qualified Data.Text as T

import Data.Attoparsec.Text

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import qualified Text.Read as Read

import Text.Email.Header.Grammar
import Text.Domain.Parser

-- | Represents an email address.
data EmailAddress = EmailAddress { -- | Extracts the local part of an email address.
                                   localPart :: !Text
                                   -- | Extracts the domain part of an email address.
                                 , domainPart :: !(Either Domain Text)
                                 }
    deriving (Eq, Ord, Data, Typeable, Generic)

instance Show EmailAddress where
    show = show . toText

instance Read EmailAddress where
    readsPrec n s = mapMaybe go $ readsPrec n s where
        go (adr, rem) = case parseOnly (addressHeader <* endOfInput) adr of
            Right r -> Just (r, rem)
            Left _ -> Nothing

-- | Converts an email address back to a Text
toText :: EmailAddress -> Text
toText (EmailAddress l d) = l <> "@" <> d'
    where d' = case d of
                   Left d -> d
                   Right l -> "[" <> l <> "]"

someAddress :: Parser Text -> Parser Text -> Parser EmailAddress
someAddress cws ws = do
    l <- local cws ws
    _ <- char '@'
    d <- Left <$> domainWS cws <|> Right <$> (cws *> domainLiteral ws <* cws)
    return (EmailAddress l d)

-- | A parser for email addresses (RFC 5321). Domain names are restricted
--   to RFC 5890.
address :: Parser EmailAddress
address = someAddress (return "") (fail "No folding whitespace allowed")

-- | A parser for email addresses inside header fields (RFC 5322).
--   We don't allow obsolete local-part syntax (defined in section 4.4)
--   because it's not allowed in SMTP mailbox fields (RFC 5321) anyway.
addressHeader :: Parser EmailAddress
addressHeader = someAddress cfws fws

local :: Parser Text -> Parser Text -> Parser Text
local cws ws = between1 cws (dottedAtoms cws <|> quotedString ws)

domainLiteral :: Parser Text -> Parser Text
domainLiteral ws = T.concat <$> between (char '[' *> optional ws) (char ']') (many $ takeWhile1 isDomainText <* optional ws)

isDomainText :: Char -> Bool
isDomainText x = inClass "\33-\90\94-\126" x || isObsNoWsCtl x
