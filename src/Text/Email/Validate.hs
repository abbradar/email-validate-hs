module Text.Email.Validate
	( isValid
	, validate
	, emailAddress
	, canonicalizeEmail
	, EmailAddress -- re-exported
	, localPart
	, domainPart
	, toText
	)
where

import Control.Applicative ((<*))

import Data.Text (Text)
import Data.Attoparsec.Text (parseOnly, endOfInput)

import Text.Email.Parser (EmailAddress, toText, addressHeader, localPart, domainPart)

-- | Smart constructor for an email address
emailAddress :: Text -> Maybe EmailAddress
emailAddress = either (const Nothing) Just . validate

-- | Checks that an email is valid and returns a version of it
--   where comments and whitespace have been removed.
canonicalizeEmail :: Text -> Maybe Text
canonicalizeEmail = fmap toText . emailAddress

-- | Validates whether a particular string is an email address
--   according to RFC5322.
isValid :: Text -> Bool
isValid = either (const False) (const True) . validate

-- | If you want to find out *why* a particular string is not
--   an email address, use this.
validate :: Text -> Either String EmailAddress
validate = parseOnly (addressHeader <* endOfInput)
