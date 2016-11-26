{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Module      : Example.Properties.Types.FixedText
Description : Text type with constraints on allowable input and length
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

FixedText is designed to format incoming text strings with a set of valid
characters that are known at compile time.

| -}


module Example.Properties.Types.FixedText where



import Data.Text (pack,Text)
import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import GHC.TypeLits (Nat,natVal,KnownNat,Symbol,KnownSymbol,symbolVal)
import Text.Regex.Lens
import Text.Regex.Base
import Text.Regex.Posix
import Control.Lens




-- | Set of things that can go wrong with Fixed Text construction
data FixedTextErrors = FixedTextErrorMin
                     | FixedTextErrorRegex String String
                     | FixedTextErrorMax
  deriving (Show,Eq,Ord)


-- | Text array with max size and min size and character set
newtype  FixedText (lengthMax :: Nat)
                   (lengthMin :: Nat)
                   (regex     :: Symbol) 
           = FixedText { _unFixedText :: Text}
  deriving (Show,Ord,Eq)


fixedTextFromString :: forall max min regex . 
  ( KnownNat    max
  , KnownNat    min
  , KnownSymbol regex) => String -> Either FixedTextErrors (FixedText max min regex)
fixedTextFromString str = final
  where
    max'          = fromIntegral $ natVal (Proxy :: Proxy max)
    min'          = fromIntegral $ natVal (Proxy :: Proxy min)    
    isTooLittle   = length str < min'
    regexStr      = symbolVal (Proxy :: Proxy regex)
    trimmedString = take max' str
    notRegex      = notValidRegex regexStr trimmedString
    final
      | isTooLittle = Left   FixedTextErrorMin
      | notRegex    = Left (FixedTextErrorRegex regexStr trimmedString)
      | otherwise   = (Right . FixedText .   pack) trimmedString  

notValidRegex :: String -> String -> Bool
notValidRegex regexStr txt =  regexPart /= txt
  where
    regexPart     = txt ^. regex compiledRegex . matchedString
    compiledRegex :: Regex
    compiledRegex = makeRegex regexStr




-- | Just works, example
exampleFixedText  :: Either FixedTextErrors (FixedText 30 1 "[[:alnum:]]")
exampleFixedText = fixedTextFromString "exampleText1234" 

-- | Cut off too much input.
exampleOverFlowProtection :: Either FixedTextErrors (FixedText 10 1 "[[:alnum:]]")
exampleOverFlowProtection = fixedTextFromString "exampleText1234" 

-- | Reject if below min input
exampleUnderFlowProtection :: Either FixedTextErrors (FixedText 200 20 "[[:alnum:]]")
exampleUnderFlowProtection = fixedTextFromString "exampleText1234"

-- | Reject if invalid char
exampleInvalidChar :: Either FixedTextErrors (FixedText 30 1 "[[:digit:]]")
exampleInvalidChar = fixedTextFromString "exampleNotAllDigits"
