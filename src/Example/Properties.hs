{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Example.Properties where


import Data.Text (pack,Text)
import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import GHC.TypeLits (Nat,natVal,KnownNat,Symbol,KnownSymbol,symbolVal)
import Text.Regex.Lens
import Text.Regex.Base
import Text.Regex.Posix
import Control.Lens
import Example.Properties.Types.FixedText



-- | Product numbers  are: [[:alnum:]]
-- unicode enc
-- Max length 140
-- Min length 1
newtype ProductNumber = ProductNumber { _unProductNumber :: Text}
  deriving (Show,Eq,Ord)
-- | Product names are: [[:alnum:]]
-- unicode enc
-- Max length 140
-- Min length 1
newtype ProductName   = ProductName { _unProductName :: Text}

newtype ProductVersion = ProductVersion {_unProductVersion :: Text}

-- | Customer names must be one of: [[:alnum:],'"-_*!@]
-- unicode enc
-- Max length 140
-- Min length 1
newtype CustomerName = CustomerName { _unCustomerName :: Text}


-- | Customer numbers are: [[:alnum:]]
-- unicode enc
-- Max length 140
-- Min length 1
newtype CustomerNumber = CustomerNumber { _unCustomerNumber :: Text}

data State = Oklahoma | Texas | Kansas 


-- | Customer Address Properties
-- One of the given States
-- city and street must be alpha numeric and
-- unicode enc
-- less than 100 characters
data CustomerAddress = CustomerAddress {
  _street :: Text,
  _city   :: Text,
  _state  :: State
}



data Customer = Customer {
   _customerName    :: CustomerName    ,  
   _customerNumber  :: CustomerNumber  ,
   _customerAddress :: CustomerAddress  
  }


-- | Product Properties
-- Product descriptions are defined as valid unicode
-- not including: (0x00-00-00 -> 0x00-00-1f)
-- not including: (0x00-00-7f -> 0x00-00-9f)
-- which are program commands
data Product = Product {
 _productNumber       :: ProductNumber,
 _productName         :: ProductName,
 _version             :: ProductVersion ,
 _productCustomer     :: Customer,
 _productDescription  :: Text }


