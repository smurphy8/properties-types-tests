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


data Product = Product {
 _productNumber       :: ProductNumber,
 _productName         :: ProductName,
 _version             :: ProductVersion ,
 _productCustomer     :: Customer,
 _productDescription  :: TText }

data Customer = Customer {
   _customerName    :: CustomerName    ,  
   _customerNumber  :: CustomerNumber  ,
   _customerAddress :: CustomerAddress  
  }

data CustomerAddress = CustomerAddress {
  _street :: TText,
  _city   :: TText,
  _state  :: State
} deriving (Eq,Ord,Show)

data State = Oklahoma | Texas | Kansas
  deriving (Eq,Ord,Show)

-- | Base fields
newtype ProductNumber  = ProductNumber
  { _unProductNumber  :: TText}
  deriving (Eq,Ord,Show)

newtype ProductName    = ProductName
  { _unProductName    :: TText}
  deriving (Eq,Ord,Show)

newtype ProductVersion = ProductVersion
  {_unProductVersion  :: TText}
  deriving (Eq,Ord,Show)
newtype CustomerName   = CustomerName
  { _unCustomerName   :: TText}
  deriving (Eq,Ord,Show)
newtype CustomerNumber = CustomerNumber
  { _unCustomerNumber :: TText}
  deriving (Eq,Ord,Show)

-- | 140 characters alphanumeric unicode
type TText = FixedText 140 0 "[[:alnum:]]"
