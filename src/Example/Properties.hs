{-# LANGUAGE RecordWildCards #-}
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
 productNumber       :: ProductNumber,
 productName         :: ProductName,
 version             :: ProductVersion ,
 productCustomer     :: Customer,
 productDescription  :: TText }

data Customer = Customer {
   customerName    :: CustomerName    ,  
   customerNumber  :: CustomerNumber  ,
   customerAddress :: CustomerAddress  
  }

data CustomerAddress = CustomerAddress {
  street :: TText,
  city   :: TText,
  state  :: State
} deriving (Eq,Ord,Show)

data State = Oklahoma | Texas | Kansas
  deriving (Eq,Ord,Show)

-- | Base fields
newtype ProductNumber  = ProductNumber
  { unProductNumber  :: TText}
  deriving (Eq,Ord,Show)

newtype ProductName    = ProductName
  { unProductName    :: TText}
  deriving (Eq,Ord,Show)

newtype ProductVersion = ProductVersion
  {unProductVersion  :: TText}
  deriving (Eq,Ord,Show)
newtype CustomerName   = CustomerName
  { unCustomerName   :: TText}
  deriving (Eq,Ord,Show)
newtype CustomerNumber = CustomerNumber
  { unCustomerNumber :: TText}
  deriving (Eq,Ord,Show)

-- | 140 characters alphanumeric unicode
type TText = FixedText 140 0 "[[:alnum:]]"


data ProductRow = ProductRow
  { rowProductNumber  :: ProductNumber,
    rowName           :: ProductName,
    rowVersion        :: ProductVersion,
    rowDescription    :: TText,
    rowCustomerName   :: CustomerName,
    rowCustomerNumber :: CustomerNumber,    
    rowCustomerStreet :: TText,
    rowCustomerCity   :: TText,
    rowCustomerState  :: State}


data ProductDocument = ProductDocument !Product !Customer !CustomerAddress


toProductRow  :: Product -> ProductRow
toProductRow Product {..} = ProductRow productNumber productName    productVersion productDescription
                                          customerName  customerNumber customerStreet customerCity customerState
  where
    Customer {..}        = productCustomer
    CustomerAddress {..} = customerAddress



fromProductRow :: ProductRow -> Product
fromProductRow (ProductRow {..}) = Product rowProductNumber rowName rowVersion customer rowDescription 
  where
    customer        = Customer        rowCustomerName   rowCustomerNumber customerAddress
    customerAddress = CustomerAddress rowCustomerStreet rowCustomerCity   rowCustomerState
    
