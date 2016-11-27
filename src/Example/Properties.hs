{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Example.Properties where









import GHC.Generics
import Test.QuickCheck.Arbitrary.ADT
import Example.Properties.Types.FixedText
import Test.QuickCheck

data Product = Product {
 productNumber       :: ProductNumber,
 productName         :: ProductName,
 version             :: ProductVersion ,
 productCustomer     :: Customer,
 productDescription  :: TText }
  deriving (Eq,Ord,Show,Generic)

data Customer = Customer {
   customerName    :: CustomerName    ,  
   customerNumber  :: CustomerNumber  ,
   customerAddress :: CustomerAddress }
    deriving (Eq,Ord,Show,Generic)

data CustomerAddress = CustomerAddress {
  street :: TText,
  city   :: TText,
  state  :: State
} deriving (Eq,Ord,Show,Generic)

data State = Oklahoma | Texas | Kansas
  deriving (Eq,Ord,Show,Generic)

-- | Base fields
newtype ProductNumber  = ProductNumber
  { unProductNumber  :: TText}
  deriving (Eq,Ord,Show,Generic)

newtype ProductName    = ProductName
  { unProductName    :: TText}
  deriving (Eq,Ord,Show,Generic)

newtype ProductVersion = ProductVersion
  {unProductVersion  :: TText}
  deriving (Eq,Ord,Show,Generic)
newtype CustomerName   = CustomerName
  { unCustomerName   :: TText}
  deriving (Eq,Ord,Show,Generic)
newtype CustomerNumber = CustomerNumber
  { unCustomerNumber :: TText}
  deriving (Eq,Ord,Show,Generic)

-- | 140 characters alphanumeric unicode
type TText = FixedText 140 0 "[a-z1234567890]"
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
   deriving(Eq,Ord,Show,Generic)

data ProductDocument = ProductDocument !Product !Customer !CustomerAddress
 deriving(Eq,Ord,Show,Generic)




toProductRow  :: Product -> ProductRow
toProductRow Product {..} = ProductRow productNumber productName    version productDescription
                                       customerName  customerNumber street  city                state
  where
    Customer {..}        = productCustomer
    CustomerAddress {..} = customerAddress



fromProductRow :: ProductRow -> Product
fromProductRow (ProductRow {..}) = Product rowProductNumber rowName rowVersion customer rowDescription 
  where
    customer        = Customer        rowCustomerName   rowCustomerNumber customerAddress
    customerAddress = CustomerAddress rowCustomerStreet rowCustomerCity   rowCustomerState


    
-- | Arbitrary instances for the above types (separated for readability)

instance Arbitrary ProductVersion where
  arbitrary = genericArbitrary

instance Arbitrary ProductNumber where
  arbitrary = genericArbitrary

instance Arbitrary ProductName where
  arbitrary = genericArbitrary

instance Arbitrary Product where
  arbitrary = genericArbitrary



instance Arbitrary CustomerName where
  arbitrary = genericArbitrary

instance Arbitrary CustomerNumber where
  arbitrary = genericArbitrary

instance Arbitrary State where
  arbitrary = genericArbitrary

instance Arbitrary CustomerAddress where
  arbitrary = genericArbitrary

instance Arbitrary Customer where
  arbitrary = genericArbitrary

