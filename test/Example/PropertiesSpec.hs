{-# LANGUAGE TypeInType #-}
module Example.PropertiesSpec (tests) where

import Example.Properties ()

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Example.Properties.Types.FixedText
import Data.Monoid



tests :: TestTree
tests = testGroup "Tests" [properties ]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "FixedText properties"
  [ QC.testProperty "((mempty <> str) == str)"                           leftIdMonoid
  , QC.testProperty "((str <> mempty) == str)"                           rightIdMonoid
  , QC.testProperty "(strA <>  strB) <> strC == strA <> (strB  <> strC)" associativityMonoid
  ]


type ExampleFixedText = FixedText 10 0 "[[01233456789]{0,3}"
leftIdMonoid  :: ExampleFixedText  -> Bool
leftIdMonoid str = ((mempty <> str) == str) 

rightIdMonoid  :: ExampleFixedText  -> Bool
rightIdMonoid str = ((str <> mempty) == str)                   

associativityMonoid :: ExampleFixedText  ->
                       ExampleFixedText  ->
                       ExampleFixedText  -> Bool
associativityMonoid strA strB strC = leftAsc == rightAsc
  where
    leftAsc  = (strA <>  strB) <> strC
    rightAsc =  strA <> (strB  <> strC)
