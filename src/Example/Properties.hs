
module Example.Properties where





newtype ProductNumber = ProductNumber { _unProductNumber :: String}

newtype ProductName = ProductName { _unProductName :: String}


data Product = Product {
 _productNumber       :: ProductNumber,
 _productName         :: ProductName,
 _version             :: String,
 _productCustomer     :: String,
 _productDescription  :: String,
 _preparedBy          :: String,
 _productDate         :: String,
 _productNotes        :: String }
