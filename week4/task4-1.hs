type CountryCode = Integer
type PhoneNo     = Integer

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other
    deriving (Show, Eq, Read)

data Phone = Phone { phoneType :: PhoneType
                   , countryCode :: CountryCode
                   , phoneNo :: PhoneNo
                   } deriving (Show, Eq, Read)

-- Function to create a Phone type 
toPhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
toPhone pt cc pn
    | cc >= 0 || pn >= 0 = Phone {phoneType = pt, countryCode = cc, phoneNo = pn}
    | otherwise = error "CountryCode and PhoneNo must be positive integers."