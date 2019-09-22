-- Main function of this task is readPhone :: String -> String -> String -> Phone.
-- Example> readPhone "WorkMobile" "358" "123456789"
-- +358 123456789 (WorkMobile)

-- Use these to check that the function doesn't throw an exception
listOfSomeCountryCodes = ["1", "242", "236", "243", "420", "358", "996", "598", "666"]

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other
    deriving (Show, Eq, Read)

data CountryCode = CountryCode Integer
    deriving Eq

data PhoneNo = PhoneNo Integer
    deriving Eq

data Phone = Phone { phoneType   :: PhoneType
                   , countryCode :: CountryCode
                   , phoneNo     :: PhoneNo
                   } deriving Eq

data PhoneBook = PhoneBook { name  :: String
                           , phone :: Phone
                           }

-- Show instances for the data types
instance Show CountryCode where
    show (CountryCode cc) = '+' : show cc

instance Show PhoneNo where
    show (PhoneNo pn) = show pn

instance Show Phone where
    show (Phone pt cc pn) = show cc ++ " " ++ show pn ++ " (" ++ show pt ++ ")"

-- Functions to create contry code and phone number data types
toCountryCode :: Integer -> CountryCode
toCountryCode cc
    | cc >= 0 = CountryCode cc
    | otherwise = error "Country code must be a positive integer."

toPhoneNo :: Integer -> PhoneNo
toPhoneNo pn
    | pn >= 0 = PhoneNo pn
    | otherwise = error "Phone number must be a positive integer."

-- Function to create a Phone type
toPhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
toPhone pt cc pn = Phone { phoneType=pt, countryCode=cc, phoneNo=pn }

readPhone :: String -> String -> String -> Phone
readPhone pt cc pn = 
    Phone { phoneType = read pt
          , countryCode = toCountryCode . read $ parseCountryCode cc
          , phoneNo = toPhoneNo $ read pn
          }
    where parseCountryCode cc
            | length cc < 1 = error "Invalid country code."
            | head cc == '+' = (parseCountryCode $ tail cc)
            | take 2 cc == "00" = (parseCountryCode $ drop 2 cc)
            | cc `elem` listOfSomeCountryCodes = cc
            | otherwise = error "Invalid country code."

-- findPhoneBookEntries :: String -> []