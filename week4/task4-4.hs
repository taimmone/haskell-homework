-- Task 4.4
-- Find phonebook entries with a person's name
findPhonebookEntry :: String -> [PhonebookEntry] -> [PhonebookEntry]
findPhonebookEntry personName entries = filter (\entry -> name entry == personName) entries

-- Create a new phonebook entry (must be stored in a new variable)
addPhonebookEntry :: String -> String -> String -> String -> [PhonebookEntry] -> [PhonebookEntry]
addPhonebookEntry personName pt cc pn entries
    | entry `elem` entries = entries
    | otherwise = entries ++ [entry]
    where entry = PhonebookEntry { name = personName, phone = readPhone pt cc pn }


-- Premade phonebook with entries to test the findPhonebookEntry function
phonebookEntries = 
    [ PhonebookEntry {name = "PersonA", phone = Phone {phoneType = WorkMobile, countryCode = CountryCode 358, phoneNo = PhoneNo 123456789}}
    , PhonebookEntry {name = "PersonB", phone = Phone {phoneType = PrivateMobile, countryCode = CountryCode 598, phoneNo = PhoneNo 112233445}}
    , PhonebookEntry {name = "PersonC", phone = Phone {phoneType = Other, countryCode = CountryCode 996, phoneNo = PhoneNo 102030405}}
    , PhonebookEntry {name = "PersonA", phone = Phone {phoneType = PrivateMobile, countryCode = CountryCode 358, phoneNo = PhoneNo 987654321}}
    ]
    
-------------------------------------------------------------------------------

listOfSomeCountryCodes = ["1", "242", "236", "243", "420", "358", "996", "598", "666"]

-- All the data type definitions
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

data PhonebookEntry = PhonebookEntry { name  :: String
                                     , phone :: Phone
                                     } deriving (Show, Eq)

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

-- Function to create a Phone
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