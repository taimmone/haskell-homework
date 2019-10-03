import qualified Data.Map as Map

-- adds new phonebook entry or if an entry already exists, append the phone
addPhonebookEntry :: String -> String -> String -> String -> Map.Map String [Phone] -> Map.Map String [Phone]
addPhonebookEntry personName pt cc pn phonebook = 
    Map.insertWith (++) personName [(readPhone pt cc pn)] phonebook

-- finds a person from a phonebook and returns a map containing the person and all the phones,
-- otherwise returns an empty map. 
findPhonebookEntry :: String -> Map.Map String [Phone] -> Map.Map String (Maybe [Phone])
findPhonebookEntry name phonebook = if Map.member name phonebookEntries 
                then Map.insert name (Map.lookup name phonebookEntries) Map.empty else Map.empty

-- Premade phonebook with entries
phonebookEntries = addPhonebookEntry "PersonA" "WorkMobile" "358" "123456789"
                 $ addPhonebookEntry "PersonB" "PrivateMobile" "598" "112233445"
                 $ addPhonebookEntry "PersonC" "Other" "996" "102030405"
                 $ addPhonebookEntry "PersonA" "PrivateMobile" "358" "987654321" Map.empty

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
        | otherwise = error "Invalid country code."

toPhoneNo :: Integer -> PhoneNo
toPhoneNo pn
        | pn >= 0 = PhoneNo pn
        | otherwise = error "Invalid phone number."

toPhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
toPhone pt cc pn = Phone pt cc pn


readPhone :: String -> String -> String -> Phone
readPhone pt cc pn = toPhone (read pt)
                             (toCountryCode $ parseCC cc)
                             (toPhoneNo $ read pn)
    where parseCC []           = -1
          parseCC ('+':cc)     = parseCC cc
          parseCC ('0':'0':cc) = parseCC cc
          parseCC cc           = if cc `elem` listOfSomeCountryCodes then read cc else error "Unknown country code."
