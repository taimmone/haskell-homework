-- Predefined list of some country codes that are valid
listOfSomeCountryCodes = ["1", "242", "236", "243", "420", "358", "996", "598", "666"]

-------------------------------------------------------------------------------
-- All the data type definitions
newtype CountryCode = CountryCode Integer
    deriving Eq

newtype PhoneNo = PhoneNo Integer
    deriving Eq

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other
    deriving (Show, Eq, Read)

data Phone = Phone { phoneType   :: Maybe PhoneType
                   , countryCode :: Maybe CountryCode
                   , phoneNo     :: PhoneNo
                   } deriving Eq

-- Show instances for the data types
instance Show CountryCode where
    show (CountryCode cc) = '+' : show cc

instance Show PhoneNo where
    show (PhoneNo pn) = show pn

instance Show Phone where
    show (Phone pt cc pn) = maybe "" (foldr (:) " " . show) cc
                            ++ show pn
                            ++ maybe "" ((++) " (" . foldr (:) ")" . show) pt


toCountryCode :: Integer -> Maybe CountryCode
toCountryCode cc
        | cc >= 0 = Just $ CountryCode cc
        | otherwise = Nothing

toPhoneNo :: Integer -> PhoneNo
toPhoneNo pn
        | pn >= 0 = PhoneNo pn
        | otherwise = error "Invalid phone number."

toPhone :: Maybe PhoneType -> Maybe CountryCode -> PhoneNo -> Phone
toPhone pt cc pn = Phone pt cc pn


readPhone :: String -> String -> String -> Phone
readPhone pt cc pn = toPhone (if pt /= "" then Just (read pt) else Nothing)
                             (toCountryCode $ parseCC cc)
                             (toPhoneNo $ read pn)
    where parseCC []           = -1
          parseCC ('+':cc)     = parseCC cc
          parseCC ('0':'0':cc) = parseCC cc
          parseCC cc           = if cc `elem` listOfSomeCountryCodes then read cc else error "Unknown country code."
