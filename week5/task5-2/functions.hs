module Functions where

import Datadefs

-- Predefined list of some country codes that are valid -----------------------
listOfSomeCountryCodes = ["1", "242", "236", "243", "420", "358", "996", "598", "666"]
-------------------------------------------------------------------------------

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
