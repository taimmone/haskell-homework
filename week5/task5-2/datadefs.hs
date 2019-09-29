module Datadefs
( CountryCode(..)
, PhoneNo(..)
, PhoneType(..)
, Phone(..)
) where

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

instance Show CountryCode where
    show (CountryCode cc) = '+' : show cc

instance Show PhoneNo where
    show (PhoneNo pn) = show pn

instance Show Phone where
    show (Phone pt cc pn) = maybe "" (foldr (:) " " . show) cc
                            ++ show pn
                            ++ maybe "" ((++) " (" . foldr (:) ")" . show) pt