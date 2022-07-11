module Data.ABC where

import Prelude

import Language.TypeScript.DTS as DTS
import PursTs.Class (class ToTsType)

data A


data B

data C

data D

data E

data F

data G

data H

data I

data J

data K

data L

data M

data N

data O

data P

data Q

data R

data S

data T

data U

data V

data W

data X

data Y

data Z


instance toTsTypeA :: ToTsType A where
  toTsType _ = DTS.TypeVar $ DTS.Name "A"

instance toTsTypeB :: ToTsType B where
  toTsType _ = DTS.TypeVar $ DTS.Name "B"

instance toTsTypeC :: ToTsType C where
  toTsType _ = DTS.TypeVar $ DTS.Name "C"

instance toTsTypeD :: ToTsType D where
  toTsType _ = DTS.TypeVar $ DTS.Name "D"

instance toTsTypeE :: ToTsType E where
  toTsType _ = DTS.TypeVar $ DTS.Name "E"


instance toTsTypeZ :: ToTsType Z where
  toTsType _ = DTS.TypeVar $ DTS.Name "Z"

