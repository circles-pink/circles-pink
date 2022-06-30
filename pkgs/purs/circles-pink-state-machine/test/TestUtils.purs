module Test.TestUtils where

import Prelude

import CirclesCore (User)
import CirclesPink.Data.Address (Address(..))
import Control.Error.Util (hush)
import Data.Argonaut (decodeJson, fromString)
import Data.Maybe (fromJust)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
addrA :: Address
addrA = unsafeMkAddr "0x0142e59D7e0744e984aCa46Bbe9A7eF5C3Fa50ba"

userA :: User
userA = mkDummyUser addrA

--
addrB :: Address
addrB = unsafeMkAddr "0x01F19302779CfB177b1F928386FCd61eE6856057"

userB :: User
userB = mkDummyUser addrB

--
addrC :: Address
addrC = unsafeMkAddr "0x02B50e87C577084b9659a625870b4A6e8a8E9238"

userC :: User
userC = mkDummyUser addrC

--
addrD :: Address
addrD = unsafeMkAddr "0x041653a75c0238Fe4382Ed36A36BbD6F71C0f52B"

userD :: User
userD = mkDummyUser addrD

--
addrE :: Address
addrE = unsafeMkAddr "0x07E23d9A3c09AD80aa9acF0bcEcF41cD3B06FdD0"

userE :: User
userE = mkDummyUser addrE

--
mkDummyUser :: Address -> User
mkDummyUser a = { id: 1, username: "A", avatarUrl: "", safeAddress: a }

--

unsafeMkAddr :: String -> Address
unsafeMkAddr str =
  unsafePartial
    ( str
        # fromString
        # decodeJson
        # hush
        # fromJust
    )