module VoucherServer.Spec.Types where

import CirclesPink.Prelude

import Data.BN (BN)
import Data.DateTime.Instant as DT
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, un)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import PursTsGen.Class.ToPursNominal (PursNominal(..))
import PursTsGen.Lang.PureScript.Type as PS
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)

moduleName :: String
moduleName = "VoucherServer.Spec.Types"

--------------------------------------------------------------------------------
newtype Instant = Instant DT.Instant

derive instance newtypeInstant :: Newtype Instant _

instance writeForeignInstant :: WriteForeign Instant where
  writeImpl = un Instant >>> DT.unInstant >>> un Milliseconds >>> writeImpl

--------------------------------------------------------------------------------

newtype VoucherProvider = VoucherProvider
  { id :: VoucherProviderId
  , name :: String
  , logoUrl :: String
  , shopUrl :: String
  , availableOffers :: Array VoucherOffer
  }

derive newtype instance WriteForeign VoucherProvider
derive newtype instance ReadForeign VoucherProvider

-- instance ToPursNominal VoucherProvider where
--   toPursNominal _ = PursNominal moduleName "VoucherProvider"

instance ToTsType VoucherProvider where
  toTsType (VoucherProvider r) = toTsType r

instance ToPursType VoucherProvider where
  toPursType _ = PS.var $ PS.Name "TODO"

--------------------------------------------------------------------------------

newtype VoucherOffer = VoucherOffer
  { amount :: VoucherAmount
  , countAvailable :: Int
  }

derive newtype instance WriteForeign VoucherOffer
derive newtype instance ReadForeign VoucherOffer

instance ToTsType VoucherOffer where
  toTsType (VoucherOffer r) = toTsType r

instance ToPursType VoucherOffer where
  toPursType _ = PS.var $ PS.Name "TODO"

--------------------------------------------------------------------------------

newtype VoucherProviderId = VoucherProviderId String

derive instance Generic VoucherProviderId _
derive newtype instance WriteForeign VoucherProviderId
derive newtype instance ReadForeign VoucherProviderId
derive newtype instance Eq VoucherProviderId
derive newtype instance Ord VoucherProviderId

unVoucherProviderId :: VoucherProviderId -> String
unVoucherProviderId (VoucherProviderId x) = x

instance Show VoucherProviderId where
  show = genericShow

instance ToPursNominal VoucherProviderId where
  toPursNominal _ = PursNominal moduleName "VoucherProviderId"

instance ToTsType VoucherProviderId where
  toTsType = defaultToTsType' []

instance ToTsDef VoucherProviderId where
  toTsDef = defaultToTsDef' []

instance ToPursType VoucherProviderId where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

newtype EurCent = EurCent Int

derive instance Generic EurCent _
derive newtype instance WriteForeign EurCent
derive newtype instance ReadForeign EurCent
-- derive newtype instance semiringEurCent :: Semiring EurCent
-- derive newtype instance eqEurCent :: Eq EurCent
-- derive newtype instance ordEurCent :: Ord EurCent

instance Show EurCent where
  show = genericShow

instance ToPursNominal EurCent where
  toPursNominal _ = PursNominal moduleName "EurCent"

instance ToTsType EurCent where
  toTsType = defaultToTsType' []

instance ToTsDef EurCent where
  toTsDef = defaultToTsDef' []

instance ToPursType EurCent where
  toPursType = defaultToPursType' []

unEurCent :: EurCent -> Int
unEurCent (EurCent x) = x

--------------------------------------------------------------------------------
newtype VoucherAmount = VoucherAmount EurCent

derive instance Generic VoucherAmount _
derive newtype instance WriteForeign VoucherAmount
derive newtype instance ReadForeign VoucherAmount

instance Show VoucherAmount where
  show = genericShow

instance ToPursNominal VoucherAmount where
  toPursNominal _ = PursNominal moduleName "VoucherAmount"

instance ToTsType VoucherAmount where
  toTsType = defaultToTsType' []

instance ToTsDef VoucherAmount where
  toTsDef = defaultToTsDef' []

instance ToPursType VoucherAmount where
  toPursType = defaultToPursType' []

unVoucherAmount :: VoucherAmount -> EurCent
unVoucherAmount (VoucherAmount x) = x

--------------------------------------------------------------------------------

newtype Freckles = Freckles BN

--------------------------------------------------------------------------------
newtype VoucherCodeEncrypted = VoucherCodeEncrypted String

derive instance genericVoucherCodeEncrypted :: Generic VoucherCodeEncrypted _
derive newtype instance readForeignVoucherCodeEncrypted :: ReadForeign VoucherCodeEncrypted

instance showVoucherCodeEncrypted :: Show VoucherCodeEncrypted where
  show = genericShow

--------------------------------------------------------------------------------

newtype VoucherCode = VoucherCode String

derive newtype instance readForeignVoucherCode :: ReadForeign VoucherCode
derive newtype instance writeForeignVoucherCode :: WriteForeign VoucherCode

instance ToPursNominal VoucherCode where
  toPursNominal _ = PursNominal moduleName "VoucherCode"

instance ToTsType VoucherCode where
  toTsType = defaultToTsType' []

instance ToTsDef VoucherCode where
  toTsDef = defaultToTsDef' []

instance ToPursType VoucherCode where
  toPursType = defaultToPursType' []

unVoucherCode :: VoucherCode -> String
unVoucherCode (VoucherCode x) = x

--------------------------------------------------------------------------------

newtype VoucherEncrypted =
  VoucherEncrypted
    { providerId :: VoucherProviderId
    , amount :: VoucherAmount
    , code :: VoucherCodeEncrypted
    , sold ::
        { transactionId :: TransferId
        , safeAddress :: String
        , timestamp :: String
        }
    }

derive newtype instance readForeignVoucherEncrypted :: ReadForeign VoucherEncrypted
derive instance genericVoucherEncrypted :: Generic VoucherEncrypted _

instance showVoucherEncrypted :: Show VoucherEncrypted where
  show = genericShow

--------------------------------------------------------------------------------

newtype Voucher =
  Voucher
    { providerId :: VoucherProviderId
    , amount :: VoucherAmount
    , code :: VoucherCode
    , sold ::
        { transactionId :: TransferId
        , safeAddress :: String
        , timestamp :: String
        }
    }

derive newtype instance readForeignVoucher :: ReadForeign Voucher
derive newtype instance writeForeignVoucher :: WriteForeign Voucher

instance ToTsType Voucher where
  toTsType (Voucher r) = toTsType r

instance ToPursType Voucher where
  toPursType _ = PS.var $ PS.Name "TODO"

--------------------------------------------------------------------------------

newtype TransferId = TransferId String

derive instance genericTransferId :: Generic TransferId _
derive instance newtypeTransferId :: Newtype TransferId _
derive newtype instance readForeignTransferId :: ReadForeign TransferId
derive newtype instance writeForeignTransferId :: WriteForeign TransferId
derive newtype instance eqTransferId :: Eq TransferId
derive newtype instance ordTransferId :: Ord TransferId

instance showTransferId :: Show TransferId where
  show = genericShow

instance ToPursNominal TransferId where
  toPursNominal _ = PursNominal moduleName "TransferId"

instance ToTsType TransferId where
  toTsType = defaultToTsType' []

instance ToTsDef TransferId where
  toTsDef = defaultToTsDef' []

instance ToPursType TransferId where
  toPursType = defaultToPursType' []
