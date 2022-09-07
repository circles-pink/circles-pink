module CirclesPink.Garden.StateMachine.Action
  ( AskEmailAction
  , AskUsernameAction
  , CirclesAction
  , DashboardAction
  , DebugAction
  , InfoSecurityAction
  , LandingAction
  , LoginAction
  , MagicWordsAction
  , SubmitAction
  , TrustsAction
  , _addTrustConnection
  , _askEmail
  , _askEmailAction
  , _infoSecurityAction
  , _askUsername
  , _askUsernameAction
  , _checkForSession
  , _circlesAction
  , _coreToWindow
  , _dashboard
  , _debug
  , _expandTrustNetwork
  , _finalizeRegisterUser
  , _getBalance
  , _getSafeStatus
  , _getTrusts
  , _getUBIPayout
  , _getUsers
  , _getVoucherProviders
  , _getVouchers
  , _infoSecurity
  , _landing
  , _landingAction
  , _login
  , _login'
  , _loginAction
  , _magicWords
  , _newPrivKey
  , _next
  , _prev
  , _redeploySafeAndToken
  , _removeTrustConnection
  , _setEmail
  , _setMagicWords
  , _setPrivacy
  , _setTerms
  , _setUsername
  , _signIn
  , _signUp
  , _submit
  , _submit'
  , _transfer
  , _trusts
  , _userSearch
  )
  where

import Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.UserIdent (UserIdent)
import Data.BN (BN)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type CirclesAction = Variant
  ( askUsername :: AskUsernameAction
  , askEmail :: AskEmailAction
  , infoSecurity :: InfoSecurityAction
  , magicWords :: MagicWordsAction
  , submit :: SubmitAction
  , dashboard :: DashboardAction
  , login :: LoginAction
  , trusts :: TrustsAction
  , debug :: DebugAction
  , landing :: LandingAction
  )

----

type AskUsernameAction = Variant
  ( setUsername :: String
  , next :: Unit
  )

type AskEmailAction = Variant
  ( prev :: Unit
  , setEmail :: String
  , setTerms :: Unit
  , setPrivacy :: Unit
  , next :: Unit
  )

type InfoSecurityAction = Variant
  ( prev :: Unit
  , next :: Unit
  )

type MagicWordsAction = Variant
  ( prev :: Unit
  , newPrivKey :: Unit
  , next :: Unit
  )

type SubmitAction = Variant
  ( prev :: Unit
  , submit :: Unit
  )

type DashboardAction = Variant
  ( logout :: Unit
  , getTrusts :: Unit
  , addTrustConnection :: UserIdent
  , removeTrustConnection :: UserIdent
  , getBalance :: Unit
  , getUBIPayout :: Unit
  , transfer ::
      { from :: Address
      , to :: Address
      , value :: BN
      , paymentNote :: String
      }
  , getUsers ::
      { userNames :: Array String
      , addresses :: Array Address
      }
  , userSearch ::
      { query :: String
      }
  , redeploySafeAndToken :: Unit
  , expandTrustNetwork :: String
  , getVouchers :: String
  , getVoucherProviders :: Unit
  )

type LoginAction = Variant
  ( login :: Unit
  , signUp :: Unit
  , setMagicWords :: String
  )

type TrustsAction = Variant
  ( getSafeStatus :: Unit
  , finalizeRegisterUser :: Unit
  )

type DebugAction = Variant
  ( coreToWindow :: Unit
  , setMagicWords :: String
  )

type LandingAction = Variant
  ( signUp :: Unit
  , signIn :: Unit
  , checkForSession :: Unit
  )

----

_circlesAction
  :: { _askUsername :: AskUsernameAction -> CirclesAction
     , _askEmail :: AskEmailAction -> CirclesAction
     , _infoSecurity :: InfoSecurityAction -> CirclesAction
     , _landing :: LandingAction -> CirclesAction
     , _login :: LoginAction -> CirclesAction
     }
_circlesAction =
  { _askUsername
  , _askEmail
  , _infoSecurity
  , _landing
  , _login
  }

_landingAction
  :: { _checkForSession :: Unit -> LandingAction
     , _signIn :: Unit -> LandingAction
     , _signUp :: Unit -> LandingAction
     }
_landingAction =
  { _checkForSession: inj (Proxy :: _ "checkForSession")
  , _signIn: inj (Proxy :: _ "signIn")
  , _signUp: inj (Proxy :: _ "signUp")
  }

_loginAction
  :: { _setMagicWords :: String -> LoginAction
     , _login :: Unit -> LoginAction
     , _signUp :: Unit -> LoginAction
     }
_loginAction =
  { _setMagicWords: inj (Proxy :: _ "setMagicWords")
  , _login: inj (Proxy :: _ "login")
  , _signUp: inj (Proxy :: _ "signUp")
  }

_askUsernameAction
  :: { _setUsername :: String -> AskUsernameAction
     , _next :: Unit -> AskUsernameAction
     }
_askUsernameAction =
  { _setUsername: inj (Proxy :: _ "setUsername")
  , _next: inj (Proxy :: _ "next")
  }

_askEmailAction
  :: { _prev :: Unit -> AskEmailAction
     , _setEmail :: String -> AskEmailAction
     , _setTerms :: Unit -> AskEmailAction
     , _setPrivacy :: Unit -> AskEmailAction
     , _next :: Unit -> AskEmailAction
     }
_askEmailAction =
  { _prev: inj (Proxy :: _ "prev")
  , _setEmail: inj (Proxy :: _ "setEmail")
  , _setTerms: inj (Proxy :: _ "setTerms")
  , _setPrivacy: inj (Proxy :: _ "setPrivacy")
  , _next: inj (Proxy :: _ "next")
  }

_infoSecurityAction
  :: { _prev :: Unit -> InfoSecurityAction
     , _next :: Unit -> InfoSecurityAction
     }
_infoSecurityAction =
  { _prev: inj (Proxy :: _ "prev")
  , _next: inj (Proxy :: _ "next")
  }


_askUsername :: AskUsernameAction -> CirclesAction
_askUsername = inj (Proxy :: _ "askUsername")

_askEmail :: AskEmailAction -> CirclesAction
_askEmail = inj (Proxy :: _ "askEmail")

_infoSecurity :: InfoSecurityAction -> CirclesAction
_infoSecurity = inj (Proxy :: _ "infoSecurity")

_magicWords :: MagicWordsAction -> CirclesAction
_magicWords = inj (Proxy :: _ "magicWords")

_submit :: SubmitAction -> CirclesAction
_submit = inj (Proxy :: _ "submit")

_submit' :: forall a v. a -> Variant (submit :: a | v)
_submit' = inj (Proxy :: _ "submit")

_dashboard :: DashboardAction -> CirclesAction
_dashboard = inj (Proxy :: _ "dashboard")

_login :: LoginAction -> CirclesAction
_login = inj (Proxy :: _ "login")

_login' :: forall a v. a -> Variant (login :: a | v)
_login' = inj (Proxy :: _ "login")

_trusts :: TrustsAction -> CirclesAction
_trusts = inj (Proxy :: _ "trusts")

_landing :: LandingAction -> CirclesAction
_landing = inj (Proxy :: _ "landing")

_debug :: DebugAction -> CirclesAction
_debug = inj (Proxy :: _ "debug")

----
_next :: forall a v. a -> Variant (next :: a | v)
_next = inj (Proxy :: _ "next")

_prev :: forall a v. a -> Variant (prev :: a | v)
_prev = inj (Proxy :: _ "prev")

_setUsername :: forall a v. a -> Variant (setUsername :: a | v)
_setUsername = inj (Proxy :: _ "setUsername")

_setEmail :: forall a v. a -> Variant (setEmail :: a | v)
_setEmail = inj (Proxy :: _ "setEmail")

_setTerms :: forall a v. a -> Variant (setTerms :: a | v)
_setTerms = inj (Proxy :: _ "setTerms")

_setPrivacy :: forall a v. a -> Variant (setPrivacy :: a | v)
_setPrivacy = inj (Proxy :: _ "setPrivacy")

_newPrivKey :: forall a v. a -> Variant (newPrivKey :: a | v)
_newPrivKey = inj (Proxy :: _ "newPrivKey")

_signIn :: forall a v. a -> Variant (signIn :: a | v)
_signIn = inj (Proxy :: _ "signIn")

_signUp :: forall a v. a -> Variant (signUp :: a | v)
_signUp = inj (Proxy :: _ "signUp")

_setMagicWords :: forall a v. a -> Variant (setMagicWords :: a | v)
_setMagicWords = inj (Proxy :: _ "setMagicWords")

_coreToWindow :: forall a v. a -> Variant (coreToWindow :: a | v)
_coreToWindow = inj (Proxy :: _ "coreToWindow")

_getTrusts :: forall a v. a -> Variant (getTrusts :: a | v)
_getTrusts = inj (Proxy :: _ "getTrusts")

_getSafeStatus :: forall a v. a -> Variant (getSafeStatus :: a | v)
_getSafeStatus = inj (Proxy :: _ "getSafeStatus")

_finalizeRegisterUser :: forall a v. a -> Variant (finalizeRegisterUser :: a | v)
_finalizeRegisterUser = inj (Proxy :: _ "finalizeRegisterUser")

_addTrustConnection :: forall a v. a -> Variant (addTrustConnection :: a | v)
_addTrustConnection = inj (Proxy :: _ "addTrustConnection")

_removeTrustConnection :: forall a v. a -> Variant (removeTrustConnection :: a | v)
_removeTrustConnection = inj (Proxy :: _ "removeTrustConnection")

_checkForSession :: forall a v. a -> Variant (checkForSession :: a | v)
_checkForSession = inj (Proxy :: _ "checkForSession")

_getBalance :: forall a v. a -> Variant (getBalance :: a | v)
_getBalance = inj (Proxy :: _ "getBalance")

_getUBIPayout :: forall a v. a -> Variant (getUBIPayout :: a | v)
_getUBIPayout = inj (Proxy :: _ "getUBIPayout")

_getUsers :: forall a v. a -> Variant (getUsers :: a | v)
_getUsers = inj (Proxy :: _ "getUsers")

_transfer :: forall a v. a -> Variant (transfer :: a | v)
_transfer = inj (Proxy :: _ "transfer")

_userSearch :: forall a v. a -> Variant (userSearch :: a | v)
_userSearch = inj (Proxy :: _ "userSearch")

_redeploySafeAndToken :: forall a v. a -> Variant (redeploySafeAndToken :: a | v)
_redeploySafeAndToken = inj (Proxy :: _ "redeploySafeAndToken")

_expandTrustNetwork :: forall a v. a -> Variant (expandTrustNetwork :: a | v)
_expandTrustNetwork = inj (Proxy :: _ "expandTrustNetwork")

_getVouchers :: forall a v. a -> Variant (getVouchers :: a | v)
_getVouchers = inj (Proxy :: _ "getVouchers")

_getVoucherProviders :: forall a v. a -> Variant (getVoucherProviders :: a | v)
_getVoucherProviders = inj (Proxy :: _ "getVoucherProviders")