module CirclesPink.Garden.StateMachine.Action
  ( CirclesAction
  , _addTrustConnection
  , _askEmail
  , _askUsername
  , _checkForSession
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
  , _infoGeneral
  , _infoSecurity
  , _landing
  , _login
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
  , _signMessage
  , _signUp
  , _submit
  , _transfer
  , _trusts
  , _userSearch
  ) where

import Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.UserIdent (UserIdent)
import Data.BN (BN)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type CirclesAction = Variant
  ( infoGeneral ::
      Variant
        ( next :: Unit
        )
  , askUsername ::
      Variant
        ( prev :: Unit
        , setUsername :: String
        , next :: Unit
        )
  , askEmail ::
      Variant
        ( prev :: Unit
        , setEmail :: String
        , setTerms :: Unit
        , setPrivacy :: Unit
        , next :: Unit
        )
  , infoSecurity ::
      Variant
        ( prev :: Unit
        , next :: Unit
        )
  , magicWords ::
      Variant
        ( prev :: Unit
        , newPrivKey :: Unit
        , next :: Unit
        )
  , submit ::
      Variant
        ( prev :: Unit
        , submit :: Unit
        )
  , dashboard ::
      Variant
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
        , signMessage :: String
        )
  , login ::
      Variant
        ( login :: Unit
        , signUp :: Unit
        , setMagicWords :: String
        )
  , trusts ::
      Variant
        ( getSafeStatus :: Unit
        , finalizeRegisterUser :: Unit
        )
  , debug ::
      Variant
        ( coreToWindow :: Unit
        , setMagicWords :: String
        )
  , landing ::
      Variant
        ( signUp :: Unit
        , signIn :: Unit
        , checkForSession :: Unit
        )
  )

----
_infoGeneral :: forall a v. a -> Variant (infoGeneral :: a | v)
_infoGeneral = inj (Proxy :: _ "infoGeneral")

_askUsername :: forall a v. a -> Variant (askUsername :: a | v)
_askUsername = inj (Proxy :: _ "askUsername")

_askEmail :: forall a v. a -> Variant (askEmail :: a | v)
_askEmail = inj (Proxy :: _ "askEmail")

_infoSecurity :: forall a v. a -> Variant (infoSecurity :: a | v)
_infoSecurity = inj (Proxy :: _ "infoSecurity")

_magicWords :: forall a v. a -> Variant (magicWords :: a | v)
_magicWords = inj (Proxy :: _ "magicWords")

_submit :: forall a v. a -> Variant (submit :: a | v)
_submit = inj (Proxy :: _ "submit")

_dashboard :: forall a v. a -> Variant (dashboard :: a | v)
_dashboard = inj (Proxy :: _ "dashboard")

_landing :: forall a v. a -> Variant (landing :: a | v)
_landing = inj (Proxy :: _ "landing")

_trusts :: forall a v. a -> Variant (trusts :: a | v)
_trusts = inj (Proxy :: _ "trusts")

_login :: forall a v. a -> Variant (login :: a | v)
_login = inj (Proxy :: _ "login")

_debug :: forall a v. a -> Variant (debug :: a | v)
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

_signMessage :: forall a v. a -> Variant (signMessage :: a | v)
_signMessage = inj (Proxy :: _ "signMessage")