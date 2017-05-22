module Chat.App where

import Prelude
import Chat.Connection as Connection
import Chat.LoginForm as LoginForm
import Chat.Stats as Stats
import Data.Either as Either
import Data.Lens as Lens
import Strophe as Strophe
import Chat.LoginForm (AuthenticationStatus(..))
import Chat.Utils (UpdateFun(UpdateFun), focus, focusEffModel, onlyEffEffect', runUpdateFun)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.ST (pureST)
import DOM (DOM)
import Data.Foldable (for_, maximumBy)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Pux (FoldP, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML, mapEvent)
import Signal.Channel (CHANNEL)
import Strophe (CONNECTION, HTTP, Status, build, c, iq)
import Text.Smolder.HTML (button, h1, li, p, ul)
import Text.Smolder.Markup (text, (#!))

type State =
  { connection ∷ Connection.State
  , stats ∷ Stats.State
  , loginForm ∷ LoginForm.State
  , log ∷ List Status
  }

data Action
  = LoginFormAction LoginForm.Action
  | ConnectionAction Connection.Action
  | SendPing

loginFormL :: forall a b r. Lens.Lens { "loginForm" :: a | r } { "loginForm" :: b | r } a b
loginFormL = Lens.lens _."loginForm" (_ { "loginForm" = _ })

_LoginFormAction :: Lens.Prism' Action LoginForm.Action
_LoginFormAction = Lens.prism LoginFormAction unwrap
  where
    unwrap (LoginFormAction x) = Either.Right x
    unwrap y = Either.Left y

connectionL :: forall a b r. Lens.Lens { "connection" :: a | r } { "connection" :: b | r } a b
connectionL = Lens.lens _."connection" (_ { "connection" = _ })

_ConnectionAction :: Lens.Prism' Action Connection.Action
_ConnectionAction = Lens.prism ConnectionAction unwrap
  where
    unwrap (ConnectionAction x) = Either.Right x
    unwrap y = Either.Left y

-- update ∷ ∀ eff. Action → State → EffModel State Action (http ∷ HTTP, ajax ∷ AJAX | eff)
update ∷ ∀ eff. FoldP State Action (http ∷ HTTP, ajax ∷ AJAX, connection ∷ CONNECTION, console ∷ CONSOLE, channel ∷ CHANNEL, dom ∷ DOM, now ∷ NOW, err ∷ EXCEPTION | eff)
update a@(LoginFormAction (LoginForm.Login jid password)) state =
  let
    connectionEffModel =
      Connection.update (Connection.Connect jid password) (Lens.view connectionL state)
  in
    focusEffModel connectionL _ConnectionAction connectionEffModel state
update SendPing state@{connection: connectionState@{connection: Just connection}}=
  onlyEffEffect'
    state
    (Connection.send
      connection
      ping
      connectionState.outgoingStanzaChannel)
 where
  ping = pureST (do
    b ← iq (fromFoldable [Tuple "to" "paluh@localhost", Tuple "type" "get"])
    c b "ping" (fromFoldable [Tuple "xmlns" "urn:xmpp:ping"])
    build b)
update a s =
  runUpdateFun (loginFormUpdateFun <> connectionUpdateFun <> updateLogUpdateFun <> statsUpdateFun) a s
 where
  updateLog ∷ ∀ e. FoldP (List Status) Connection.Action e
  updateLog (Connection.StatusChange status) l = noEffects (status : l)
  updateLog _ l = noEffects l

  logL :: forall a b r. Lens.Lens { "log" :: a | r } { "log" :: b | r } a b
  logL = Lens.lens _."log" (_ { "log" = _ })

  statsL :: forall a b r. Lens.Lens { "stats" :: a | r } { "stats" :: b | r } a b
  statsL = Lens.lens _."stats" (_ { "stats" = _ })

  updateLogUpdateFun = UpdateFun (focus logL _ConnectionAction updateLog)
  loginFormUpdateFun = UpdateFun (focus loginFormL _LoginFormAction LoginForm.update)
  connectionUpdateFun = UpdateFun (focus connectionL _ConnectionAction Connection.update)
  statsUpdateFun = UpdateFun (focus statsL _ConnectionAction Stats.foldp)

view ∷ State → HTML Action
view { connection : { status }, loginForm, log, stats } = do
  case status of
    Strophe.Connected →
      h1 $ do
        text "connected"
        button #! onClick (pure (ConnectionAction Connection.Disconnect)) $ text "disconnect"
        button #! onClick (pure (SendPing)) $ text "ping"
    Strophe.Disconnecting →
      h1 $ text "disconnecting..."
    otherwise →
      mapEvent LoginFormAction (LoginForm.view loginForm (authStatus status))
  case maximumBy (\r1 r2 → compare r1.sentAt r2.sentAt) stats of
    Nothing → pure unit
    Just r → do
      p $ text "Latest request"
      p $ text (show r.sentAt)
  case log of
    Nil → pure unit
    otherwise →
      ul $ do
        for_ log $ \s →
          li $ text (show s)
 where
  authStatus Strophe.Attached = Just LoginSucceed
  authStatus Strophe.Authenticating = Just Authenticating
  authStatus Strophe.Authfail = Just (LoginFailed)
  authStatus Strophe.Connected = Just Authenticating
  authStatus Strophe.Connecting = Just (LoginError "connecting")
  authStatus Strophe.Connfail = Just (LoginError "Could not connect")
  authStatus Strophe.Conntimeout = Just (LoginError "Connection timeout")
  authStatus Strophe.Disconnected = Just (LoginError "Disconnected")
  authStatus Strophe.Disconnecting = Just (LoginError "Disconnecting")
  authStatus Strophe.Error = Just (LoginError "Connection problem")
