module Chat.Connection where

import Prelude
import Signal.Channel as Channel
import Strophe as Strophe
import Strophe as Strophe
import Chat.Utils (onlyEffEffect', onlyEffect')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import Data.DateTime.Locale (LocalDateTime)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceAnyA)
import Pux (EffModel, noEffects)
import Signal.Channel (CHANNEL, Channel)
import Strophe (CONNECTION, Connection, HTTP, Jid, Password, ServerUrl, StanzaDocument, StanzaHandlerRef, Status(..), deleteHandler)
import Strophe.Xmpp.Stanza (Stanza(..), fromDocument)

type Credentials =
  { jid ∷ Jid
  , password ∷ Password
  }

type State' =
  { connection ∷ Connection
  , credentials ∷ Credentials
  , serverUrl ∷ ServerUrl
  , stanzaHandlerRef ∷ StanzaHandlerRef
  , status ∷ Status
  }

type State = Maybe State'

data Action
  = Connect ServerUrl Credentials
  | Disconnect
  | NewConnectionSpawned ServerUrl Credentials Connection StanzaHandlerRef
  | SendStanza Stanza
  | StanzaReceived (Maybe {stanza ∷ Stanza, receivedAt ∷ LocalDateTime})
  | StanzaSent (Maybe {stanza ∷ Stanza, sentAt ∷ LocalDateTime})
  | StatusChange Status

disconnect :: ∀ eff. Connection → StanzaHandlerRef → String → Eff ( connection ∷ CONNECTION , http ∷ HTTP | eff ) Unit
disconnect conn stanzaHandlerRef msg = do
  deleteHandler conn stanzaHandlerRef
  Strophe.disconnect conn msg

update ∷ ∀ eff. Action → State → EffModel State Action (connection ∷ CONNECTION, dom ∷ DOM, http ∷ HTTP, now ∷ NOW | eff)
update (Connect _ _) state = noEffects state
update Disconnect state =
  case state of
    Just s →
      onlyEffEffect' Nothing (disconnect s.connection s.stanzaHandlerRef "on request")
    otherwise → noEffects Nothing
update (NewConnectionSpawned serverUrl credentials connection stanzaHandlerRef) state =
  let newState = Just {connection, credentials, serverUrl, stanzaHandlerRef, status: Disconnected}
  in case state of
    Nothing → noEffects newState
    Just s →
      onlyEffEffect' newState (disconnect s.connection s.stanzaHandlerRef "on request")
update (SendStanza stanza) state@(Just { connection, status: Connected }) =
  case stanza of
    OtherStanza stanzaDocument →
      let
        s = do
          stanzaDocument' ← Strophe.send' connection stanzaDocument
          sentAt ← nowDateTime
          stanzaInfo ← ((\stanza → {stanza, sentAt}) <$> _) <$> fromDocument stanzaDocument'
          pure (Just <<< StanzaSent $ stanzaInfo)
      in onlyEffEffect' state s
    otherwise → noEffects state
update (SendStanza _) state =
  noEffects state
update (StanzaReceived s) state =
  case s of
    Just { stanza } → onlyEffect' state (traceAnyA stanza >>= const (traceAnyA "ABOVE IS STANZA"))
    Nothing → noEffects state
update (StanzaSent _) state =
  noEffects state
update (StatusChange status) state =
  case state of
    Just s → noEffects $ Just (s { status = status })
    otherwise → noEffects state

connect ∷ ∀ eff. Connection → Jid → Password → Channel Status → Eff ( http ∷ HTTP , channel ∷ CHANNEL, connection ∷ CONNECTION | eff ) Unit
connect connection jid password statusChannel = do
  Strophe.connect connection jid password onStatusChange
 where
  onStatusChange status = Channel.send statusChannel status

send ∷ ∀ eff. Connection → StanzaDocument → Channel (Maybe {stanza ∷ Stanza, sentAt ∷ LocalDateTime}) → Eff (http ∷ HTTP, connection ∷ CONNECTION, channel ∷ CHANNEL, dom ∷ DOM, now ∷ NOW | eff) Strophe.StanzaDocument
send connection stanzaDocument outgoingStanzaChannel = do
  stanzaDocument' ← Strophe.send' connection stanzaDocument
  sentAt ← nowDateTime
  stanzaInfo ← ((\stanza → {stanza, sentAt}) <$> _) <$> fromDocument stanzaDocument'
  Channel.send outgoingStanzaChannel stanzaInfo
  pure stanzaDocument'
