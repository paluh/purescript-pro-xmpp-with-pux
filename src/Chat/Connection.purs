module Chat.Connection where

import Prelude
import Signal.Channel as Channel
import Strophe as Strophe
import Chat.Utils (onlyEffEffect', onlyEffect')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import Data.DateTime.Locale (LocalDateTime)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Debug.Trace (traceAnyA)
import Pux (EffModel, noEffects)
import Signal.Channel (CHANNEL, Channel)
import Strophe (CONNECTION, Connection, HTTP, Jid, Password, ServerUrl, StanzaDocument, Status(Disconnected), disconnect)
import Strophe.Xmpp.Stanza (Stanza, fromDocument)

type State =
  { connection ∷ Maybe Connection
  , serverUrl ∷ ServerUrl
  , credentials ∷
      Maybe
        { jid ∷ Jid
        , password ∷ Password
        }
  , incomingStanzaChannel ∷
      Channel
        (Maybe
          { stanza ∷ Stanza
          , receivedAt ∷ LocalDateTime
          })
  , outgoingStanzaChannel ∷
      Channel
        (Maybe
          { stanza ∷ Stanza
          , sentAt ∷ LocalDateTime
          })
  , statusChannel ∷ Channel Status
  , status ∷ Status
  }

data Action
  = StatusChange Status
  | StanzaSent (Maybe {stanza ∷ Stanza, sentAt ∷ LocalDateTime})
  | StanzaReceived (Maybe {stanza ∷ Stanza, receivedAt ∷ LocalDateTime})
  | Connect Jid Password
  -- to satisfy typechecker
  | Disconnect
  | NewConnectionSpawned Connection

-- handling communication
update ∷ ∀ eff. Action → State → EffModel State Action (connection ∷ CONNECTION, dom ∷ DOM, http ∷ HTTP, now ∷ NOW | eff)
update (StanzaReceived (Just {stanza})) state =
  onlyEffect' state (traceAnyA stanza >>= const (traceAnyA "ABOVE IS STANZA"))

-- handling connection
update (StatusChange Disconnected) state =
  noEffects $ state { connection = Nothing, status = Disconnected}
update (StatusChange status) state = noEffects $ state { status = status }
update Disconnect state@{ connection: Just conn } =
  { state: state { connection = Nothing }
  , effects: [liftEff (disconnect conn "on request") >>= const (pure Nothing)]
  }
update (Connect jid password) state@{ connection, incomingStanzaChannel, statusChannel } =
  { state: state { credentials = Just { jid, password }, connection = Nothing }
  , effects: [ spawnConnection ]
  }
 where
  spawnConnection = liftEff $ do
    void $ (flip disconnect "we are going to create new connection") `traverse` connection
    conn ← Strophe.connection state.serverUrl
    Strophe.addHandler
      conn
      (\stanzaDocument → do
        receivedAt ← nowDateTime
        stanza ← fromDocument stanzaDocument
        Channel.send incomingStanzaChannel ({stanza: _, receivedAt} <$> stanza) >>= const (pure true))
    connect conn jid password statusChannel
    pure <<< Just $ NewConnectionSpawned conn
update (NewConnectionSpawned conn) state = noEffects $ state { connection = Just conn }
-- debug log
update a s = onlyEffEffect' s (traceAnyA a >>= const (traceAnyA s))

-- wire ∷ ∀ action eff. App action action State → Prism' action Action → Eff eff Unit
-- setupConnetors


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
