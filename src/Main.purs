module Main where

import Prelude
import Signal.Channel as Channel
import Strophe as Strophe
import Chat.App (Action(..), update, view)
import Chat.Connection (Action(..))
import Chat.Stats (Request)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import Data.Foldable (sequence_)
import Data.List (List(..), singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.StrMap (StrMap, empty)
import Debug.Trace (traceAnyA)
import Network.HTTP.Affjax (AJAX)
import Pux (start)
import Pux.Renderer.React (renderToDOM)
import Signal (runSignal, (~>))
import Signal.Channel (CHANNEL, channel, subscribe)
import Strophe (CONNECTION, HTTP, ServerUrl(ServerUrl), Status(Disconnected), connect)
import Strophe.Xmpp.Stanza (fromDocument)

main ∷ ∀ eff. Eff (channel ∷ CHANNEL, ajax ∷ AJAX, channel ∷ CHANNEL, connection ∷ CONNECTION, console ∷ CONSOLE, dom ∷ DOM, exception ∷ EXCEPTION, exception ∷ EXCEPTION, http ∷ HTTP, now ∷ NOW | eff) Unit
main = do
  statusChannel ← channel Disconnected
  incomingStanzaChannel ← channel Nothing
  outgoingStanzaChannel ← channel Nothing

  let
    state =
      { connection: Nothing
      , serverUrl: ServerUrl "http://localhost:5280/http-bind/"
      , stats: (empty ∷ StrMap Request)
      , loginForm:
          { jid: ""
          , password: ""
          }
      , log: Nil
      }
  app <- start
    { initialState: state
    , foldp: update
    , view: view
    , inputs: []
    }

  let
    send' action = Channel.send app.input (singleton action)
    connectionHandler ev = case ev of
      (ConnectionAction (Connect serverUrl credentials)) → do
        send' <<< ConnectionAction $ Disconnect
        conn ← Strophe.connection serverUrl
        let
          onStatusChange status = send' <<< ConnectionAction <<< StatusChange $ status
          onIncommingStanza stanzaDocument = do
            traceAnyA stanzaDocument
            receivedAt ← nowDateTime
            stanza ← fromDocument stanzaDocument
            traceAnyA stanza
            send' <<< ConnectionAction <<< StanzaReceived $ {stanza: _, receivedAt} <$> stanza
            log "STANZA RECEIVED"
            pure true
        handerRef ← Strophe.addHandler conn onIncommingStanza
        send' <<< ConnectionAction $ NewConnectionSpawned serverUrl credentials conn handerRef
        connect conn credentials.jid credentials.password onStatusChange
      _ → pure unit

  runSignal $ app.events ~> sequence_ <<< map connectionHandler

  renderToDOM "#app" app.markup app.input
