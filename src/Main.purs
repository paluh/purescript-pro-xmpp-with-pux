module Main where

import Chat.App (Action(..), update, view)
import Chat.Connection (Action(..))
import Chat.Stats (Request)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty)
import Network.HTTP.Affjax (AJAX)
import Prelude
import Pux (start)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL, channel, subscribe)
import Strophe (CONNECTION, HTTP, ServerUrl(ServerUrl), Status(Disconnected))

main ∷ ∀ eff. Eff (channel ∷ CHANNEL, ajax ∷ AJAX, channel ∷ CHANNEL, connection ∷ CONNECTION, console ∷ CONSOLE, dom ∷ DOM, err ∷ EXCEPTION, exception ∷ EXCEPTION, http ∷ HTTP, now ∷ NOW | eff) Unit
main = do
  statusChannel ← channel Disconnected
  incomingStanzaChannel ← channel Nothing
  outgoingStanzaChannel ← channel Nothing

  let
    state =
      { connection:
          { connection: Nothing
          , credentials: Nothing
          , incomingStanzaChannel: incomingStanzaChannel
          , outgoingStanzaChannel: outgoingStanzaChannel
          , serverUrl: ServerUrl "http://localhost:5280/http-bind/"
          , statusChannel: statusChannel
          , status: Disconnected
          }
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
    , inputs:
      [ (ConnectionAction <<< StatusChange) <$> subscribe statusChannel
       -- stanza parsing
      , (ConnectionAction <<< StanzaSent) <$> subscribe outgoingStanzaChannel
      , (ConnectionAction <<< StanzaReceived) <$> subscribe incomingStanzaChannel
      ]
    }
  renderToDOM "#app" app.markup app.input



-- module Test.Main where
--
-- import Prelude
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, log)
-- import Control.Monad.ST (runST)
-- import Data.StrMap (empty)
-- import Strophe (StanzaDOM, c, iq, build, t, toString, up)
--
-- stanzas ∷ ∀ eff. Eff eff {s1 ∷ StanzaDOM, s2 ∷ StanzaDOM}
-- stanzas = runST (do
--   b ← iq empty
--   c b "bar" empty
--   s1 ← build b
--   c b "baz" empty
--   t b "TEST TET"
--   up b
--   up b
--   c b "foo" empty
--   s2 ← build b
--   pure {s1, s2})
--
-- -- stanza ∷ ∀ eff. Eff eff Stanza
-- -- stanza = runST (do
-- --   b ← iq empty
-- --   c b "bar" empty
-- --   c b "baz" empty
-- --   runBuilder b)
--
-- main ∷ forall e. Eff (console ∷ CONSOLE | e) Unit
-- main = do
--   {s1, s2} ← stanzas
--   log (toString s1)
--   log (toString s2)
--   log "You should add some tests!!"
