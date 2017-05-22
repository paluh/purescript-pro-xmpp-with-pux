module Chat.Stats where

import Chat.Connection as Connection
import Chat.Connection (Action(..))
import Data.DateTime.Locale (LocalDateTime)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, insert, update)
import Debug.Trace (traceAny)
import Pux (FoldP, noEffects)
import Strophe (HTTP)
import Strophe.Xmpp.Stanza (Stanza(..))
import Prelude hiding (id)

type Request =
  { sentAt ∷ LocalDateTime
  , stanza ∷ Stanza
  , response ∷
      Maybe
      { receivedAt ∷ LocalDateTime
      , stanza ∷ Stanza
      }
  }

type State = StrMap Request

foldp ∷ ∀ e. FoldP State Connection.Action (http ∷ HTTP | e)
foldp (StanzaSent (Just {stanza, sentAt})) requests =
  noEffects $ case stanza of
    IqRequest { id } →
      traceAny requests $ \_ → insert id { sentAt, stanza, response: Nothing } requests
    otherwise → requests
foldp (StanzaReceived (Just {stanza, receivedAt})) requests =
  noEffects $ case stanza of
    IqResult { id } →
      traceAny requests $
        \_ → update (\r → Just (r{ response = Just { receivedAt, stanza }})) id requests
    otherise → requests
foldp a requests = noEffects requests

