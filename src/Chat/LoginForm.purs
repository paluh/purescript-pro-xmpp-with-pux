module Chat.LoginForm where

-- import Pux.Html.Bootstrap.Button as Button
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onChange, onSubmit, targetValue)
import Pux.DOM.HTML (HTML)
import Strophe (Jid(..), Password(..))
import Text.Smolder.HTML (button, div, form, input)
import Text.Smolder.HTML.Attributes (className, name, type', value)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)

data AuthenticationStatus
  = Authenticating
  | Connecting
  | LoginError String
  | LoginFailed
  | LoginSucceed

type State =
  { jid ∷ String
  , password ∷ String
  }

data Action
  = SignIn Event
  | JidInputChanged Event
  | PasswordInputChanged Event
  | Login Jid Password

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console ∷ CONSOLE, dom ∷ DOM | e)
update (JidInputChanged ev) s = noEffects $ s { jid = targetValue ev }
update (PasswordInputChanged ev) s = noEffects $ s { password = targetValue ev }
update (SignIn e) s@({ jid, password }) =
  { state : s
  , effects : [ (liftEff $ preventDefault e) >>= (const $ pure Nothing), (const $ pure Nothing) =<< log "SUBMIT", pure <<< Just $ Login (Jid jid) (Password password) ]
  }
update (Login _ _) s = noEffects s

view :: State → Maybe AuthenticationStatus → HTML Action
view (s@{ jid, password }) loginStatus =
  -- XXX: Bring back error reporting
  let
    err = (errorMessage loginStatus)
    children = do
      input ! type' "jid" ! value s.jid #! onChange JidInputChanged
      input ! type' "password" ! value s.password #! onChange PasswordInputChanged
      button ! type' "submit" $ text "submit"
    f = form ! name "signin" #! onSubmit SignIn
  in form' f err children
 where
  form' f (Just m) children =
    f ! className "error" $ do
      div ! className "error-message" $ text m
      children
  form' f Nothing children =
    f children

  -- b active title =
  --   Button.render $
  --     Button.Button
  --       { active
  --       , attributes: []
  --       , block: false
  --       , buttonType: Button.Primary
  --       , outline: false
  --       , size: Button.Small
  --       , title
  --       }

  errorMessage :: Maybe AuthenticationStatus -> Maybe String
  errorMessage Nothing = pure "Nothing?"
  errorMessage maybeLoginStatus = do
    ls <- maybeLoginStatus
    case ls of
      Authenticating → pure "Authenticating..."
      Connecting → pure "Connecting..."
      LoginError m → pure ("Loging process error: " <> m)
      LoginFailed → pure "Incorrect email or password"
      LoginSucceed → pure "Authenticated"

