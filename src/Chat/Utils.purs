module Chat.Utils where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Lens (Lens', Prism', preview, review, set, view)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Pux (EffModel, FoldP, CoreEffects, noEffects, onlyEffects)

focus
  ∷ ∀ eff state2 state1 action1 action2
  . Lens' state2 state1
  → Prism' action2 action1
  → FoldP state1 action1 eff
  → FoldP state2 action2 eff
focus lens prism update =
  update'
 where
  update' a2 s2 =
    case preview prism a2 of
      Just a1 →
        let effModel = (update a1 (view lens s2))
        in focusEffModel lens prism effModel s2
      Nothing → noEffects s2

focusEffModel
  ∷ ∀ eff state2 state1 action1 action2
  . Lens' state2 state1
  → Prism' action2 action1
  → EffModel state1 action1 eff
  → state2
  → EffModel state2 action2 eff
focusEffModel lens prism { state: s1, effects: e1 } s2 =
  let
    state = set lens s1 s2
    -- map over list of effects which result in `Maybe Action`
    effects = ((review prism <$> _) <$> _) <$> e1
  in { state, effects }


newtype UpdateFun a s e = UpdateFun (FoldP a s e)

runUpdateFun ∷ ∀ a s e. UpdateFun a s e → FoldP a s e
runUpdateFun (UpdateFun update) = update

instance updateFunSemigroup ∷ Semigroup (UpdateFun a s e) where
  append (UpdateFun u) (UpdateFun v) =
    UpdateFun update
   where
    update action state =
      case u action state of
        {state: s, effects: e} →
          case v action s of
            { state: s', effects: e' } → { state: s', effects: e <> e' }

instance updateFunMonoid ∷ Monoid (UpdateFun a s e) where
  mempty = UpdateFun (\_ s → noEffects s)

onlyEffect ∷ ∀ action eff state. state → (Aff (CoreEffects eff) (Maybe action)) → EffModel state action eff
onlyEffect state effect =
  onlyEffects state [effect]

onlyEffect' ∷ ∀ a action eff state. state → (Aff (CoreEffects eff) a) → EffModel state action eff
onlyEffect' state effect =
  onlyEffect state (effect >>= const (pure Nothing))

onlyEffEffect ∷ ∀ action eff state. state → (Eff (CoreEffects eff) (Maybe action)) → EffModel state action eff
onlyEffEffect state effect =
  onlyEffects state [liftEff effect]

onlyEffEffect' ∷ ∀ a action eff state. state → (Eff (CoreEffects eff) a) → EffModel state action eff
onlyEffEffect' state effect =
  onlyEffEffect state (effect >>= const (pure Nothing))

-- -- | A variant of `focus` which only changes the state type, by applying a `Lens`.
-- focusState
--   :: forall eff props state2 state1 action
--    . Lens' state2 state1
--   -> Spec eff state1 props action
--   -> Spec eff state2 props action
-- focusState lens = focus lens id
-- 
-- -- | A variant of `focus` which only changes the action type, by applying a `Prism`,
-- -- | effectively matching some subset of a larger action type.
-- match
--   :: forall eff props state action1 action2
--    . Prism' action2 action1
--   -> Spec eff state props action1
--   -> Spec eff state props action2
-- match prism = focus id prism
