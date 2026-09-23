module Test.Spec.Runner.Node.Persist where

import Prelude

import Data.Map as Map
import Effect.Aff (Aff)
import Test.Spec.Result as Spec
import Test.Spec.Tree (Tree)

-- | The JavaScript runner persists each run to `.spec-results` as JSON so that
-- | later `--only-failures`/`--next-failure` runs can reuse the previous
-- | outcome. The native runner has no JSON codec available yet, so persistence
-- | is a documented no-op: a run always executes the full spec tree, and
-- | `--only-failures` filters nothing.
type TestFullName = String

type TestRunResults = Map.Map TestFullName
  { success :: Boolean
  }

persistResults :: Array (Tree String Void Spec.Result) -> Aff Unit
persistResults _ = pure unit

lastPersistedResults :: Aff TestRunResults
lastPersistedResults = pure Map.empty
