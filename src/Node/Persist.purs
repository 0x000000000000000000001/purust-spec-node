module Test.Spec.Runner.Node.Persist where

import Prelude

import Data.Array (mapMaybe)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Void (Void)
import Effect.Aff (Aff, catchError)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.Spec.Result as Spec
import Test.Spec.Tree (Path, Tree(..), annotateWithPaths, parentSuiteName)
import Yoga.JSON as JSON

type TestFullName = String

type TestRunResults = Map.Map TestFullName
  { success :: Boolean
  , timestamp :: Timestamp
  }

newtype Timestamp = Timestamp Instant

-- | Persist this run's outcomes, merged over the previous file, using the same
-- | `.spec-results` JSON format as the JavaScript runner:
-- | `{"full test name": {"success": Boolean, "timestamp": "seconds"}}`.
-- | A missing or unreadable file behaves like an empty last run, so
-- | `--only-failures` falls back to running everything.
persistResults :: Array (Tree String Void Spec.Result) -> Aff Unit
persistResults trees = do
  now' <- Timestamp <$> liftEffect now
  let currentRun = Map.unions $ serializeRun now' <$> annotateWithPaths trees
  lastRun <- lastPersistedResults
  FS.writeTextFile UTF8 persistFileName $
    JSON.writeJSON (encodeResults (Map.union currentRun lastRun))
  where
  serializeRun :: Timestamp -> Tree (Tuple String Path) Void Spec.Result -> TestRunResults
  serializeRun timestamp = case _ of
    Node _ cs -> Map.unions $ serializeRun timestamp <$> cs
    Leaf _ Nothing -> Map.empty
    Leaf (name /\ path) (Just result) ->
      Map.singleton
        (joinWith " " $ parentSuiteName path <> [name])
        { timestamp
        , success: case result of
            Spec.Success _ _ -> true
            Spec.Failure _ -> false
        }

lastPersistedResults :: Aff TestRunResults
lastPersistedResults = readFile `catchError` \_ -> pure Map.empty
  where
  readFile = decodeResults <$> FS.readTextFile UTF8 persistFileName

persistFileName :: String
persistFileName = ".spec-results"

type StoredResult = { success :: Boolean, timestamp :: String }

encodeResults :: TestRunResults -> Map.Map String StoredResult
encodeResults = map \result ->
  { success: result.success, timestamp: showTimestamp result.timestamp }

decodeResults :: String -> TestRunResults
decodeResults text = case JSON.readJSON text of
  Right (stored :: Map.Map String StoredResult) ->
    Map.fromFoldable $ mapMaybe decodeEntry $ Map.toUnfoldable stored
  Left _ -> Map.empty
  where
  decodeEntry (Tuple name result) = do
    timestamp <- readTimestamp result.timestamp
    pure $ name /\ { success: result.success, timestamp }

-- | Upstream writes the instant as the decimal string of its milliseconds.
showTimestamp :: Timestamp -> String
showTimestamp (Timestamp t) = show (unwrap (unInstant t))

readTimestamp :: String -> Maybe Timestamp
readTimestamp text = do
  milliseconds <- Number.fromString text
  Timestamp <$> instant (Milliseconds milliseconds)
