module Test.Main where

import Prelude

import Data.Array as Array
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.String.CodeUnits as CU
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)

import Effect.Exception (throw)
import Node.Buffer as Buffer
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.Errors.SystemError as SysErr
import Node.FS.Aff as FS
import Node.Process (lookupEnv)
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config as Config

-- | The CLI scenarios run the compiled fixture binaries as child processes,
-- | exactly like the upstream suite ran them through `spago test`. The
-- | binaries are built by `bin/test` and passed through the environment.
main :: Effect Unit
main = do
  projectBin <- requireFixture "PURUST_SPEC_NODE_FIXTURE_PROJECT"
  issue2Bin <- requireFixture "PURUST_SPEC_NODE_FIXTURE_ISSUE2"
  runSpecAndExitProcess'
    { defaultConfig: Config.defaultConfig { timeout = Just (Milliseconds 60000.0) }
    , parseCLIOptions: false
    }
    [ consoleReporter ]
    (spec projectBin issue2Bin)

spec :: String -> String -> Spec Unit
spec projectBin issue2Bin = do
    Spec.before_ (nukeLastResults "project") do

      let runTest = runFixture projectBin "project"

      Spec.describe "--fail-fast" do
        Spec.it "stops after first failure" do
          runTest [ "--fail-fast" ] >>= shouldFailWith "fail-fast.txt"

      Spec.describe "--example" do
        Spec.it "can filter by test name" do
          runTest [ "--example", "plane" ] >>= shouldSucceedWith "filter.txt"

        Spec.it "can filter by test name with spaces in it" do
          runTest [ "--example", "gotham city" ] >>= shouldFailWith "filter-spaces.txt"

        Spec.it "can filter by FULL test name" do
          runTest [ "--example", "gotham city is a dark" ] >>= shouldFailWith "filter-full-name.txt"

      Spec.describe "--example-matches" do
        Spec.it "can filter by test name by regex" do
          runTest [ "--example-matches", "is\\s(a plane|superman)" ] >>= shouldSucceedWith "filter-regex.txt"

        Spec.it "can filter by FULL test name" do
          runTest [ "--example-matches", "(metropolis|gotham city)\\sis\\s(superman|time)" ] >>= shouldSucceedWith "filter-full-name-regex.txt"

      Spec.describe "--only-failures" do
        Spec.it "runs only tests that failed on last run" do
          runTest [] >>= shouldFail
          runTest [ "--only-failures" ] >>= shouldFailWith "only-failures.txt"

        Spec.it "runs all tests when there is no last results file" do
          runTest [] >>= shouldFail
          FS.unlink "test-fixtures/project/.spec-results"
          runTest [ "--only-failures" ] >>= shouldFailWith "only-failures-no-results.txt"

      Spec.describe "--timeout" do
        Spec.it "can set a timeout for the tests" do
          runTest [ "--timeout", "1" ] >>= shouldFailWith "timeout.txt"

      Spec.describe "--next-failure" do
        Spec.it "runs only tests that failed last time and until first failure" do
          runTest [] >>= shouldFail
          runTest [ "--next-failure" ] >>= shouldFailWith "next-failure.txt"

      Spec.describe "Combination of several options" do
        Spec.it "can combine --fail-fast with --example" do
          runTest [ "--fail-fast", "--example", "bird" ] >>= shouldFailWith "fail-fast-and-filter.txt"

        Spec.it "can combine --only-failures with --example-matches and --timeout" do
          runTest [ "--timeout", "1" ] >>= shouldFail
          runTest [ "--only-failures", "--example-matches", "metr.+lis", "--timeout", "1" ] >>= shouldFailWith "only-failures-and-filter-regex-and-timeout.txt"

    Spec.describe "#2" do
      let dir = "issue-2-non-identity-generator-monad"
      Spec.before_ (nukeLastResults dir) do
        Spec.it "supports non-Identity test tree generator monad" do
          runFixture issue2Bin dir [ "5" ] >>= shouldTerminateWith (dir <> "/5-tests.txt")
          runFixture issue2Bin dir [] >>= shouldTerminateWith (dir <> "/3-tests.txt")
          runFixture issue2Bin dir [ "0" ] >>= shouldTerminateWith (dir <> "/0-tests.txt")

type RunResult =
  { stdout :: String
  , stderr :: String
  , error :: Maybe SysErr.SystemError
  }

requireFixture :: String -> Effect String
requireFixture name = do
  value <- lookupEnv name
  case value of
    Just path | path /= "" -> pure path
    _ -> throw ("Missing fixture binary path in " <> name)

runFixture :: String -> String -> Array String -> Aff RunResult
runFixture binary dir args = makeAff \done -> do
  _ <- CP.execFile' binary args (\options -> options { cwd = Just ("test-fixtures/" <> dir) }) \result -> do
    stdout <- Buffer.toString UTF8 result.stdout
    stderr <- Buffer.toString UTF8 result.stderr
    done $ Right { stdout, stderr, error: result.error }
  pure nonCanceler

-- | The suite reports success with exit code 0 and compares the trimmed,
-- | colourless stdout with the golden file, like the upstream suite.
shouldSucceedWith :: String -> RunResult -> Aff Unit
shouldSucceedWith fixture result = do
  case result.error of
    Just systemError ->
      fail $ "Expected the command to succeed, but it failed: " <> SysErr.code systemError <> "\n" <> result.stderr
    Nothing -> pure unit
  checkFixture fixture result.stdout

-- | The upstream `shouldFailWith` accepts any normal termination; only signal
-- | kills are rejected. The port marks those with `ERR_CHILD_PROCESS`.
shouldFailWith :: String -> RunResult -> Aff Unit
shouldFailWith fixture result = do
  case result.error of
    Just systemError
      | SysErr.code systemError == "ERR_CHILD_PROCESS" ->
          fail $ "Expected the command to terminate normally: " <> result.stderr
    _ -> pure unit
  checkFixture fixture result.stdout

-- | Any normal termination, without a golden file.
shouldTerminateWith :: String -> RunResult -> Aff Unit
shouldTerminateWith fixture result = do
  case result.error of
    Just systemError
      | SysErr.code systemError == "ERR_CHILD_PROCESS" ->
          fail $ "Expected the command to terminate normally: " <> result.stderr
    _ -> pure unit
  checkFixture fixture result.stdout

shouldFail :: RunResult -> Aff Unit
shouldFail result = case result.error of
  Nothing -> fail "Expected the command to fail, but it succeeded."
  Just _ -> pure unit

shouldSucceed :: RunResult -> Aff Unit
shouldSucceed result = case result.error of
  Nothing -> pure unit
  Just systemError ->
    fail $ "Expected the command to succeed: " <> SysErr.code systemError <> "\n" <> result.stderr

checkFixture :: String -> String -> Aff Unit
checkFixture fixture actual = do
  expected <- FS.readTextFile UTF8 ("test-fixtures/" <> fixture)
  let expectedTrimmed = trim expected
  let actualTrimmed = trim (stripColors actual)
  when (expectedTrimmed /= actualTrimmed) $
    fail $ Array.intercalate "\n"
      [ "", "===== (Actual)", actualTrimmed, "=====", "  ≠", "===== (Expected)", expectedTrimmed, "=====", "" ]

nukeLastResults :: String -> Aff Unit
nukeLastResults dir =
  FS.rm' ("test-fixtures/" <> dir <> "/.spec-results")
    { force: true, maxRetries: 1, recursive: true, retryDelay: 1000 }

-- | Removes the CSI colour sequences the reporter emits, mirroring the
-- | upstream `stripColors` without depending on the regex FFI.
stripColors :: String -> String
stripColors input = CU.fromCharArray (go (CU.toCharArray input))
  where
  go chars = case Array.uncons chars of
    Nothing -> []
    Just { head: c, tail }
      | toCharCode c == 0x1B -> case Array.uncons tail of
          Just { head: '[', tail: rest } -> go (dropSequence rest)
          _ -> Array.cons c (go tail)
      | otherwise -> Array.cons c (go tail)

  dropSequence chars = case Array.uncons chars of
    Nothing -> []
    Just { head: c, tail }
      | isFinalByte c -> tail
      | otherwise -> dropSequence tail

  isFinalByte c =
    let byte = toCharCode c
    in byte >= 0x40 && byte <= 0x7E
