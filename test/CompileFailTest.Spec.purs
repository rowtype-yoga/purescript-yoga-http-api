module CompileFailTest.Spec where

import Prelude

import CompileFail (compileFile, spagoSources, warmCache)
import Data.Array as Array
import Data.Foldable (for_)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.FS.Sync as FSSync
import ViTest (beforeAll, describe, test)
import ViTest.Expect (expectToBe)

casesDir :: String
casesDir = "test-compile-fail/cases"

testCompileFail :: Effect Unit
testCompileFail = describe "Compile-fail tests" do
  ctxRef <- liftEffect $ Ref.new { sources: [], outputDir: "output-compile-fail" }

  beforeAll do
    sources <- spagoSources
    let ctx = { sources, outputDir: "output-compile-fail" }
    liftEffect $ Ref.write ctx ctxRef
    warmCache ctx

  pursFiles <- liftEffect do
    files <- FSSync.readdir casesDir
    pure $ Array.sort $ Array.filter (String.contains (Pattern ".purs")) files

  for_ pursFiles \file -> do
    let filePath = casesDir <> "/" <> file
    let testName = String.replace (Pattern ".purs") (String.Replacement "") file

    test testName do
      ctx <- liftEffect $ Ref.read ctxRef
      result <- compileFile ctx filePath
      expectToBe true result.compilationFailed
      expectToBe true result.containsExpected
