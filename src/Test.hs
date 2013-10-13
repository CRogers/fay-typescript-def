module Test where

import Test.HUnit
import Main
import Control.Monad

testDir = "tests/"

makeTestCase :: String -> Test
makeTestCase n = TestCase $ do
	input  <- readFile $ testDir ++ n ++ ".d.ts"
	output <- readFile $ testDir ++ n ++ ".hs"
	let compiled = compile input
	assertEqual "" output compiled

testPrefixes :: [String]
testPrefixes =
	[
		"basic"
	]

testCases :: [Test]
testCases = map makeTestCase testPrefixes

runTestCases :: IO [Counts]
runTestCases = forM testCases runTestTT