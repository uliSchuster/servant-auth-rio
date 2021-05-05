{-# OPTIONS_GHC -F -pgmF tasty-discover #-}

-- This module serves as starting point for automatic test case discovery.
-- The above GHC options pragma, in combination with the "tasty-discovery"
-- package instructs GHC to automatically discover and execute all test cases
-- in the current directory.
-- See https://www.stackage.org/package/tasty-discover
-- Depending on the test-case prefix, the corresponding test-runner is used:
-- unit_*: HUnit test case
-- spec_*: HSpec test cases
-- prop_*: QuickCheck test case
-- test_*: Tasty test trees
