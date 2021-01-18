----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Maintainer  :  Marvin Stenger
--
-- Standard TestSuite.
--
-----------------------------------------------------------------------------

module Test
  ( Test.tests
  ) where

-----------------------------------------------------------------------------
import Distribution.TestSuite

import qualified LTL2TGBATest (tests)

-----------------------------------------------------------------------------
-- | The Tests
tests :: IO [Test]
tests = do
  spotTests <- LTL2TGBATest.tests
  return $ concatMap (map Test) [spotTests]
