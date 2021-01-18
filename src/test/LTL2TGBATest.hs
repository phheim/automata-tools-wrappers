----------------------------------------------------------------------------
-- |
-- Module      :  LTL2TGBATest
-- Maintainer  :  Marvin Stenger
--
-- TODO
--
-----------------------------------------------------------------------------

module LTL2TGBATest
  ( tests
  ) where

-----------------------------------------------------------------------------
import Distribution.TestSuite

import Spot.LTL2TGBA

import System.IO (readFile)

import Control.Exception (IOException, try)

-----------------------------------------------------------------------------
tests :: IO [TestInstance]
tests =
  let test =
        TestInstance
          { run = do
              let input = defaultLTL2TGBAInput {formula = "p1 xor (XX(p0 | p1) M XGF!p0)"}
              let automatonPath = "src/test/res/LTL2TGBATest.hoa"
              res <- ltl2tgba input
              case res of
                LTL2TGBAException err ->
                  return $ Finished $ Fail $ "TESTING FAILURE: " ++ err
                LTL2TGBAFailure err ->
                  return $ Finished $ Fail $ "TESTING FAILURE: " ++ err
                LTL2TGBASuccess hoa -> do
                  hoaReference <- try $ readFile automatonPath
                  case hoaReference of
                    Left ex -> do
                      let err = show (ex :: IOException)
                      return $ Finished $ Fail $ "TESTING FAILURE: " ++ err
                    Right hoaReference ->
                      if hoa == hoaReference
                      then return $ Finished Pass
                      else do
                        putStrLn "Reference:"
                        putStrLn hoaReference
                        putStrLn "Generated:"
                        putStrLn hoa
                        return $ Finished $ Fail "hoa differs from reference"
          , name = "LTL2TGBATest"
          , tags = []
          , options = []
          , setOption = \_ _ -> Right test
          }
  in
  return [test]
