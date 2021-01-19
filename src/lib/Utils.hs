----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Maintainer  :  Marvin Stenger
--
-- TODO
--
-----------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}

-----------------------------------------------------------------------------
module Utils
  ( cmd
  ) where

-----------------------------------------------------------------------------

import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-----------------------------------------------------------------------------
-- | cmd
cmd :: String -> String -> [String] -> IO (Either String (ExitCode, String, String))
cmd executable stdin args =
  findExecutable executable
  >>= \case
    Nothing  -> return $ Left $ executable ++ " not found"
    Just exe -> Right <$> readProcessWithExitCode exe args stdin
