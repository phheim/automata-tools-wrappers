----------------------------------------------------------------------------
-- |
-- Module      :  LTL2TGBA
-- Maintainer  :  Marvin Stenger
--
-- TODO
--
-----------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

-----------------------------------------------------------------------------
module Spot.LTL2TGBA where

-----------------------------------------------------------------------------

import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-----------------------------------------------------------------------------
type Error = String

data LTL2TGBAResult =
    LTL2TGBASuccess String
  | LTL2TGBAFailure Error
  | LTL2TGBAException Error

-----------------------------------------------------------------------------
-- | ltl2tgba (spot) plain wrapper
ltl2tgbaCMD :: String -> [String] -> IO LTL2TGBAResult
ltl2tgbaCMD stdin args =
  let executable = "ltl2tgba" in
  findExecutable executable
  >>= \case
    Nothing -> return $ LTL2TGBAException (executable ++ " not found")
    Just ltl2tgba -> do
      (ec,out,err) <- readProcessWithExitCode ltl2tgba args stdin
      case ec of
        ExitSuccess   -> return $ LTL2TGBASuccess out
        ExitFailure _ -> return $ LTL2TGBAFailure err

-----------------------------------------------------------------------------
-- | TODO
data StateBasedAcceptance =
    StateBasedAcceptance       -- ^ state-based Automaton
  | TransitionBasedAcceptance  -- ^ transition-based Automaton (default)
  deriving (Show)

-----------------------------------------------------------------------------
-- | TODO
data Parity =
    ParityAny
  | ParityMin
  | ParityMax
  | ParityOdd
  | ParityEven
  | ParityMinOdd
  | ParityMinEven
  | ParityMaxOdd
  | ParityMaxEven
  deriving (Show)

-----------------------------------------------------------------------------
-- | TODO
data Acceptance =
    AcceptanceDefault                                                                 -- ^ (-> tgba)
  | AcceptanceGeneric                                                                 -- ^ any acceptance condition is allowed
  | AcceptanceBuchi                                                                   -- ^ Büchi Automaton (implies stateBasedAcceptance)
  | AcceptanceCoBuchi { acceptance :: StateBasedAcceptance }                          -- ^ automaton with co-Büchi acceptance (will recognize a superset of the input language if not co-Büchi realizable)
  | AcceptanceMonitor { acceptance :: StateBasedAcceptance }                          -- ^ Monitor (accepts all finite prefixes of the given property)
  | AcceptanceParity { parity :: Parity, acceptance :: StateBasedAcceptance }         -- ^ automaton with parity acceptance
  | AcceptanceColoredParity { parity :: Parity, acceptance :: StateBasedAcceptance }  -- ^ colored automaton with parity acceptance
  | AcceptanceTGBA                                                                    -- ^ transition-based Generalized Büchi Automaton
  deriving (Show)

-----------------------------------------------------------------------------
-- | TODO
data SimplificationGoal =
    SimplificationGoalDefault        -- ^ (-> small)
  | SimplificationGoalAny            -- ^ no preference, do not bother making it small or deterministic
  | SimplificationGoalDeterministic  -- ^ prefer deterministic automata (combine with generic to be sure to obtain a deterministic automaton)
  | SimplificationGoalSmall          -- ^ prefer small automata
  deriving (Show)

-----------------------------------------------------------------------------
-- | TODO
data SimplificationLevel =
    SimplificationLevelDefault  -- ^ (-> high)
  | SimplificationLevelHigh     -- ^ all available optimizations
  | SimplificationLevelMedium   -- ^ moderate optimizations
  | SimplificationLevelLow      -- ^ minimal optimizations
  deriving (Show)

-----------------------------------------------------------------------------
-- | TODO
data LTL2TGBAInput =
  LTL2TGBAInput
    { -- | process the formula
      formula :: String

    , -- | negate the formula
      negate :: Bool

    , -- | output automaton acceptance
      acceptance :: Acceptance

    , -- | output a complete automaton
      complete :: Bool
    , -- | output unambiguous automata
      unambiguous :: Bool

    , -- | simplicication goal
      simplificationGoal :: SimplificationGoal

    , -- | simplicication level
      simplificationLevel :: SimplificationLevel

    , -- | extra parameters
      extraParams :: [String]
    }

-----------------------------------------------------------------------------
-- | Default LTL2TGBAInput
defaultLTL2TGBAInput
  :: LTL2TGBAInput

defaultLTL2TGBAInput =
  LTL2TGBAInput
    { formula             = ""
    , negate              = False
    , acceptance          = AcceptanceDefault
    , complete            = False
    , unambiguous         = False
    , simplificationGoal  = SimplificationGoalDefault
    , simplificationLevel = SimplificationLevelDefault
    , extraParams         = []
    }


-----------------------------------------------------------------------------
class LTL2TGBAArgument a where
  toString :: a -> String
  toString _ = ""
  toArgs :: a -> [String]
  toArgs _ = []

instance LTL2TGBAArgument Parity where
  toString = \case
    ParityAny     -> "any"
    ParityMin     -> "min"
    ParityMax     -> "max"
    ParityOdd     -> "odd"
    ParityEven    -> "even"
    ParityMinOdd  -> "min odd"
    ParityMinEven -> "min even"
    ParityMaxOdd  -> "max odd"
    ParityMaxEven -> "max even"

instance LTL2TGBAArgument Acceptance where
  toArgs = \case
    AcceptanceDefault -> []
    AcceptanceGeneric -> ["--generic"]
    AcceptanceBuchi -> ["--ba"]
    AcceptanceCoBuchi{..} ->
      let args = ["--cobuchi"] in
      case acceptance of
        StateBasedAcceptance -> args ++ ["--state-based-acceptance"]
        _                    -> args
    AcceptanceMonitor{..} ->
      let args = ["--monitor"] in
      case acceptance of
        StateBasedAcceptance -> args ++ ["--state-based-acceptance"]
        _                    -> args
    AcceptanceParity{..} ->
      let args = ["--parity=" ++ (toString parity)] in
      case acceptance of
        StateBasedAcceptance -> args ++ ["--state-based-acceptance"]
        _                    -> args
    AcceptanceColoredParity{..} ->
      let args = ["--colored-parity=" ++ (toString parity)] in
      case acceptance of
        StateBasedAcceptance -> args ++ ["--state-based-acceptance"]
        _                    -> args
    AcceptanceTGBA -> ["--tgba"]

instance LTL2TGBAArgument SimplificationGoal where
  toArgs = \case
    SimplificationGoalDefault       -> []
    SimplificationGoalAny           -> ["--any"]
    SimplificationGoalDeterministic -> ["--deterministic"]
    SimplificationGoalSmall         -> ["--small"]

instance LTL2TGBAArgument SimplificationLevel where
  toArgs = \case
    SimplificationLevelDefault -> []
    SimplificationLevelHigh    -> ["--high"]
    SimplificationLevelMedium  -> ["--medium"]
    SimplificationLevelLow     -> ["--low"]

instance LTL2TGBAArgument LTL2TGBAInput where
  toArgs LTL2TGBAInput{..} =
    ["--negate" | negate]
    ++
    (toArgs acceptance)
    ++
    ["--complete" | complete]
    ++
    ["--unambiguous" | unambiguous]
    ++
    (toArgs simplificationGoal)
    ++
    (toArgs simplificationLevel)
    ++
    extraParams

-----------------------------------------------------------------------------
-- | ltl2tgba (spot) wrapper
ltl2tgba :: LTL2TGBAInput -> IO LTL2TGBAResult
ltl2tgba input@LTL2TGBAInput{formula} =
  ltl2tgbaCMD formula (toArgs input)
