{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-splices #-}

-- | Arbitrary instances for Update System types

module Pos.Update.Arbitrary.MemState
       (
       ) where

import           Data.DeriveTH       (derive, makeArbitrary)

import qualified Pos.Update.MemState as Upd

import           Test.QuickCheck     (Arbitrary (..))

derive makeArbitrary ''Upd.MemPool
