module Daedalus.Crypto where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)

import Control.Monad.Eff (Eff)

foreign import data CRYPTO :: !

foreign import isValidMnemonic :: String -> Boolean
foreign import generateMnemonic :: forall eff. Eff (crypto :: CRYPTO | eff) String

-- TODO: make this newtypes
type DataB16 = String

foreign import blake2b :: String -> Uint8Array

foreign import bytesToB16 :: Uint8Array -> DataB16

