{-# LANGUAGE TemplateHaskell #-}

module Pos.Types.Version
       ( ProtocolVersion (..)
       , SoftwareVersion (..)
       , ApplicationName (getApplicationName)
       , mkApplicationName
       , applicationNameMaxLength
       ) where

import           Control.Monad.Fail  (MonadFail (fail))
import           Data.Char           (isAscii)
import           Data.SafeCopy       (base, deriveSafeCopySimple)
import qualified Data.Text           as T
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, shown, stext, (%))
import           Prelude             (show)
import           Universum           hiding (show)

data ProtocolVersion = ProtocolVersion
    { pvMajor :: Word16
    , pvMinor :: Word16
    , pvAlt   :: Word8
    }
  deriving (Eq, Generic, Ord)

instance Show ProtocolVersion where
    show ProtocolVersion {..} = mconcat
        [ show pvMajor
        , "."
        , show pvMinor
        , "."
        , show pvAlt
        ]

instance Buildable ProtocolVersion where
    build = bprint shown

newtype ApplicationName = ApplicationName
    { getApplicationName :: Text
    }
  deriving (Eq, Ord, Show, Generic)

applicationNameMaxLength :: Integral i => i
applicationNameMaxLength = 7

mkApplicationName :: MonadFail m => Text -> m ApplicationName
mkApplicationName appName | T.length appName > applicationNameMaxLength
                              = fail "ApplicationName: too long string passed"
                          | T.any (not . isAscii) appName
                              = fail "ApplicationName: not ascii string passed"
                          | otherwise
                              = pure $ ApplicationName appName

data SoftwareVersion = SoftwareVersion
    { svAppName :: ApplicationName
    , svMajor   :: Word8
    , svMinor   :: Word16
    }
  deriving (Eq, Generic, Ord)

instance Buildable SoftwareVersion where
    build SoftwareVersion {..} =
      bprint (stext % ":" % build % "." % build)
         (getApplicationName svAppName) svMajor svMinor

instance Show SoftwareVersion where
    show SoftwareVersion {..} = mconcat
        [ T.unpack $ getApplicationName svAppName
        , ":"
        , show svMajor
        , "."
        , show svMinor
        ]

deriveSafeCopySimple 0 'base ''ApplicationName
deriveSafeCopySimple 0 'base ''ProtocolVersion
deriveSafeCopySimple 0 'base ''SoftwareVersion