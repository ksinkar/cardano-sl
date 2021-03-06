{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Wallet.KeyStorage
       ( MonadKeys (..)
       , newSecretKey
       , KeyStorage (..)
       , KeyData
       , KeyError (..)
       , runKeyStorage
       , runKeyStorageRaw
       ) where

import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (iso, lens, (%=), (<>=))
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (ReaderT (..), ask)
import           Control.Monad.State         (MonadState (..))
import           Control.Monad.Trans         (MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Mockable                    (ChannelT, Counter, Distribution, Gauge,
                                              MFunctor', Mockable (liftMockable), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Binary.Crypto           ()
import           Pos.Client.Txp.Balances     (MonadBalances)
import           Pos.Client.Txp.History      (MonadTxHistory)
import           Pos.Communication.PeerState (PeerStateHolder)
import           Pos.Context                 (ContextHolder (..), NodeContext (..),
                                              WithNodeContext (..))
import           Pos.Crypto                  (EncryptedSecretKey, PassPhrase, SecretKey,
                                              hash, safeKeyGen)
import           Pos.DB                      (MonadDB)
import           Pos.DB.Limits               (MonadDBLimits)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Delegation.Holder       (DelegationT (..))
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Reporting.MemState      (MonadReportingMem)
import           Pos.Slotting                (MonadSlots, MonadSlotsData, NtpSlotting,
                                              SlottingHolder)
import           Pos.Ssc.Extra               (SscHolder (..))
import           Pos.Txp                     (TxpHolder (..))
import           Pos.Util                    ()
import           Pos.Util.UserSecret         (UserSecret, peekUserSecret, usKeys,
                                              usPrimKey, writeUserSecret)
import           Pos.Wallet.Context          (WithWalletContext)
import           Pos.Wallet.State.State      (MonadWalletDB)

type KeyData = STM.TVar UserSecret

----------------------------------------------------------------------
-- MonadKeys class
----------------------------------------------------------------------

class Monad m => MonadKeys m where
    getPrimaryKey :: m (Maybe SecretKey)
    getSecretKeys :: m [EncryptedSecretKey]
    addSecretKey :: EncryptedSecretKey -> m ()
    deleteSecretKey :: Word -> m ()

    default getPrimaryKey :: (MonadTrans t, MonadKeys m', t m' ~ m) => m (Maybe SecretKey)
    getPrimaryKey = lift getPrimaryKey

    default getSecretKeys :: (MonadTrans t, MonadKeys m', t m' ~ m) => m [EncryptedSecretKey]
    getSecretKeys = lift getSecretKeys

    default addSecretKey :: (MonadTrans t, MonadKeys m', t m' ~ m) => EncryptedSecretKey -> m ()
    addSecretKey = lift . addSecretKey

    default deleteSecretKey :: (MonadTrans t, MonadKeys m', t m' ~ m) => Word -> m ()
    deleteSecretKey = lift . deleteSecretKey

-- | Instances for common transformers
instance MonadKeys m => MonadKeys (ReaderT r m)
instance MonadKeys m => MonadKeys (StateT s m)

-- | Instances for ancestor in the monadic stack
instance MonadKeys m => MonadKeys (KademliaDHT m)
instance MonadKeys m => MonadKeys (PeerStateHolder m)
instance MonadKeys m => MonadKeys (NtpSlotting m)
instance MonadKeys m => MonadKeys (SlottingHolder m)

-- | Helper for generating a new secret key
newSecretKey :: (MonadIO m, MonadKeys m) => PassPhrase -> m EncryptedSecretKey
newSecretKey pp = do
    (_, sk) <- safeKeyGen pp
    addSecretKey sk
    return sk

------------------------------------------------------------------------
-- Common functions
------------------------------------------------------------------------

getSecret
    :: (MonadIO m, MonadReader KeyData m)
    => m UserSecret
getSecret = ask >>= atomically . STM.readTVar

putSecret
    :: (MonadIO m, MonadReader KeyData m)
    => UserSecret -> m ()
putSecret s = ask >>= atomically . flip STM.writeTVar s >> writeUserSecret s

deleteAt :: Int -> [a] -> [a]
deleteAt j ls = let (l, r) = splitAt j ls in l ++ drop 1 r

containsKey :: [EncryptedSecretKey] -> EncryptedSecretKey -> Bool
containsKey ls k = hash k `elem` map hash ls

------------------------------------------------------------------------
-- KeyStorage transformer
------------------------------------------------------------------------

newtype KeyStorage m a = KeyStorage
    { getKeyStorage :: ReaderT KeyData m a
    } deriving (Functor, Applicative, Monad, MonadSlotsData,
                MonadThrow, MonadSlots, MonadCatch, MonadIO, MonadFail,
                HasLoggerName, CanLog, MonadMask,
                MonadReader KeyData, MonadDB,
                MonadWalletDB, WithWalletContext, WithNodeContext ssc,
                MonadDelegation, MonadTrans, MonadBase io, MonadFix,
                MonadDBLimits, MonadReportingMem,
                MonadTxHistory, MonadBalances)

instance Monad m => WrappedM (KeyStorage m) where
    type UnwrappedM (KeyStorage m) = ReaderT KeyData m
    _WrappedM = iso getKeyStorage KeyStorage

instance (MonadIO m) => MonadState UserSecret (KeyStorage m) where
    get = KeyStorage getSecret
    put = KeyStorage . putSecret

instance MonadTransControl KeyStorage where
    type StT KeyStorage a = StT (ReaderT KeyData) a
    liftWith = defaultLiftWith KeyStorage getKeyStorage
    restoreT = defaultRestoreT KeyStorage

instance MonadBaseControl IO m => MonadBaseControl IO (KeyStorage m) where
    type StM (KeyStorage m) a = ComposeSt KeyStorage m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type instance ThreadId (KeyStorage m) = ThreadId m
type instance Promise (KeyStorage m) = Promise m
type instance SharedAtomicT (KeyStorage m) = SharedAtomicT m
type instance Counter (KeyStorage m) = Counter m
type instance Distribution (KeyStorage m) = Distribution m
type instance SharedExclusiveT (KeyStorage m) = SharedExclusiveT m
type instance Gauge (KeyStorage m) = Gauge m
type instance ChannelT (KeyStorage m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (KeyStorage m) (ReaderT KeyData m)
         , MFunctor' d (ReaderT KeyData m) m
         ) => Mockable d (KeyStorage m) where
    liftMockable = liftMockableWrappedM

runKeyStorage :: (MonadIO m, MonadThrow m) => FilePath -> KeyStorage m a -> m a
runKeyStorage fp ks =
    peekUserSecret fp >>= liftIO . STM.newTVarIO >>= runKeyStorageRaw ks

runKeyStorageRaw :: KeyStorage m a -> KeyData -> m a
runKeyStorageRaw = runReaderT . getKeyStorage

instance (MonadIO m) => MonadKeys (KeyStorage m) where
    getPrimaryKey = use usPrimKey
    getSecretKeys = use usKeys
    addSecretKey sk =
        whenM (not . (`containsKey` sk) <$> use usKeys) $
            usKeys <>= [sk]
    deleteSecretKey (fromIntegral -> i) = usKeys %= deleteAt i

-------------------------------------------------------------------------
-- ContextHolder instance
-------------------------------------------------------------------------

data KeyError =
    PrimaryKey !Text -- ^ Failed attempt to delete primary key
    deriving (Show)

instance Exception KeyError

usLens :: Lens' (NodeContext ssc) KeyData
usLens = lens ncUserSecret $ \c us -> c { ncUserSecret = us }

instance Monad m => MonadReader KeyData (ContextHolder ssc m) where
    ask = ncUserSecret <$> getNodeContext
    local f = ContextHolder . local (usLens %~ f) . getContextHolder

instance (MonadIO m) =>
         MonadState UserSecret (ContextHolder ssc m) where
    get = getSecret
    put = putSecret

instance (MonadIO m, MonadThrow m) =>
         MonadKeys (ContextHolder ssc m) where
    getPrimaryKey = use usPrimKey
    getSecretKeys = use usKeys
    addSecretKey sk =
        whenM (not . (`containsKey` sk) <$> use usKeys) $
            usKeys <>= [sk]
    deleteSecretKey (fromIntegral -> i) = usKeys %= deleteAt i

-- | Derived instances for ancestors in monad stack
deriving instance MonadKeys m => MonadKeys (SscHolder ssc m)
deriving instance MonadKeys m => MonadKeys (TxpHolder __ m)
deriving instance MonadKeys m => MonadKeys (DelegationT m)
