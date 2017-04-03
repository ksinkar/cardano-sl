-- | Specification for Pos.Update.MemState.Functions

module Test.Pos.Update.MemStateSpec
       ( spec
       ) where

import qualified Data.HashMap.Strict as HM

import           Pos.Binary            (Bi)
import           Pos.Crypto            (hash)
import           Pos.Update.Arbitrary  ()
import qualified Pos.Update.Core       as Upd
import qualified Pos.Update.MemState   as Upd

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (===), (.&&.))
import           Universum

spec :: Spec
spec = describe "MemState" $ do
    describe "addToMemPool" $ do
        prop
            "applying an update payload to the mempool means all update votes are\
            \ properly added to it, and if it exists, the update proposal is added\
            \ correctly to the mempool as well."
            payloadIsAddedToMemPool

payloadIsAddedToMemPool
    :: Bi Upd.UpdateProposal
    => Upd.UpdatePayload
    -> Upd.MemPool
    -> Property
payloadIsAddedToMemPool up@Upd.UpdatePayload {..} mp@Upd.MemPool {..} =
    proposalWasAdded .&&. votesWereAdded
  where
    proposalWasAdded = case upProposal of
        Nothing -> True
        Just uProp -> HM.lookup (hash uProp) mpProposals == Just uProp
    votesWereAdded = foldr' verifyVoteWasAdded True upVotes
    verifyVoteWasAdded vote@(Upd.UpdateVote {..}) bool = bool &&
        let theProposalVotes = HM.lookup uvProposalId mpLocalVotes
            theKeysVote = theProposalVotes >>= HM.lookup uvKey
        in theKeysVote == Just vote
    mp'@(Upd.MemPool mpProp mpVotes) = Upd.addToMemPool up mp
