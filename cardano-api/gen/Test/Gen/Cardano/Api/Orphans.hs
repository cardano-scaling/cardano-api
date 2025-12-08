{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Gen.Cardano.Api.Orphans (obtainArbitraryConstraints) where

import Cardano.Api hiding (DijkstraEra, txIns)
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Address ()
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()

import Data.Functor.Identity
import System.Random.Stateful
  ( StatefulGen (..)
  , runStateGen_
  )

import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  )
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.Natural ()

import Generic.Random (genericArbitraryU)

-- | Pseudo random generator compatible with QuickCheck's stateful interface.
data QC = QC

instance StatefulGen QC Gen where
  uniformWord32 QC = MkGen (\r _n -> runStateGen_ r uniformWord32)
  {-# INLINE uniformWord32 #-}
  uniformWord64 QC = MkGen (\r _n -> runStateGen_ r uniformWord64)
  {-# INLINE uniformWord64 #-}
  uniformShortByteString k QC =
    MkGen (\r _n -> runStateGen_ r (uniformShortByteString k))
  {-# INLINE uniformShortByteString #-}



-------------------------------------------------------------------------------

-- * Helper: Era-Specific Arbitraries

-------------------------------------------------------------------------------

obtainArbitraryConstraints
  :: ShelleyBasedEra era
  -> ( ( Arbitrary (ShelleyPParams Identity (ShelleyLedgerEra era))
       , Arbitrary (L.VotingProcedures (ShelleyLedgerEra era))
       , Arbitrary (L.ProposalProcedure (ShelleyLedgerEra era))
       )
       => a
     )
  -> a
obtainArbitraryConstraints era f = case era of
  ShelleyBasedEraShelley -> f
  ShelleyBasedEraAllegra -> f
  ShelleyBasedEraMary -> f
  ShelleyBasedEraAlonzo -> f
  ShelleyBasedEraBabbage -> f
  ShelleyBasedEraConway -> f
  ShelleyBasedEraDijkstra -> f

instance Arbitrary (DijkstraPParams Identity DijkstraEra) where
  arbitrary = genericArbitraryU

instance Arbitrary (DijkstraPParams StrictMaybe DijkstraEra) where
  arbitrary = genericArbitraryU

