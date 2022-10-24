{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}

module NftUpgrader.Tests where

import           NftUpgrader.OffChain

import           Control.Monad                  hiding (fmap)
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
import           Prelude                        (IO)
import           Wallet.Emulator.Wallet 
import           Plutus.Trace.Emulator          as Emulator

-- Test generic upgrading scenario
upgradeSequence :: IO ()
upgradeSequence = runEmulatorTraceIO $ do

    let s1 = "Halloween_0000_S1_"
        qs = 5
        t1 = "Halloween_0001_L1_"
        q1 = 1
        t2 = "Halloween_0002_L1_"
        q2 = 1
        u1 = "Halloween_0001_L2_"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    
    callEndpoint @"mint" h1 $ NFTMintParams
        { mToken    = t1
        , mAmount   = q1
        , mAddress  = mockWalletAddress w1
        , mReceiver = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ NFTMintParams
        { mToken    = s1
        , mAmount   = qs
        , mAddress  = mockWalletAddress w1
        , mReceiver = mockWalletAddress w2
        }   
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ NFTMintParams
        { mToken    = t2
        , mAmount   = q2
        , mAddress  = mockWalletAddress w1
        , mReceiver = mockWalletAddress w1
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"upgrade" h2 $ NFTUpgradeParams
        { uToken    = u1
        , uAddress  = mockWalletAddress w2
        , uMinter   = mockWalletAddress w1
        }
    void $ Emulator.waitNSlots 1
