{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module NftUpgrader.PolicyRedeemer where

import qualified PlutusTx
import           PlutusTx.Prelude
import           Plutus.V2.Ledger.Api     (TxOutRef)
import           Ledger                   (TokenName)

-- Defining custom redeemer
data RedeemerParam = RP
                   { rOutRef     :: TxOutRef
                   , rTokenName  :: TokenName
                   , rMintAmount :: Integer
                   }  

PlutusTx.makeIsDataIndexed ''RedeemerParam [('RP,0)]
