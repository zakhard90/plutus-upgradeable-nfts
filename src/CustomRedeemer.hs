{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module CustomRedeemer where

import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger             (TxOutRef, TokenName, PaymentPubKeyHash)

-- Defining custom redeemer
data RedeemerParam = RP
                   { rOutRef     :: TxOutRef
                   , rTokenName  :: TokenName
                   , rMintAmount :: Integer
                   , rPkHash     :: PaymentPubKeyHash
                   }  

PlutusTx.makeIsDataIndexed ''RedeemerParam [('RP,0)]
