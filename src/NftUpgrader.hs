-- TODO
-- [x] Working minting policy for NFTs with modular asset name
-- [x] Minting policy for a collection, i.e. same CurrencySymbol for all minted NFTs
-- [x] Enforce uniqueness on TokenName (Modular asset name + id + level + slice of hashed txid)
-- [x] Mintable Serum token with a custom amount
-- [x] Traverse tx inputs to get utxos with genesis NFT and Serum
-- [x] Add minting condition to allow users to mint upgraded NFT with genesis NFT and Serum
-- [x] Verify that input tokens meet the required condition, i.e. L1 + S1 = L2
-- [x] Generalize upgrade Tx construction to handle a different number of input UTxOs
-- [x] Burn the input tokens
-- [x] Generalize for L2, L3, etc..

module NftUpgrader where
