# Plutus Upgradeable NFTs
Final project for Cardano Developer Professional course

Ported to PlutusV2, compatible with [Plutus Application Framework v1.1.0](https://github.com/input-output-hk/plutus-apps/releases/tag/v1.1.0)  

# Halloween P2E
This contract implementation is focused on the concept of NFT upgradeability through crafting, where two tokens are consumed to produce a new one.
More specifically, the upgrade is enabled by applying a serum on an NFT of the same level, i.e. L1 + S1 = L2 .. and so on.

The sample NFT collection is themed after the forthcoming **Halloween** holiday.

<table border="0">
 <tr>
    <td><b style="font-size:30px">Level 1 NFT</b></td>
    <td><b style="font-size:30px">+ Level 1 Serum</b></td>
   <td><b style="font-size:30px">= Level 2 NFT</b></td>
 </tr>
 <tr>
    <td><img src="https://github.com/zakhard90/plutus-upgradeable-nfts/blob/main/SkeletonWarrior_L1.png"></td>
    <td><img width="250px" height="auto" src="https://github.com/zakhard90/plutus-upgradeable-nfts/blob/main/SerumToken.png"></td>
    <td><img src="https://github.com/zakhard90/plutus-upgradeable-nfts/blob/main/SkeletonWarrior_L2.png"></td>
 </tr>
</table>

The contract validator allows to perform multiple operations:
 - Minting of the genesis NFTs by the original author (L1)
 - Minting of the fungible serum tokens by the original author (S1, S2, S3...)
 - Minting of the upgraded NFTs by the user owning the original (L2, L3...)

To enforce the rules required for these functionalities, some specific design choices were made for the minting policy configuration:
- The policy id (CurrencySymbol) is **the same for all tokens**, and determines the uniqueness of an entire collection rather then of a single token. To achieve this, the PaymentPubKeyHash of the author has to be provided.
- The asset name (TokenName) has to follow a **modular fomat** to allow us to store some information in it: 
  - Type template
    - Monster (NFT): Halloween_{ID}_L{Level}
    - Serum (FT):    Halloween_0000_S{Level}
  - ID: Zero-padded value like 0001, 1-9999
  - Level: Integer value representing the current level, 1,2,3, etc..
- To make an NFT unique, a **fingerpint**, obtained from the hashed value of an input UTxO id, is appended to the TokenName

# eUTxO flow diagram
![Visual representation of the UTxO flow](https://github.com/zakhard90/plutus-upgradeable-nfts/blob/main/NftUpgrader.png)

# Instructions

*Note: these installation istructions are good for Linux OS*

To build and run the project you need to:

1. Install Nix:

``` sudo apt-get update
    sudo apt-get upgrade
    sudo apt-get install snap git nano curl
       
    curl -L https://nixos.org/nix/install > install-nix.sh
    chmod +x install-nix.sh
    ./install-nix.sh
    . ~/.nix-profile/etc/profile.d/nix.sh 
```         
 
- Make sure you have read and understood the [cache warning](https://github.com/input-output-hk/plutus-apps#cache-warning).  

2. Use git to clone the official Plutus Application Framework and checkout the tagged commit.
  - repo: https://github.com/input-output-hk/plutus-apps.git
  - tag:v1.0.0

3. Clone the `plutus-upgradeable-nfts` repository too.

4. Open a new terminal from the `plutus-apps` directory and start `nix-shell`

5. Once the nix environment is bootstrapped, `cd` to the directory where the `plutus-upgradeable-nfts` project is situated.

6. Run `cabal update`

7. Run `cabal repl`

8. Test the project with the Emulator Trace functionality by running `NftUpgrader.Tests.upgradeSequence`

# Functionality overview

## Genesis NFT minting
The original L1 NFTs can be minted only by the collection author, i.e. the user deploying the contract. A PaymentPubKeyHash is used to define a unique CurrencySymbol for the whole collection. The TokenName follows a specific pattern and the positions of the bytes corresponding to different segments are currently fixed. The collection author is also allowed to directly transfer the newly minted tokens to any wallet.

## Serum FT minting
With a specific TokenName a Serum fungible token can be minted by the collection author. The mint can be performed for an arbitrary amount of tokens, the validator makes sure that there is no confusion between NFT and FT minting. To keep the same byte positions in both cases, the Serum's TokenName value contains a filler of zeros without any functionality.

## NFT upgrading
The contract minting policy allows any user to mint a new non genesis (L2+) NFT just by possesing both an NFT of a lower level and the corresponding Serum. The validation process requires 3 UTxO inputs carrying different values each:
- Ada
- NFT
- Serum

To be able to produce the same CurrencySymbol, the user upgrading an NFT has to provide the PaymentPubKeyHash of the collection author account. In the process of upgrading, the input tokens are burned.

## Test scenarios
There are currently 2 test sequences:
- NftUpgrader.Tests.upgradeSequence - performs a series of different token mints and a final upgrade
- NftUpgrader.Tests.upgradeAdvSequence - performs a series of mints with the goal of leveling-up an NFT to L3

The Token name values resulting from the test can be inspected with [this converter](https://dencode.com/en/string/hex).

Given the following output,

```
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 103995874
    {886e646b58999b149f212dfc1278f4956d75ef482ca13f7f301dc83d, 0x48616c6c6f7765656e5f303030305f53315f96338d5cb0bf427a}: 4
    {886e646b58999b149f212dfc1278f4956d75ef482ca13f7f301dc83d, 0x48616c6c6f7765656e5f303030315f4c325ffdd3a42e7f7c76f3}: 1
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {886e646b58999b149f212dfc1278f4956d75ef482ca13f7f301dc83d, 0x48616c6c6f7765656e5f303030325f4c315f289f7b836bf5b15e}: 1
    {, ""}: 95987622
```
it is possible to inspect the results by decoding the hexadecimal string values defining asset names (TokenName).
For example`0x48616c6c6f7765656e5f303030325f4c315f289f7b836bf5b15e` can be split into different segments (5f is an underscore separator):

```
48616c6c6f7765656e5f303030325f4c31 + 5f + 289f7b836bf5b15e
^--------Modular TokenName-------^        ^--Fingerprint-^

48616c6c6f7765656e5f303030325f4c31 --> Halloween_0002_L1
```

---

**Disclaimer**: NFT illustrations were generated with DALL-E
