# Plutus Upgradeable NFTs
Final project for Cardano Developer Professional course

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
  - tag:62efdd2bfab3e076d40e07f8f4d7864a7f2ccc91

3. Clone the `plutus-upgradeable-nfts` repository too.

4. Open a new terminal from the `plutus-apps` directory and start `nix-shell`

5. Once the nix environment is bootstrapped, `cd` to the directory where the `plutus-upgradeable-nfts` project is situated.

6. Run `cabal update`

7. Run `cabal repl`

8. Test the project with the Emulator Trace functionality by running `NftUpgrader.Tests.upgradeSequence`

# Functionality overview

## Genesis NFT minting

## Serum FT minting

## NFT upgrading


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
