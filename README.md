# Plutus Upgradeable NFTs
Final project for Cardano Developer Professional course

# Halloween P2E

- Policy ID: a unique value enforced by the Payment Public Key Hash of the first minter. 
- Token Name Template: 
  - Type
    - Monster (NFT): Halloween_{ID}_L{Level}
    - Serum (FT):    Halloween_0000_S{Level}
  - ID: Zero-padded value like 0001, 1-9999
  - Level: Integer value representing the current level, 1,2,3, etc..
Examlple -> Halloween_0036_L2

<table border="0">
 <tr>
    <td><b style="font-size:30px">Level 1 NFT</b></td>
    <td><b style="font-size:30px">Serum</b></td>
   <td><b style="font-size:30px">Level 2 NFT</b></td>
 </tr>
 <tr>
    <td><img src="https://github.com/zakhard90/plutus-upgradeable-nfts/blob/main/SkeletonWarrior_L1.png"></td>
    <td><img width="250px" height="auto" src="https://github.com/zakhard90/plutus-upgradeable-nfts/blob/main/SerumToken.png"></td>
    <td><img src="https://github.com/zakhard90/plutus-upgradeable-nfts/blob/main/SkeletonWarrior_L2.png"></td>
 </tr>
</table>

Monster is an NFT representing a collectible character card.
Serum is a fungible token which enables a level-up action. There are different levels of serum.

L1 + S1 = L2 .. and so on

# eUTxO flow diagram
![Visual representation of the UTxO flow](https://github.com/zakhard90/plutus-upgradeable-nfts/blob/main/NftUpgrader.png)

# Instructions

Note: these installation istructions are good for Linux OS

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

5. Once the nix environment is bootstrapped, `cd` to the directory where the `plutus-upgradeable-nfts` project has been placed.

6. Run `cabal update`

7. Run `cabal repl`

8. Test the project with the Emulator Trace functionality by running `NftUpgrader.Tests.upgradeSequence`

---

**Disclaimer**: NFT illustrations were generated with DALL-E
