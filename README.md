# learn_haskell
# Haskell Project Setup

This guide will walk you through installing Haskell, setting up your project, and running individual files from the command line.

## 1. Install Haskell

The easiest way to get started with Haskell is by installing the Glasgow Haskell Compiler (GHC) and Cabal, the Haskell build tool. We recommend using the GHCup installer:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.sh | sh
```

Follow the on-screen instructions to install GHC and Cabal.  You may need to restart your terminal or source your profile (e.g., `source ~/.profile` or `source ~/.bashrc`) to make the commands available.

## 2. Setting Up Your Project

Create a new directory for your Haskell project:

```bash
mkdir my-haskell-project
cd my-haskell-project
```

Now you can create your Haskell files (e.g., `MyFile.hs`).

## 3. Running Haskell Files

### Using `runghc`

To run a Haskell file directly from the command line (compiling and executing), use the `runghc` command followed by the filename:

```bash
runghc MyFile.hs
```

This will compile and execute your Haskell code.