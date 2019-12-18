{nixpkgs ? import <nixpkgs> {}, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "layer3com";
  buildInputs = [ 
                  # zlib
                  # haskellPackages.yesod-bin
                  # msmtp
                  # gmp
                ];
  inherit ghc;
}
