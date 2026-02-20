{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  reflex-classh = import ./default.nix;
  drv = variant (haskellPackages.callPackage reflex-classh {
    #ClasshSS = pkgs.haskell.lib.doJailbreak (haskellPackages.callCabal2nix "ClasshSS" ../ClasshSS-dev {});
  }); 
in
pkgs.mkShell {
  buildInputs = [ pkgs.cabal-install ];
  inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
} 



