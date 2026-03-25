{ mkDerivation, base, containers, data-default, hedgehog, henforcer
, lens, lib, tasty, tasty-hedgehog, template-haskell, text
, reflex-dom-core
, pkgs
, ClasshSS ? pkgs.haskellPackages.callCabal2nix "ClasshSS" ../ClasshSS-dev {}
}:
mkDerivation {
  pname = "reflex-classhss";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers data-default henforcer lens template-haskell text
    ClasshSS reflex-dom-core
  ];
  testHaskellDepends = [
    base containers hedgehog tasty tasty-hedgehog text ClasshSS
  ];
  homepage = "https://github.com/augyg/ClasshSS";
  description = "Typified Tailwind for Rapid Development";
  license = lib.licenses.mit;
}
