{ mkDerivation, base, data-default, lens, lib, template-haskell
, text
, reflex-dom-core
, pkgs
, ClasshSS ? pkgs.haskellPackages.callCabal2nix "ClasshSS" ../ClasshSS-dev {}
}:
mkDerivation {
  pname = "reflex-classhss";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base data-default lens template-haskell text (pkgs.haskell.lib.doJailbreak ClasshSS) reflex-dom-core
  ];
  homepage = "https://github.com/augyg/ClasshSS";
  description = "Typified Tailwind for Rapid Development";
  license = lib.licenses.mit;
}
