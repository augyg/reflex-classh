{ mkDerivation, base, data-default, lens, lib, template-haskell
, text
, reflex-dom-core
, pkgs
, ClasshSS ? pkgs.haskellPackages.callPackage (pkgs.fetchFromGitHub {
  owner = "augyg";
  repo = "ClasshSS";
  rev = "bb41d2a6a756180c2aed3db567c69653a495d92e";
  sha256 = "sha256-/PB6iQqcYJWzpyEcQlI+h0DzQfg39sCwo/QzQupcEvQ=";
}) {}
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
