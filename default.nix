{ mkDerivation, base, data-default, lens, lib, template-haskell
, text, reflex-dom-core
}:
mkDerivation {
  pname = "reflex-classhss";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base data-default lens template-haskell text reflex-dom-core
  ];
  homepage = "https://github.com/augyg/ClasshSS";
  description = "Typified Tailwind for Rapid Development";
  license = lib.licenses.mit;
}
