{ mkDerivation, base, bytestring, containers, mtl, stdenv, text
, time, unliftio
}:
mkDerivation {
  pname = "masse-prelude";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers mtl text time unliftio
  ];
  license = stdenv.lib.licenses.mit;
}
