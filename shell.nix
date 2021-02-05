with (import <nixpkgs> {});

let hp = haskellPackages.extend (self: super: {
  masse-prelude = self.callPackage ./. {};
});

in
hp.shellFor {
  packages = h: [h.masse-prelude];
  buildInputs = [
    cabal-install
    ghcid
  ];
}
