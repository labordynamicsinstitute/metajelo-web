{
  nodejsVersion ? "10.14.1",
  yarnVersion ? "1.12.3",
  purescriptVersion ? "0.12.2",
  nixjs ? fetchTarball "https://github.com/cprussin/nixjs/archive/0.0.7.tar.gz",
  # nixpkgs ? (import ./pkgs.nix) # or change to: ? <nixpkgs>
  # how to get this to work?
  nixpkgs ? <nixpkgs>
}:


with import nixpkgs {
  overlays = [
    (import nixjs {
      nodejs = nodejsVersion;
      yarn = yarnVersion;
      purescript = purescriptVersion;
    })
  ];
};

mkShell {
  buildInputs = [
    nodejs yarn purescript
    nodePackages.bower
    nodePackages.pulp
    psc-package
  ];
}
