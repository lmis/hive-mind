let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./default.nix {
    extraPackages = p: with p; [
      git
      ghcid
      haskellPackages.brittany
      haskellPackages.hoogle
      hlint
    ];
    shellHook = "shopt -s globstar";
  }
