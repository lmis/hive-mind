let
  pinnedPkgs = fetchGit {
    name = "nixos-20.03-2020-06-14";
    url = "https://github.com/nixos/nixpkgs-channels/";
    # git ls-remote https://github.com/nixos/nixpkgs-channels nixos-20.03
    rev="8b071be7512bd2cd0ff5c3bdf60f01ab4eb94abd";
    ref="refs/heads/nixos-20.03";
  };
in
{
  pkgs ? import pinnedPkgs {},
  extraPackages ? (pkgs: []),
  shellHook ? ""
}: with pkgs; haskellPackages.developPackage {
  name = "bare-project";
  root = nix-gitignore.gitignoreSource [".gitignore" "default.nix"] ./.;
  modifier = drv: haskell.lib.overrideCabal drv (old: {
    inherit shellHook;
    buildDepends = with pkgs; [
      ghc
      haskellPackages.cabal-install
    ] ++ extraPackages pkgs;
  });
}
