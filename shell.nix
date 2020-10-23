let
  mypkgs = import ./. {};
  localPackages = import ./nix/localPackages.nix {};
#   shellFrom = import ./nix/shellFrom.nix {};
# in shellFrom mypkgs.rhine
in localPackages.shellFor {
  packages = pkgs: with pkgs; [
    rhine
    gloss
  ];
}
