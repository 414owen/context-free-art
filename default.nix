{ pkgs ? import <unstable> {} }:

pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv: pkgs.haskell.lib.addBuildTools drv (
    with pkgs.haskellPackages; [
      hlint
      haskell-language-server 
    ]
  );
}
