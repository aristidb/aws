{
  packageOverrides = pkgs: rec {
    haskellPackages = with pkgs.haskellPackages; pkgs.haskellPackages // rec {
       errors  = callPackage ./errors.nix {};
     };
  };
}
