{ pkgs }:
{

  inherit (import ../default.nix {
    inherit pkgs;
    lib = pkgs.lib;
    passwords = false;
  })
  arcred
  compton-kawase
  eclipse
  imgurdl
  soph
  ;

}
