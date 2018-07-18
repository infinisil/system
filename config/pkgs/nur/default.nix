{ pkgs, lib }:
{

  inherit (import ../default.nix {
    inherit pkgs lib;
    passwords = false;
  })
  arcred
  compton-kawase
  eclipse
  imgurdl
  ;

}
