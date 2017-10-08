{

  mac = (import <nixpkgs/nixos> { configuration = ./mac; });
  dobby = (import <nixpkgs/nixos> { configuration = ./dobby.nix; });

}
