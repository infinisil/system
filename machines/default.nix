{

  mac = (import <nixpkgs/nixos> { configuration = ./mac; }).config;
  dobby = (import <nixpkgs/nixos> { configuration = ./dobby.nix; }).config;

}
