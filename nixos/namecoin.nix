{ config, lib, pkgs, ... }:

let
  custom = import ((import <nixpkgs> { config = {}; overlays = []; }).fetchFromGitHub {
    owner = "Infinisil";
    repo = "nixpkgs";
    rev = "bb6fa63d85aa18cbd5cb15a99b8c7baf4f8e6e41";
    sha256 = "1187bv8rbgskzcihlzm0rlnijjch03v44xpc3131dlx84vfbvzvl";
  }) {};
in
{
  nixpkgs.overlays = [
    (self: super: {
      altcoins = custom.altcoins;
    })
  ];
  
  environment.systemPackages = with pkgs; [
    altcoins.namecoind
  ];

  networking = {
    firewall = {
      allowedTCPPorts = [
        
      ];
    };
  };

  services.namecoind = {
    enable = true;
    rpc.user = "infinisil";
    rpc.password = "ccgjkqbxtnheprcgansaoetuh";
  };

}
