{ config, lib, pkgs, ... }:

{
  networking = {
    firewall = {
      allowedTCPPorts = [

      ];
    };
  };

  services.namecoind = {
    enable = false;
    rpc.user = "infinisil";
    rpc.password = "ccgjkqbxtnheprcgansaoetuh";
  };

  systemd.services.namecoind.wantedBy = lib.mkForce [];

}
