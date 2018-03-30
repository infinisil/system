{ config, pkgs, nodes, ... }: {

  imports = [
    ../../modules/ipfs.nix
    ../../modules/localserver.nix
    ../../modules/keylayout.nix
    ../../modules/task.nix
    ../../modules/emacs
    ../../modules/audioclient.nix
  ];

  environment.systemPackages = with pkgs; [
    #(idrisPackages.with-packages (with idrisPackages; [
    #  base
    #  contrib
    #  effects
    #  pruviloj

    #  lightyear
    #  #wl-pprint broken
    #  specdris
    #  #httpclient broken
    #  bi
    #]))
  ];

  networking.firewall.allowedTCPPorts = [ 8081 ];
}
