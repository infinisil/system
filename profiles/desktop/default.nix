{ config, pkgs, nodes, ... }: {

  imports = [
    ../defaults.nix
    ../test.nix
    ../../modules/audio.nix
    ../../modules/x
    ../../modules/console.nix
    ../../modules/ipfs.nix
    ../../modules/localserver.nix
    ../../modules/keylayout.nix
    ../../modules/task.nix
    ../../modules/say.nix
    ../../modules/emacs.nix
    ../../modules/audioclient.nix
    ../../modules/sshclient.nix
    ../../modules/ssh.nix
  ];

  users.users.root.openssh.authorizedKeys.keys = [
    config.sshkeys.mac.nixos.root
    config.sshkeys.pc.root
  ];

  virtualisation.virtualbox.host.enable = true;

  environment.systemPackages = with pkgs; [
    texlive.combined.scheme-full
    nixops
    neofetch
    youtube-dl
    ffmpeg-full
    stack
    imagemagick7Big
    (idrisPackages.with-packages (with idrisPackages; [
      base
      contrib
      effects
      pruviloj

      lightyear
      #wl-pprint broken
      specdris
      #httpclient broken
      bi
    ]))
  ];

  environment.variables.PATH = "/cfg/bin";

  networking = {
    nameservers = [
      nodes.server.config.deployment.targetHost
    ];
    firewall.allowedTCPPorts = [ 8081 ];
  };

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    nixPath = [
      "cfg=/cfg"
      "nixpkgs=/cfg/nixpkgs"
      "nixos-config=/cfg/hosts/mac"
    ];
  };

  services.dbus.socketActivated = true;

  services.usbmuxd.enable = true;

  boot.supportedFilesystems = [ "exfat" "ntfs" ];
}
