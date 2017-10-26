# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, fetchFromGitHub, ... }:

with lib;

let

  domain = config.networking.domain;

in

{
  imports = [
      ./hardware.nix
      ./info.nix
      ./youtube.nix
      ../../private
      <cfg/modules/base.nix>
      <cfg/modules/ssh.nix>
      <cfg/modules/radicale.nix>
      <cfg/modules/bind.nix>
      <cfg/modules/console.nix>
      <cfg/modules/mpdServer.nix>
      <cfg/modules/mail.nix>
      <cfg/modules/namecoin.nix>
    ];

  environment.systemPackages = with pkgs; [
    git
    youtube-dl
    coreutils
    neovim
    tmux
    iperf
    bind
    lsof
  ];

  security.sudo.wheelNeedsPassword = false;

  boot = {
    loader.timeout = 60;
    loader.grub = {
      enable = true;
      device = "/dev/vda";
      splashImage = null;
    };
    zfs.devNodes = "/dev";
    cleanTmpDir = true;
    kernelParams = [ "net.ifnames=0" ];
  };

  nix = {
    trustedUsers = [ "root" "@wheel" ];
    autoOptimiseStore = true;
    nixPath = [
      # Ruin the config so we don't accidentally run
      # nixos-rebuild switch on the host (thanks grahamc!)
      "nixos-config=${pkgs.writeText "throw-configuration.nix" ''
        throw "Hey dummy, you're on your server! Use NixOps!"
      ''}"
      "nixpkgs=/run/current-system/nixpkgs"
    ];
  };

  system.extraSystemBuilderCmds = ''
    ln -sv ${lib.cleanSource pkgs.path} $out/nixpkgs
  '';

  networking = {
    hostName = "dobby";
    hostId = "ecb69508";
    domain = "infinisil.com";
    nameservers = [ "8.8.8.8" ];
    defaultGateway = "139.59.144.1";
    #defaultGateway6 = "2a03:b0c0:3:d0::1";
    #usePredictableInterfaceNames = false;
    interfaces.eth0 = {
      ipAddress = "139.59.149.43";
      #  ipv6Address = "2a03:b0c0:3:d0::5df6:1";
      prefixLength = 20;
    };
    firewall = {
      allowedTCPPorts = [
        22 # SSH
        80 # HTTP
        443 # HTTPS
        5001 # iperf
        5201 # ??
        2022 # ??
        5232 # Radicale
        3128 # ??
      ];
      logRefusedConnections = false;
    };
  };

  services = {
    openssh = {
      enable = true;
      passwordAuthentication = false;
    };

    nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedProxySettings = true;

      virtualHosts."www.${domain}".globalRedirect = domain;
      virtualHosts."${domain}" = {
        forceSSL = true;
        enableACME = true;
        root = "/webroot/www";
        locations."/pwp".extraConfig = "autoindex on;";
      };
      virtualHosts."mac.${domain}" = {
        forceSSL = true;
        enableACME = true;
        root = "/webroot/mac";
        locations."/" = {
          proxyPass = "http://localhost:1808";
        };
      };
      virtualHosts."test.${domain}" = {
        forceSSL = true;
        enableACME = true;
        root = "/webroot/test";
        locations."/".extraConfig = "autoindex on;";
      };
    };

    znapzend = {
      enable = true;
      autoCreation = true;
    };

    ipfs = {
      enable = true;
      dataDir = "/var/lib/ipfs";
    };

    znc = {
      enable = true;
      openFirewall = true;
      mutable = false;
      confOptions = {
        nick = "infinisil";
        userName = "infinisil";
        extraUserConf = ''
          AutoClearChanBuffer = false
          AutoClearQueryBuffer = false
        '';
        networks.freenode = {
          userName = "infinisil";
          server = "chat.freenode.net";
          modules = [
            "sasl"
            "log"
            "savebuff ${config.passwords.znc-savebuff}"
          ];
          channels = [
            "nixos"
            "nixos-dev"
            "nixos-wiki"
            "emacs"
            "linux"
            "anime"
            "idris"
            "xmonad"
            "beets"
            "znc"
            "purism"
            "youtube-dl"
            "bash"
            "mpd"
            "git"
            "gpg"
            "crypto"
            "ffmpeg"
            "zfsonlinux"
            "weechat"
            "deluge"
            "ipfs"
          ];
        };
      };
    };
  };

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };

  system.stateVersion = "17.03";
}
