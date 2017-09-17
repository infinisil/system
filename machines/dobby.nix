# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, fetchFromGitHub, ... }:
{
  imports = [
      /etc/nixos/hardware-configuration.nix
      ../modules/base.nix
      ../modules/ssh.nix
      ../modules/radicale.nix
      ../modules/bind.nix
      ../modules/console.nix
      ../modules/mpdServer.nix
      ../modules/mail.nix
      ../modules/namecoin.nix
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
      "nixpkgs=/root/nixpkgs"
      "nixos-config=/cfg/system/machines/dobby.nix"
      "modules=/cfg/system/modules"
    ];
  };

  networking = {
    hostId = "ecb69508";
    nameservers = [ "8.8.8.8" ];
    defaultGateway = "139.59.144.1";
    defaultGateway6 = "2a03:b0c0:3:d0::1";
    #usePredictableInterfaceNames = false;
    interfaces.eth0 = {
      ipAddress = "139.59.149.43";
      ipv6Address = "2a03:b0c0:3:d0::5df6:1";
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
      package = pkgs.nginxMainline;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;

      virtualHosts."www.infinisil.io".globalRedirect = "infinisil.io";
      virtualHosts."infinisil.io" = {
        forceSSL = true;
        enableACME = true;
        root = "/webroot/www";
        locations."/".extraConfig = "autoindex on;";
      };
      virtualHosts."mac.infinisil.io" = {
        locations."/" = {
          proxyPass = "http://localhost:81";
        };
      };
      virtualHosts."test.infinisil.io" = {
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
  };

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };

  system.stateVersion = "17.03";
}
