# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, fetchFromGitHub, ... }:

{
  imports = [
      /etc/nixos/hardware-configuration.nix
      ./base.nix
      ./ssh-host.nix
      ./radicale.nix
      ./bind.nix
      ./console.nix
    ];
  
  environment.systemPackages = with pkgs; [
    git
    coreutils
    neovim
    tmux
    iperf
    bind
  ];

  security.sudo.wheelNeedsPassword = false;

  boot = {
    loader.grub = {
      enable = true;
      device = "/dev/vda";
    };
    zfs.devNodes = "/dev/";
    cleanTmpDir = true;
  };

  networking = {
    hostName = "dobby";
    hostId = "ecb69508";
    nameservers = [ "8.8.8.8" ];
    defaultGateway = "207.154.240.1";
    defaultGateway6 = "2a03:b0c0:3:d0::1";
    usePredictableInterfaceNames = false;
    interfaces.eth0 = {
      ipAddress = "207.154.251.58";
      ipv6Address = "2a03:b0c0:3:d0::47dc:3001";
      prefixLength = 20;
    };
    firewall = {
      allowedTCPPorts = [
        22 # SSH
        80 # HTTP
        443 # HTTPS
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
  };

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };

  system.stateVersion = "17.03";
}
