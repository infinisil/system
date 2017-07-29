# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, fetchFromGitHub, ... }:

let

  pkgs2 = import <nixpkgs> { config = {}; };

  systemd-digitalocean = pkgs2.fetchFromGitHub {
    owner = "edef1c";
    repo = "systemd-digitalocean";
    rev = "8e232ce77ae04421a99be6787f1d185e49ebbc63";
    sha256 = "1635zqfj1yip5ixmakxy370ya1q30xg8wxzn9gn3623bba1s28sh";
  };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      #systemd-digitalocean.outPath
      ../console.nix
      ../users.nix
      ../ssh.nix
      ./radicale.nix
      ./bind.nix
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

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";
  boot.zfs.devNodes = "/dev/";

  networking = {
    hostId = "ecb69508";
    hostName = "dobby";
    nameservers = [ "8.8.8.8" ];
    defaultGateway = "207.154.240.1";
    defaultGateway6 = "2a03:b0c0:3:d0::1";
    usePredictableInterfaceNames = false;
    interfaces.eth0 = {
      ipAddress = "207.154.251.58";
      ipv6Address = "2a03:b0c0:3:d0::47dc:3001";
      prefixLength = 20;
    };
  };

  services.openssh.enable = true;  
  services.openssh.passwordAuthentication = false;

  networking.firewall.allowedTCPPorts = [22 80 443 5201 2022 5232 3128 ];
  networking.firewall.logRefusedConnections = false;
  services.nginx = {
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

  environment.variables.PATH = "/global/nixpkgs/result/bin";

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";
}
