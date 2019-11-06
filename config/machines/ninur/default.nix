{ config, pkgs, lib, ... }: {

  imports = [
    ./boot.nix
    ./hardware-configuration.nix
    ./lidWakeDisable.nix
  ];

  mine.enableUser = true;

  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';
  mine.compton.enable = true;

  hardware.cpu.intel.updateMicrocode = true;

  # Seems to cause a bunch of problems
  # boot.kernelParams = [ "acpi_osi=" ];
  boot.extraModprobeConfig = ''
    options hid_apple fnmode=2
  '';

  services.tlp.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };

  mine.hardware = {
    battery = true;
    cpuCount = 4;
    swap = true;
    touchpad = true;
    wlan = true;
    audio = true;
  };

  services.xserver.videoDrivers = [ "intel" ];

  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
  ];

  environment.systemPackages = with pkgs; [
    #steam
    efibootmgr
    acpi
    lm_sensors
  ];

  services.mbpfan.enable = true;


  hardware.opengl.driSupport32Bit = true;

  mine.sshMounts = lib.mapAttrs (name: value: {
    host = "infinisil@${value}:/home/infinisil";
    identity = "/home/infinisil/.ssh/id_rsa";
  }) config.networking.connections // {
    betty = {
      host = "infinisil@${config.networking.connections.vario}:/betty";
      identity = "/home/infinisil/.ssh/id_rsa";
    };
  };

  services.openvpn.servers.orakel = {
    mine.type = "client";
    mine.client.serverIp = config.networking.connections.orakel;
  };

  #mine.gaming.enable = true;

  #services.ipfs.enable = true;

  services.znapzend = {
    enable = true;
    autoCreation = true;
    pure = true;
    zetup."tank/root/data" = rec {
      plan = "1h=>5min,1d=>1h,1w=>1d";
      recursive = true;
      destinations.backup = {
        host = config.networking.connectivitySpec.local.ninur.vario.ethernet;
        dataset = "main/backup/ninur";
        plan = "1d=>1h,1m=>1d,1y=>1m";
      };
    };
  };

  mine.string-transfer.enable = true;

  mine.profiles.desktop.enable = true;

  #mine.dev.rust.enable = true;

  hardware.opengl.enable = true;

  services.nginx = {
    enable = true;
    virtualHosts.localhost = {
      root = "/webroot";
      listen = [ { port = 80; addr = "0.0.0.0"; } ];
    };
  };

  networking = {
    hostName = "ninur";
    hostId = "34cc680d";
    firewall.allowedTCPPorts = [ 80 1500 1501 ];
  };
}
