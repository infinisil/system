{ nodes, lib, config, pkgs, ... }:
let
  hippo = pkgs.writeShellScriptBin "cinema" ''
    ${pkgs.xorg.xrandr}/bin/xrandr \
      --output HDMI-0 --mode 3840x2160 --rate 23.98
  '';
  projector = pkgs.writeShellScriptBin "projector" ''
    ${pkgs.xorg.xrandr}/bin/xrandr \
      --output HDMI-0 --mode 1920x1080 --rate 119.88
  '';
in {

  imports = [
    ./hardware-configuration.nix
    ../../personal/key-layout.nix
  ];

  services.transmission = {
    enable = true;
    openFirewall = true;
    settings = {
      incomplete-dir-enabled = false;
    };
  };

  mine.terminal.enable = true;
  mine.diskSupport.enable = true;
  mine.firefox.enable = true;

  hardware.cpu.intel.updateMicrocode = true;

  mine.localMusic.enable = true;

  nix.settings.experimental-features = [ "flakes" "nix-command" ];

  boot.kernelModules = [
    # Needed for disks..
    "sg"
  ];

  mine.userConfig.programs.ssh = {
    enable = true;
    controlMaster = "auto";
    controlPersist = "60";
    matchBlocks =
      lib.mapAttrs (name: value: { hostname = value.networking.public.ipv4; })
      (lib.filterAttrs (name: value: value.networking ? public.hasIpv4 && value.networking.public.hasIpv4) nodes);
  };

  mine.japaneseInput = true;

  environment.autoUpdate.enable = true;
  environment.autoUpdate.presets.yt-dlp = true;

  # Remove fs-before.target
  systemd.services.zfs-import-main.before = lib.mkForce [
    "betty.mount"
    "home-infinisil-music.mount"
    "home-infinisil-torrent.mount"
  ];
  systemd.targets.zfs-import.after = lib.mkForce [];
  systemd.services.systemd-udev-settle.serviceConfig.ExecStart = [ "" "${pkgs.coreutils}/bin/true" ];

  mine.enableUser = true;

  mine.saveSpace = true;

  mine.hardware = {
    swap = true;
    cpuCount = 8;
    audio = true;
  };

  users.users.infinisil.extraGroups = [ "transmission" "plugdev" ];
  users.groups.transmission.gid = 70;
  users.groups.plugdev = {};

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  mine.profiles.default.enable = true;
  mine.profiles.desktop.enable = true;

  mine.xmonad = {
    enable = true;
    users = [ "infinisil" ];
  };

  services.xserver.displayManager.lightdm.extraSeatDefaults = ''
    display-setup-script=${projector}/bin/projector
  '';
  services.xserver.serverFlagsSection = ''
    Option "StandbyTime" "120"
    Option "SuspendTime" "120"
    Option "OffTime" "120"
    Option "BlankTime" "120"
  '';

  # hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  boot = {
    loader = {
      grub = {
        enable = true;
        device = "nodev";
        efiSupport = true;
        gfxmodeEfi = "2560x1440";
      };
      efi.canTouchEfiVariables = true;
    };
    # https://discourse.nixos.org/t/browsers-unbearably-slow-after-update/9414/31
    kernelParams = [ "intel_pstate=active" ];
  };

  environment.systemPackages = with pkgs; [
    projector
    hippo
    element-desktop
    htop
    moreutils
  ];

  # Wingo router
  networking.extraHosts = ''
    192.168.0.254 winbox.local
  '';

  networking = {
    hostName = "vario";
    hostId = "56236562";
    firewall.allowedTCPPorts = [ 80 ];
    useDHCP = false;
    interfaces.eno1.useDHCP = true;
  };
}
