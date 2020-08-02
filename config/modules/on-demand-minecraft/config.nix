{ lib, config, pkgs, modulesPath, ... }: {

  imports = [
    (modulesPath + "/virtualisation/digital-ocean-image.nix")
  ];

  virtualisation.digitalOceanImage.compressionMethod = "bzip2";
  virtualisation.digitalOceanImage.channelNixpkgs = null;

  nixpkgs.overlays = [(self: super: {
    zfs = super.zfs.override { enablePython = false; };
  })];

  i18n.supportedLocales = [ (config.i18n.defaultLocale + "/UTF-8") ];

  documentation.enable = lib.mkDefault false;

  documentation.nixos.enable = lib.mkDefault false;

  system.nixos.revision = "unknown";

  xdg.mime.enable = false;

  security.polkit.enable = false;

  services.udisks2.enable = false;

  services.zfs.zed.settings.ZED_EMAIL_PROG = "mail";

  networking.hostId = "8e4b0e65";

  fileSystems."/var/lib/minecraft" = {
    device = "minecraft2";
    fsType = "zfs";
  };

  systemd.services.self-destruct = {
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.iproute pkgs.curl ];
    script = ''
      count=0
      while [ $count -lt 5 ]; do
        sleep 60
        if [ -z "$(ss -tOH 'sport = 25565')" ]; then
          count=$(( count + 1 ))
          echo "Incrementing count to $count"
        else
          count=0
        fi
      done
      echo "Initiating self-destruct"

      systemctl stop minecraft-server
      /run/booted-system/sw/bin/zpool export minecraft2
      id=$(curl http://169.254.169.254/metadata/v1/id)
      curl -X DELETE -H "Content-Type: application/json" -H "Authorization: Bearer ${lib.fileContents ../../../external/private/doauth}" "https://api.digitalocean.com/v2/droplets/$id"
    '';
  };

  nixpkgs.config.allowUnfree = true;

  users.users.minecraft.uid = 114;

  services.minecraft-server = {
    enable = true;
    eula = true;
    openFirewall = true;
    package = pkgs.minecraft-server.overrideAttrs (old: {
      src = pkgs.fetchurl {
        url = "https://launcher.mojang.com/v1/objects/a412fd69db1f81db3f511c1463fd304675244077/server.jar";
        sha256 = "0nwkdig6yw4cnm2ld78z4j4xzhbm1rwv55vfxz0gzhsbf93xb0i7";
      };
    });
    jvmOpts = lib.concatStringsSep " " [
      "-Xms7G"
      "-Xmx7G"
    ];
  };

  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHjY4cuUk4IWgBgnEJSULkIHO+njUmIFP+WSWy7IobBs infinisil@vario"
  ];

  system.stateVersion = "20.03";
}
