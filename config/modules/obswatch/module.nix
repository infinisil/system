{ pkgs, config, lib, ... }: {

  options.obswatch.enable = lib.mkEnableOption "obswatch";

  config = lib.mkIf config.obswatch.enable {
    nixpkgs.overlays = [
      (self: super: {
        obswatch = self.callPackage ./package.nix { };
      })
    ];

    systemd.user.services.obswatch = {
      script = ''
        fifoPath=''${XDG_RUNTIME_DIR}/obs
        if [[ ! -e "$fifoPath" ]]; then
          mkfifo "$fifoPath"
        fi
        OBS_PASSWORD=$(<${config.secrets.files.obs.file}) ${pkgs.obswatch}/bin/obswatch watch "$fifoPath"
      '';
      wantedBy = [ "default.target" ];

      serviceConfig = {
        Restart = "on-failure";
      };
    };

  };
}
