{ pkgs, config, lib, ... }: {

  options.mine.matrix.enable = lib.mkEnableOption "matrix config";

  config = lib.mkIf config.mine.matrix.enable {

    mine.userConfig = {

      home.packages = [
        pkgs.fractal
      ];

      systemd.user.services.fractal = {

        Unit = {

          Description = "Fractal autostart";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Install.WantedBy = [ "graphical-session.target" ];

        Service = {
          ExecStart = "${pkgs.fractal}/bin/fractal";
          Restart = "always";
        };
      };
    };
  };

}
