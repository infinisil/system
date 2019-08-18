{ lib, config, ... }: {

  options.mine.enableUser = lib.mkEnableOption "infinisil user";

  config = lib.mkIf config.mine.enableUser {

    users.extraUsers.infinisil = {
      uid = 1000;
      description = "Silvan Mosberger";
      isNormalUser = true;
      createHome = true;
      extraGroups = [
        "wheel"
        "systemd-journal"
      ];
    };

    mine.mainUsers = [ "infinisil" ];

  };
}
