{ lib, config, ... }: {

  options.mine.enableUser = lib.mkEnableOption "infinisil user";

  config = lib.mkIf config.mine.enableUser {

    users.extraUsers.infinisil = {
      description = "Silvan Mosberger";
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "systemd-journal"
      ];
    };

    mine.mainUsers = [ "infinisil" ];

  };
}
