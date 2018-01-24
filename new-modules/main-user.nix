{ config, lib, ... }:

with lib;

{

  options.mine = {

    mainUsers = mkOption {
      type = types.listOf types.str;
      example = [ "paul" "john" ];
      default = [];
      description = "Main users for this system";
    };

    xUserConfig = mkOption {
      # Should be home-manager submodule
      type = types.unspecified;
      default = {};
      description = "Home-manager configuration to be used for all main X users";
    };

    userConfig = mkOption {
      # Should be home-manager submodule
      type = types.unspecified;
      default = {};
      description = "Home-manager configuration to be used for all main users";
    };

  };

  config = {

    home-manager.users = mkMerge (map (user: {

      "${user}" = mkMerge [
        config.mine.userConfig
        (mkIf (config.services.xserver.enable && user != "root")
          config.mine.xUserConfig)
      ];

    }) config.mine.mainUsers);

    assertions = map (user: {
      assertion = builtins.hasAttr user config.users.users;
      message = "The main user ${user} has to exist";
    }) config.mine.mainUsers;

  };

}
