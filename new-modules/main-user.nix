{ options, config, lib, ... }:

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
      type = options.home-manager.users.type.functor.wrapped;
      default = {};
      description = "Home-manager configuration to be used for all main X users, this will in all cases exclude the root user";
    };

    userConfig = mkOption {
      type = options.home-manager.users.type.functor.wrapped;
      default = {};
      description = "Home-manager configuration to be used for all main users";
    };

  };

  config = {

    home-manager.users = mkMerge (flip map config.mine.mainUsers (user: {

      ${user} = mkMerge [
        (mkAliasDefinitions options.mine.userConfig)
        (mkIf (config.services.xserver.enable && user != "root")
          (mkAliasDefinitions options.mine.xUserConfig))
      ];

    }));

    assertions = map (user: {
      assertion = builtins.hasAttr user config.users.users;
      message = "The main user ${user} has to exist";
    }) config.mine.mainUsers;

  };
}
