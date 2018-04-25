{ config, lib, ... }:

with lib;

{

  options.mine = {

    sshkeys = mkOption {
      type = types.attrs;
      default = {};
      description = "SSH keys in an attrset";
    };

    allsshkeys = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "All SSH keys";
    };

  };

  config.mine.allsshkeys = mkDefault
    (collect (x: !builtins.isAttrs x) config.mine.sshkeys);

}
