{ lib, ... }:

with lib;

{

  options.mine.hasBattery = mkOption {
    type = types.bool;
    default = false;
    description = "Whether this machine has a battery";
  };

}
