{ config, lib, ... }:

with lib;

let

  cfg = config.mine.hardware;

in

{

  options.mine.hardware = {

    battery = mkOption {
      type = types.bool;
      default = false;
      description = "Whether this machine has a battery";
    };

    cpuCount = mkOption {
      type = types.int;
      description = "Number of CPUs";
    };

    swap = mkOption {
      type = types.bool;
      description = "Has swap";
    };

    touchpad = mkOption {
      type = types.bool;
      default = false;
      description = "Has touchpad";
    };

    wlan = mkOption {
      type = types.bool;
      default = false;
      description = "Has WLAN";
    };

    audio = mkOption {
      type = types.bool;
      default = false;
      description = "Has audio";
    };

  };

}
