{ config, lib, ... }:

with lib;

{

  options.mine.htop.enable = mkOption {
    type = types.bool;
    default = true;
    description = "Enable htop config";
  };

  config = mkIf config.mine.htop.enable {

    mine.userConfig.programs.htop = {

      enable = true;
      fields = [
        "PID" "USER" "PRIORITY"
        "PERCENT_CPU" "M_RESIDENT" "PERCENT_MEM"
        "TIME" "COMM"
      ];
      colorScheme = 0;
      hideThreads = true;
      hideUserlandThreads = true;
      showProgramPath = false;
      highlightBaseName = true;

      meters = {
        left = mkMerge [
          [
            "Memory"
          ]
          (if (config.mine.hardware.cpuCount == 4) then [
            "LeftCPUs2"
            "RightCPUs2"
          ] else [
            "CPU"
          ])
          (mkIf config.mine.hardware.swap [
            "Swap"
          ])
          [
            { kind = "CPU"; mode = 3; }
          ]
        ];
        right = mkMerge [
          [
            { kind = "Clock"; mode = 4; }
            "Uptime"
            "Tasks"
            "LoadAverage"
          ]
          (mkIf config.mine.hardware.battery [
            { kind = "Battery"; mode = 1; }
          ])
        ];
      };

    };

  };

}
