{
  webserver =
    { config, pkgs, ... }: {
      deployment = {
        targetEnv = "virtualbox";
        virtualbox = {
          memorySize = 1024;
          vcpu = 2;
        };
      };
    };
}
