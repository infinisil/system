{

  imports = [
    ./boot.nix
    ../../modules/zfs.nix
    ./hardware-configuration.nix
  ];

  services.znapzend = {
    pure = true;
    zetup = {

      "main/data/var/important" = {
        plan = "1day=>1hour,1week=>1day,1month=>1week,1year=>1month";
        destinations.pc = {
          host = "pc";
          dataset = "main/server/important";
        };
      };

      "main/data/music" = rec {
        plan = "1d=>1h,1w=>1d";
        destinations.pc = {
          plan = plan + ",1m=>1w";
          host = "pc";
          dataset = "main/server/music";
        };
      };

    };
  };

}
