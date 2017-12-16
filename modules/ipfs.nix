{ config, lib, ... }:

with lib;

{
  options.services.ipfs.autostart = mkOption {
    type = types.bool;
    default = false;
    description = "Automatically start IPFS on startup";
  };

  config = mkMerge [
    {
      services.ipfs = {
        enable = true;
        autoMount = true;
        dataDir = "/var/lib/ipfs";
      };

      users.users.infinisil.extraGroups = [
        "ipfs"
      ];
    }
    (mkIf (! config.services.ipfs.autostart) {
      systemd.services.ipfs.wantedBy = mkForce [];
    })
  ];

}
