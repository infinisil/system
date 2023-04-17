{

  nodes.zion.configuration.networking.firewall.allowedTCPPorts = [ 8888 ];
  nodes.vario.configuration.networking.firewall.allowedTCPPorts = [ 8888 ];

  zrepl.setups = {
    vario-backups = {
      mode = "push";
      from = {
        node = "vario";
        ip = "192.168.0.12";
        fileSystems."tank2/root/data<" = true;
        sendOptions.raw = true;
        snapshotInterval = "1m";
        pruning.lastN = null;
        pruning.grid = "1x15m(keep=all) | 24x1h | 10x1d";
      };
      to.zion = {
        ip = "192.168.0.17";
        port = 8888;
        rootFileSystem = "pool/backup";
        syncInterval = "1h";
        pruning.lastN = null;
        pruning.grid = "7x1d | 5x1w | 12x5w";
        placeholder.encryption = "off";
      };
    };

    zion-backups = {
      mode = "push";
      from = {
        node = "zion";
        ip = "192.168.0.17";
        fileSystems."pool/encroot/data<" = true;
        sendOptions.encrypted = true;
        snapshotInterval = "1m";
        pruning.lastN = null;
        pruning.grid = "1x15m(keep=all) | 24x1h | 10x1d";
      };
      to.vario = {
        ip = "192.168.0.12";
        port = 8888;
        rootFileSystem = "main/backup-server";
        syncInterval = "1h";
        pruning.lastN = null;
        pruning.grid = "7x1d | 5x1w | 12x5w";
        placeholder.encryption = "off";
      };
    };

    protos-backups = {
      mode = "pull";
      from = {
        node = "protos";
        ip = "10.99.3.1";
        port = 8888;
        fileSystems."tank/root/data<" = true;
        sendOptions.raw = true;
        snapshotInterval = null;
        pruning.lastN = null;
        pruning.grid = null;
      };
      to.vario = {
        ip = "10.99.3.2";
        rootFileSystem = "main/backup";
        syncInterval = "1h";
        pruning.lastN = null;
        pruning.grid = "7x1d | 5x1w | 12x5w";
        placeholder.encryption = "off";
      };
    };
  };

}
