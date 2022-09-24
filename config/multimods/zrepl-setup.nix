{

  zrepl.setups = {
    vario-backups = {
      mode = "push";
      from = {
        node = "vario";
        ip = "10.99.2.2";
        fileSystems."tank2/root/data<" = true;
        sendOptions.raw = true;
        snapshotInterval = "1m";
        pruning.lastN = null;
        pruning.grid = "1x15m(keep=all) | 24x1h | 10x1d";
      };
      to.orakel = {
        ip = "10.99.2.1";
        port = 8888;
        rootFileSystem = "tank/backup";
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
        ip = "10.99.3.4";
        fileSystems."pool/encroot/data<" = true;
        sendOptions.raw = true;
        snapshotInterval = "1m";
        pruning.lastN = null;
        pruning.grid = "1x15m(keep=all) | 24x1h | 10x1d";
      };
      to.vario = {
        ip = "10.99.3.2";
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

    orakel-backups = {
      mode = "pull";
      from = {
        node = "orakel";
        ip = "10.99.2.1";
        port = 8889;
        fileSystems."tank/root" = true;
        sendOptions.raw = true;
        snapshotInterval = null;
        pruning.lastN = null;
        pruning.grid = null;
      };
      to.vario = {
        ip = "10.99.2.2";
        rootFileSystem = "main/backup";
        syncInterval = "1h";
        pruning.lastN = null;
        pruning.grid = "7x1d";
        placeholder.encryption = "off";
      };
    };
  };

}
