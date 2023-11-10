{
  rtcwake.vario = {
    serverNode = "protos";
    host = "10.99.3.1";
    interval = 4 * 60 * 60;
    sshKeyPairFiles = ../../external/private/rtcwake;
  };

  ssh.access.zion.keys.infinisil.hasAccessTo.protos.rtcwake = true;
}
