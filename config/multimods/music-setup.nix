{
  music = {
    server.node = "protos";
    passwordFile = ../../external/private/mpd;
    server.musicDir = "/var/lib/music";
    server.domain = "tune.infinisil.com";
    client.nodes = [ "vario" "zion" ];
  };

  ssh.access.vario.keys.infinisil.hasAccessTo.protos.music = true;
  ssh.access.zion.keys.tweagysil.hasAccessTo.protos.music = true;
}
