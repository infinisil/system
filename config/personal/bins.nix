{ pkgs, ... }: {

  mine.binalias = {
    sc = "sudo ${pkgs.systemd}/bin/systemctl";
    scu = "${pkgs.systemd}/bin/systemctl --user";
    jc = "${pkgs.systemd}/bin/journalctl";
    jcu = "${pkgs.systemd}/bin/journalctl --user";
  };

}
