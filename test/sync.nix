{ pkgs, ...}:
{
  systemd.timers.sync = {
    partOf = [ "sync.service" ];
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = "minutely";
  };
  systemd.services.sync = {
    enable = false;
    path = with pkgs; [ zfs openssh ];
    script = builtins.readFile ../bin/sync.sh;
  };
  systemd.services.fatracesync = {
    enable = false;
    path = with pkgs; [ fatrace zfs openssh expect ];
    script = ''
      cd /global
      unbuffer fatrace -c -f W | while read -r line; do systemctl start sync; done
    '';
    wantedBy = [ "multi-user.target" ];
  };
}
