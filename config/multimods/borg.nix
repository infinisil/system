{ lib, ... }: {

  defaults.configuration = { config, pkgs, ... }: {
    options.mine.borg.enable = lib.mkEnableOption "borg";

    config = lib.mkIf config.mine.borg.enable {
      services.borgbackup.jobs.main = {
        paths = [ "/var/lib" "/home" "/root" ];
        exclude = [ "/var/lib/transmission/" "*/.cache/" "*/.direnv/" "/var/lib/docker/" "*/Downloads/" "*/noback/" ];
        repo =  "zh5536@zh5536.rsync.net:${config.networking.hostName}";
        encryption = {
          mode = "repokey-blake2";
          passCommand = "cat /root/borgbackup_key_passphrase";
        };
        extraArgs = [
          (
            assert config.services.borgbackup.package.version == "1.4.2";
            "--remote-path=borg14"
          )
        ];
        extraCreateArgs = [
          "--progress"
          "--stats"
        ];
        compression = "auto,zstd";
        startAt = "daily";
        inhibitsSleep = true;
        persistentTimer = true;
      };
    };
  };

  /*
  - Get an account via https://www.rsync.net/signup/order.html?code=experts
  - On each machine:
    - Make sure root has an SSH key and add it to the rsync host as per https://rsync.net/resources/howto/ssh_keys.html
    - pass generate vario@borgbackup -c
    - sudo borg init --remote-path=borg14 --encryption=repokey-blake2 zh5536@zh5536.rsync.net:$(hostname)
    - sudo borg key export --remote-path=borg14 zh5536@zh5536.rsync.net:$(hostname) /root/borgbackup_key
    - pass vario@borgbackup > /root/borgbackup_key_passphrase
  */
  #nodes.vario.configuration.mine.borg.enable = true;
  nodes.zion.configuration.mine.borg.enable = true;
  nodes.protos.configuration.mine.borg.enable = true;
  nodes.void.configuration.mine.borg.enable = true;
}
