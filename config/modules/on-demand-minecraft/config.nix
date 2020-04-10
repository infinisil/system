{ lib, config, pkgs, modulesPath, ... }: {

  imports = [
    (modulesPath + "/virtualisation/digital-ocean-image.nix")
  ];

  virtualisation.digitalOceanImage.compressionMethod = "bzip2";
  virtualisation.digitalOceanImage.channelNixpkgs = null;

  nixpkgs.overlays = [(self: super: {
    zfs = super.zfs.override { enablePython = false; };
  })];

  i18n.supportedLocales = [ (config.i18n.defaultLocale + "/UTF-8") ];

  documentation.enable = lib.mkDefault false;

  documentation.nixos.enable = lib.mkDefault false;

  system.nixos.revision = "unknown";

  xdg.mime.enable = false;

  security.polkit.enable = false;

  services.udisks2.enable = false;

  services.zfs.zed.settings.ZED_EMAIL_PROG = "mail";

  networking.hostId = "8e4b0e65";

  fileSystems."/var/lib/minecraft" = {
    device = "minecraft";
    fsType = "zfs";
  };

  systemd.services.self-destruct = {
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.iproute pkgs.curl ];
    script = ''
      count=0
      while [ $count -lt 5 ]; do
        sleep 60
        if [ -z "$(ss -tOH 'sport = 25565')" ]; then
          count=$(( count + 1 ))
          echo "Incrementing count to $count"
        else
          count=0
        fi
      done
      echo "Initiating self-destruct"

      systemctl stop minecraft-server
      /run/booted-system/sw/bin/zpool export minecraft
      id=$(curl http://169.254.169.254/metadata/v1/id)
      curl -X DELETE -H "Content-Type: application/json" -H "Authorization: Bearer ${lib.fileContents ../../../external/private/doauth}" "https://api.digitalocean.com/v2/droplets/$id"
    '';
  };

  nixpkgs.config.allowUnfree = true;

  users.users.minecraft.uid = 114;

  services.minecraft-server = {
    enable = true;
    package = pkgs.minecraft-server.overrideAttrs (old: {
      src = pkgs.fetchurl {
        url = "https://launcher.mojang.com/v1/objects/bb2b6b1aefcd70dfd1892149ac3a215f6c636b07/server.jar";
        sha256 = "12kynrpxgcdg8x12wcvwkxka0fxgm5siqg8qq0nnmv0443f8dkw0";
      };
    });
    eula = true;
    openFirewall = true;
    jvmOpts = lib.concatStringsSep " " [
      "-Xms3G"
      "-Xmx3G"
      "-XX:+UseConcMarkSweepGC"
      "-XX:+UseParNewGC"
    ];
  };

  systemd.services.minecraft-server.serviceConfig.ExecStart = lib.mkForce [
    (pkgs.writeShellScript "minecraft-start" ''
      if [ ! -p socket ]; then
        mkfifo socket
      fi
      ${pkgs.nmap}/bin/ncat --ssl chat.freenode.net 6697 -e ${pkgs.writeShellScript "script" ''
        channel=##nixos-anime

        trap exit INT TERM
        trap '${pkgs.procps}/bin/ps -s $$ -o pid= | xargs -r -n1 kill' EXIT

        cat <<EOF
        USER bot x x :Silvan Mosberger
        NICK anixbot
        PRIVMSG NickServ :IDENTIFY ${lib.fileContents ../../../external/private/anixbotpw}
        JOIN $channel
        EOF

        {
          delay=3
          nickmap=$(mktemp -d)

          tail -f socket | \
            ${pkgs.gawk}/bin/gawk 'match($0, /\[.*?\] \[.*?]: (.*?) (joined|left) the game/, a) { print a[1], a[2]; fflush() }' | \
            while read nick status; do

              if [ ! -d "$nickmap/$nick" ]; then
                mkdir "$nickmap/$nick"
                echo left > "$nickmap/$nick/status"
              fi

              update=$(date +%s.%N)
              echo "$update" > "$nickmap/$nick/update"

              {
                sleep $((delay * 60))
                lastupdate=$(cat "$nickmap/$nick/update")
                laststatus=$(cat "$nickmap/$nick/status")
                if [ "$lastupdate" = "$update" ] && [ "$laststatus" != "$status" ]; then
                  echo -e "PRIVMSG $channel :$nick $status the game ($delay minutes ago)"
                  echo "$status" > "$nickmap/$nick/status"
                fi
              } &

            done
        } &

        while read command args; do
          if [ "$command" = "PING" ]; then
            echo "PONG x"
          fi
          echo -e "$command $args" >&2
        done
      ''} &
      ${config.services.minecraft-server.package}/bin/minecraft-server ${config.services.minecraft-server.jvmOpts} | while read line; do
        echo -e "$line"
        echo -e "$line" > socket
      done
    '')
  ];

  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHjY4cuUk4IWgBgnEJSULkIHO+njUmIFP+WSWy7IobBs infinisil@vario"
  ];

  system.stateVersion = "20.03";
}
