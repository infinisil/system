{ config, lib, pkgs, ... }:
let
  cfg = config.services.on-demand-minecraft;
  port = 25565;
in {

  options.services.on-demand-minecraft = {
    enable = lib.mkEnableOption "On-demand minecraft";
  };

  config = lib.mkIf cfg.enable {
    systemd.sockets.on-demand-minecraft = {
      wantedBy = [ "sockets.target" ];
      socketConfig.ListenStream = port;
    };

    systemd.services.on-demand-minecraft-monitor = {
      path = [ pkgs.iproute pkgs.curl ];
      serviceConfig.RuntimeDirectory = "on-demand-minecraft";
      script = ''
        id=$(cat "$RUNTIME_DIRECTORY/id")
        while curl -f -X GET -H "Content-Type: application/json" -H "Authorization: Bearer $(cat ${config.secrets.doauth.file})" "https://api.digitalocean.com/v2/droplets/$id"; do
          echo "Still active"
          sleep 60
        done
      '';
    };

    networking.firewall.allowedTCPPorts = [ port ];

    systemd.services.on-demand-minecraft = {
      bindsTo = [ "on-demand-minecraft-monitor.service" ];
      requiredBy = [ "on-demand-minecraft-monitor.service" ];
      before = [ "on-demand-minecraft-monitor.service" ];
      path = [ pkgs.jq pkgs.curl pkgs.nmap "/run/wrappers" ];
      preStart = ''
        id=$(curl -X POST \
            -H "Content-Type: application/json" \
            -H "Authorization: Bearer $(cat ${config.secrets.doauth.file})" \
            -d '{ "name":"minecraft",
                  "region":"fra1",
                  "size":"c-2",
                  "private_networking":true,
                  "image":59962904,
                  "ssh_keys":[25879389],
                  "volumes":"8b787688-52d2-11ea-9e33-0a58ac14d123"
                }' \
            "https://api.digitalocean.com/v2/droplets" \
          | jq '.droplet.id' -r)
        echo "$id" > "$RUNTIME_DIRECTORY/id"

        while info=$(curl -X GET -H "Content-Type: application/json" -H "Authorization: Bearer $(cat ${config.secrets.doauth.file})" "https://api.digitalocean.com/v2/droplets/$id") && [ $(jq ".droplet.status" -r <<< "$info") = new ]; do
          echo "Still not active"
          sleep 10
        done
        echo "$info"
        ip=$(jq '.droplet.networks.v4[] | select(.type == "private") | .ip_address' -r <<< "$info")
        echo "$ip" > "$RUNTIME_DIRECTORY/host"

        while ! ping "$ip" -c 1 -w 1; do
          sleep 1
        done

        while ! ncat "$ip" ${toString port} -c ""; do
          sleep 1
        done
      '';
      script = ''
        exec ${config.systemd.package.out}/lib/systemd/systemd-socket-proxyd "$(cat "$RUNTIME_DIRECTORY/host")":${toString port}
      '';
      serviceConfig.RuntimeDirectory = "on-demand-minecraft";
    };
  };

}
