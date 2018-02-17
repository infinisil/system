{ dirty }:

{
  network.description = "Infinisil's machines";

  defaults = { pkgs, lib, ... }: {
    deployment.alwaysActivate = true;

    system.nixosLabel = let cfg = ../.; in builtins.readFile (pkgs.runCommand "cfgVersion" {} ''
      label=$(printf "${lib.optionalString dirty "%.7s-dirty-"}%.35s" \
        ${lib.optionalString dirty ''"$(${pkgs.nix}/bin/nix-hash ${cfg} --base32)"''} \
        "$(${pkgs.git}/bin/git -C ${cfg} log --pretty=format:'%h-%f' -n 1)")
      echo Generated nixosLabel: "$label"
      echo -n $label > $out
    '');

    imports = [
      ../personal
      ../new-modules
      ../pkgs
      ../profiles/defaults.nix
    ];
  };

  server = { nodes, config, ... }: {
    deployment.targetHost = "139.59.149.43";

    imports = [
      ../hardware/paul
      ../profiles/server
      ../profiles/weak.nix
    ];

    networking = {
      hostName = "paul";
      hostId = "ecb69508";
      domain = "infinisil.com";
      nameservers = [ "8.8.8.8" ];
      defaultGateway = "139.59.144.1";
      #defaultGateway6 = "2a03:b0c0:3:d0::1";
      interfaces.eth0 = {
        ipAddress = config.deployment.targetHost;
        #ipv6Address = "2a03:b0c0:3:d0::5df6:1";
        prefixLength = 20;
      };
    };
  };

  laptop = { nodes, config, lib, ... }: {
    deployment.targetHost = "localhost";

    imports = [
      ../hardware/mac
      ../profiles/desktop
      ../profiles/weak.nix
    ];

    services.nfs.server = {
      enable = false;
      exports = ''
        /home/infinisil 192.168.1.25(rw)
      '';
    };

    localserver = {
      webserverport = 1801;
      sshport = 2221;
    };

    networking = {
      hostName = "emma";
      hostId = "34cc680d";
      extraHosts = ''
        ${nodes.server.config.deployment.targetHost} ${nodes.server.config.networking.domain} ${nodes.server.config.networking.hostName}
      '';
      firewall.allowedTCPPorts = [ 111 2049 1500 ];
      firewall.allowedUDPPorts = [ 111 2049 ];
    };
  };

  pc = { pkgs, nodes, config, lib, ... }: {
    deployment.targetHost = "pc";

    imports = [
      ../hardware/pc
      ../profiles/desktop
      ../modules/remote.nix
    ];

    #fileSystems."/mnt/home" = {
    #  device = "192.168.1.19:/home/infinisil";
    #  fsType = "nfs";
    #};

    localserver = {
      webserverport = 1802;
      sshport = 2222;
    };

    services.nginx.virtualHosts."pc.${nodes.server.config.networking.domain}".locations."/betty" = {
      root = "/";
      extraConfig = ''
        autoindex on;
      '';
    };

    environment.systemPackages = with pkgs; [
      steam
    ];

    networking = {
      hostName = "nepnep";
      hostId = "56236562";
      extraHosts = ''
        ${nodes.server.config.deployment.targetHost} ${nodes.server.config.networking.domain} ${nodes.server.config.networking.hostName}
      '';
    };
  };
}
