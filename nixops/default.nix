{
  network.description = "Infinisil's machines";

  defaults = {
    deployment.alwaysActivate = true;

    imports = [
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
    };
  };

  pc = { pkgs, nodes, config, lib, ... }: {
    deployment.targetHost = "infinisil.com";
    deployment.targetPort = 2222;

    imports = [
      ../hardware/pc
      ../profiles/desktop
      ../modules/remote.nix
    ];

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
