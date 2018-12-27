{ config, lib, ... }:

with lib;

let

  spec = import ../../deploy/connectivity.nix;

in

{
  options.networking = {
    connections = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = "Mapping from hostname (plus optional alt versions) to preferred route to that host";
    };

    connectivitySpec = mkOption {
      type = types.attrs;
      default = spec;
      readOnly = true;
      description = "Connectivity specification";
    };
  };

  config.networking.connections = spec.connections.${config.networking.hostName};

}
