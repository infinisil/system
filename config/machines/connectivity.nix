let
  spec = rec {
    hosts = [
      "protos"
      "vario"
      "ninur"
    ];

    public = {
      protos = "104.248.129.84";
    };

    vpn = {
      protos = "10.149.76.1";
      vario = "10.149.76.2";
      ninur = "10.149.76.3";
    };

    local = {
      ninur.vario.ethernet = "192.168.178.28";
      vario.ninur.wireless = "192.168.178.21";
      vario.ninur.ethernet = "192.168.178.53";
    };

    connections = {
      ninur = {
        protos = public.protos;
        vario = vpn.vario;
        vario-e = local.ninur.vario.ethernet;
      };
      vario = {
        protos = public.protos;
        ninur = vpn.ninur;
        ninur-w = local.vario.ninur.wireless;
        ninur-e = local.vario.ninur.ethernet;
      };
      protos = {
        vario = vpn.vario;
        ninur = vpn.ninur;
      };
    };

  };

in

{ config, lib, ... }:

with lib;

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
