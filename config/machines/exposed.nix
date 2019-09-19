/*
This module allows machines to expose values to other machines without having to
resort to nixops `nodes` argument which would slow down evaluation considerably.

A file under machines/<host>/exposed.nix will be able to set options
`machines.<host>.*`. This file will be called with an initial argument for the
host that's evaluating the option, allowing connection-specific option values.
*/
{ lib, config, ... }:
let
  inherit (lib) types;

  evalHost = config.networking.hostName;

  exposedModule = { name, ... }: {
    options.publicIp = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Public IP of the machine.";
    };

    options.openvpn.server = {
      subnet = lib.mkOption {
        type = types.nullOr (types.strMatching "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+/[0-9]+");
        example = "10.176.56.0/24";
        default = null;
        description = ''
          Subnet this server should use. This should be a private IP range,
          see section 3 of <link
          xlink:href="https://tools.ietf.org/html/rfc1918">RFC 1918</link>.
        '';
      };

      staticClientIps = lib.mkOption {
        type = types.attrsOf types.str;
        default = {};
      };
    };

    options.openvpnIps = lib.mkOption {
      type = types.attrsOf types.str;
    };

  };
in {

  options.machines = lib.mkOption {
    type = types.attrsOf (types.submodule exposedModule);
    readOnly = true;
  };

  config = {
    machines =
      let hosts = lib.attrNames (lib.filterAttrs (n: v: v == "directory") (builtins.readDir ./.));
      in lib.genAttrs hosts (host:
        let exposedFile = ./. + "/${host}/exposed.nix";
        in lib.optionalAttrs (builtins.pathExists exposedFile) (import exposedFile evalHost) // {
          openvpnIps = lib.mapAttrs (subnetName: value: value.${host})
            ((lib.filterAttrs (subnetName: value: value ? ${host} && value ? ${evalHost})) (lib.foldl' (a: n: a // { ${n} = if config.machines.${n}.openvpn.server.subnet == null then {} else config.machines.${n}.openvpn.server.staticClientIps; }) {} (lib.attrNames config.machines)));
        }
        );

    services.openvpn.servers = lib.mkMerge [
      (lib.mkIf (config.machines.${evalHost}.openvpn.server.subnet != null) {
        ${evalHost}.mine = {
          type = "server";
          server.subnet = config.machines.${evalHost}.openvpn.server.subnet;
          server.staticClientIps = lib.mapAttrs' (name: value: { name = "${evalHost}-${name}"; inherit value; }) config.machines.${evalHost}.openvpn.server.staticClientIps;
        };
      })
      (lib.mapAttrs (server: clientIp: {
        mine = {
          type = "client";
          client.serverIp = config.machines.${server}.publicIp;
        };
      }) config.machines.${evalHost}.openvpnIps)
    ];
  };
}

