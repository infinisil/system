{ lib, config, ... }:
let
  # TODO: Make a module
  spec = with hosts; {
    vario = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKST1SmdSyr88e6sha4avm7/3LRDr8ZgIl6Sn7ARenZM";
      users.infinisil.key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHjY4cuUk4IWgBgnEJSULkIHO+njUmIFP+WSWy7IobBs infinisil@vario";
      users.infinisil.needsAccessTo = [
        ninur.infinisil
        protos.infinisil
        protos.git
        orakel.infinisil
        protos.root
        orakel.root
        vario.root
        ninur.root
      ];
      users.root.key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHgAYXd/tYlniYlz2TlfRUmZ+sxkRe8g7YGgPp7fADG+ root@vario";
      users.root.needsAccessTo = [
        orakel.root
      ];
    };
    ninur = {
      # linux
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICGE7lnVt1USFzRPkkMYsKN4sppGNDXy5CBx8Jc+2/ke";
      # macos
      #hostKey = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBF8xJWJ5CNOgMv5MDyU6cAyF0GqhOqOBaN28o49Y6jzsH2ZHP2bUfOfxtmiItS3LcsZiPJ2idYuNkpOq0yU9Z7E=";
      users.infinisil.key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHKudNIAd73BVC0G7mwL44UVysauUO+HemljoTTzbhnx infinisil@ninur";
      users.infinisil.needsAccessTo = [
        vario.infinisil
        protos.infinisil
        protos.git
        orakel.infinisil
      ];
      users.root.key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFyrwaLQfrv+GH3DezPKyJKM7UOXsKQ4A6vKb0D+ldH3 root@ninur";
      users.root.needsAccessTo = [
        vario.root
      ];
    };
    protos = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHhT5zE+T+Lp/nsvjSiWZqDLMRt+z8MQYQgTg2273FlU";
      users.infinisil.key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDxTjdV4VVoPZiJ/dql5ttABrsNDPNWWFi0sSKGA3rJP infinisil@protos";
      users.root.key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ6LOZBtnicSdL12eFDEVmiqBu7++qerEQvQ90qi+BMP root@protos";
      users.root.needsAccessTo = [
        vario.root
      ];
      users.git = {};
    };
    orakel = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH40I0FtZBAdqgiOvygJGwyIEPteTqSJjrhOQPgKaZkB";
      users.infinisil.key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMfXy5p2YiuHMyqgsyhv0qJL7/uA3TL72yvTq49C+V0 infinisil@orakel";
      users.root.key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN7ZoWP2pfikvwyxQYmDtU0h2luN6JnsKVKStepQP+SV root@orakel";
      users.root.needsAccessTo = [
        vario.root
        ninur.root
      ];
    };
    phone.users.me = {
      key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC+6hf5RgtmV/jve2GICAk8dPTPbyjIkrXs9hwKZwg54NHCqwWor5ssK1QBFTTAMGXRUbtUz6/u+fC3GI6rE2J6c60z5uG6g/uXdSf1uCbIT2gVTRTCkNaA4lFKon7z1cFg7kBlZJkAVmDruLtllhaC9BSHjMCTpUQBGCqHOJp4luCyGPmxY7ahxPF2jItU2eqAxsnK9sSSLJNHY4O2kg/JV7Er1Y0jwYpx54rLqaYdWoTbtUQqUotRDGp1S/UnEpHl0OTAAF3bU8GeWrUHI7LP9QBGhGHeQefL5iQY7FKrp2VSCYuPljsl+Tnabv4+IncKt+zfg/hJVplbbZK0BdH/G6wMZAsSrXLCi3P0sY6BsdBZICHCxaPC6sNlAUQ6QmcBf4xrUEXNvLEAWhDXqKpdEc6UAK6Y3u2g4jlkov42ex6DwpuGff17sUF2E84WU9QPk/A9XAA8VkKeOF/RXV+Ao3TbPmYsxblyXnlyx0laE1NRpdCZ3pltp1gvFdHxleokUD4E6sgKOOwZlzLJTbqnPalCmHYBu3unDaADSJv55MzC97Ws0JJZGmFJ5nxsk8p7zM062tIJh9c6xxZxKHqi3IZMZLmIj96W3DGruMV/ybT+PQKjjto+7oGZX+Cz1CVYDTl09MAgBj2/2pbDYRbVwXyf4qiQsvPu4/1y7BDOHQ== phone";
      needsAccessTo = [
        protos.git
      ];
    };
    mac.users.silvan = {
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGsGWjqENARDU/Jh7uToFtZwGlJwFM6cqq9ErLHZkdw4 silvan@mac";
      needsAccessTo = [
        protos.infinisil
      ];
    };
  };

  hosts = lib.mapAttrs (host: hostAttrs: lib.mapAttrs (user: attrs: {
    inherit host user;
  }) (hostAttrs.users or {})) spec;

  finalSpec = lib.mapAttrs (givingHost: givingHostAttrs: givingHostAttrs // { users = lib.mapAttrs (givingUser: y:
    let
      givingPair = { host = givingHost; user = givingUser; };
      givesAccessTo = lib.concatLists (lib.attrValues (lib.mapAttrs (needingHost: needingHostAttrs:
        lib.concatLists (lib.attrValues (lib.mapAttrs (needingUser: attrs:
          lib.optional (lib.elem givingPair (attrs.needsAccessTo or [])) {
            host = needingHost;
            user = needingUser;
          }
        ) (needingHostAttrs.users or {})))
      ) spec));
    in y // {
      inherit givesAccessTo;
      needsAccessTo = y.needsAccessTo or [];
    }
  ) (givingHostAttrs.users or {}); }) spec;

  keyFor = { host, user }: finalSpec.${host}.users.${user}.key or (throw "No key specified for user ${user} on host ${host}");

in {
  users.users = lib.mapAttrs (user: attrs: {
    openssh.authorizedKeys.keys = map keyFor attrs.givesAccessTo;
  }) finalSpec.${config.networking.hostName}.users;

  programs.ssh.knownHosts =
    let allHosts = lib.unique (map (x: x.host) (lib.concatMap (x: x.needsAccessTo) (lib.attrValues finalSpec.${config.networking.hostName}.users)));
    in lib.filter (x: x.publicKey != null) (map (host: {
      hostNames = config.networking.connectivitySpec.preferred.${config.networking.hostName}.${host};
      publicKey = finalSpec.${host}.hostKey or null;
    }) allHosts);

    mine.web.keys.set = lib.mapAttrs (host: hostAttrs: hostAttrs // {
      users = lib.mapAttrs (user: attrs: attrs.key) (lib.filterAttrs (user: attrs: attrs ? key) (hostAttrs.users));
    }) finalSpec;
}
