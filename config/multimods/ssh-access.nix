{
  ssh.access.vario = {
    hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKST1SmdSyr88e6sha4avm7/3LRDr8ZgIl6Sn7ARenZM";
    hostNames = [
      "10.99.2.2"
      "10.99.3.2"
      "192.168.178.53"
    ];
    users.infinisil = {
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHjY4cuUk4IWgBgnEJSULkIHO+njUmIFP+WSWy7IobBs infinisil@vario";
      hasAccessTo = {
        vario.infinisil = true;
        protos.infinisil = true;
        protos.git = true;
        orakel.infinisil = true;
        protos.root = true;
        orakel.root = true;
        vario.root = true;
      };
    };
    users.root = {
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHgAYXd/tYlniYlz2TlfRUmZ+sxkRe8g7YGgPp7fADG+ root@vario";
      hasAccessTo = {
        orakel.root = true;
      };
    };
  };

  ssh.access.protos = {
    hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHhT5zE+T+Lp/nsvjSiWZqDLMRt+z8MQYQgTg2273FlU";
    hostNames = [
      "206.81.23.189"
      "10.99.3.1"
    ];
    users.infinisil.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDxTjdV4VVoPZiJ/dql5ttABrsNDPNWWFi0sSKGA3rJP infinisil@protos";
    users.root.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ6LOZBtnicSdL12eFDEVmiqBu7++qerEQvQ90qi+BMP root@protos";
    users.root.hasAccessTo = {
      vario.root = true;
    };
  };

  ssh.access.orakel = {
    hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH40I0FtZBAdqgiOvygJGwyIEPteTqSJjrhOQPgKaZkB";
    hostNames = [
      "51.15.187.150"
      "10.99.2.1"
    ];
    users.infinisil.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMfXy5p2YiuHMyqgsyhv0qJL7/uA3TL72yvTq49C+V0 infinisil@orakel";
    users.root.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN7ZoWP2pfikvwyxQYmDtU0h2luN6JnsKVKStepQP+SV root@orakel";
    users.root.hasAccessTo = {
      vario.root = true;
    };
  };


  ssh.access.phone.users.me = {
    publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC+6hf5RgtmV/jve2GICAk8dPTPbyjIkrXs9hwKZwg54NHCqwWor5ssK1QBFTTAMGXRUbtUz6/u+fC3GI6rE2J6c60z5uG6g/uXdSf1uCbIT2gVTRTCkNaA4lFKon7z1cFg7kBlZJkAVmDruLtllhaC9BSHjMCTpUQBGCqHOJp4luCyGPmxY7ahxPF2jItU2eqAxsnK9sSSLJNHY4O2kg/JV7Er1Y0jwYpx54rLqaYdWoTbtUQqUotRDGp1S/UnEpHl0OTAAF3bU8GeWrUHI7LP9QBGhGHeQefL5iQY7FKrp2VSCYuPljsl+Tnabv4+IncKt+zfg/hJVplbbZK0BdH/G6wMZAsSrXLCi3P0sY6BsdBZICHCxaPC6sNlAUQ6QmcBf4xrUEXNvLEAWhDXqKpdEc6UAK6Y3u2g4jlkov42ex6DwpuGff17sUF2E84WU9QPk/A9XAA8VkKeOF/RXV+Ao3TbPmYsxblyXnlyx0laE1NRpdCZ3pltp1gvFdHxleokUD4E6sgKOOwZlzLJTbqnPalCmHYBu3unDaADSJv55MzC97Ws0JJZGmFJ5nxsk8p7zM062tIJh9c6xxZxKHqi3IZMZLmIj96W3DGruMV/ybT+PQKjjto+7oGZX+Cz1CVYDTl09MAgBj2/2pbDYRbVwXyf4qiQsvPu4/1y7BDOHQ== phone";
    hasAccessTo = {
      protos.git = true;
    };
  };

  ssh.access.mac.users.silvan = {
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGsGWjqENARDU/Jh7uToFtZwGlJwFM6cqq9ErLHZkdw4 silvan@mac";
    hasAccessTo = {
      protos.infinisil = true;
    };
  };
}
