{
  ssh.access.vario = {
    hostKeys.ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKST1SmdSyr88e6sha4avm7/3LRDr8ZgIl6Sn7ARenZM";
    hostNames = [
      "10.99.2.2"
      "10.99.3.2"
      "192.168.178.53"
    ];
    keys.infinisil = {
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHjY4cuUk4IWgBgnEJSULkIHO+njUmIFP+WSWy7IobBs infinisil@vario";
      hasAccessTo = {
        vario.infinisil = true;
        protos.infinisil = true;
        protos.git = true;
        orakel.infinisil = true;
        protos.root = true;
        orakel.root = true;
        vario.root = true;
        zion.root = true;
        zion.infinisil = true;
      };
    };
    keys.root = {
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHgAYXd/tYlniYlz2TlfRUmZ+sxkRe8g7YGgPp7fADG+ root@vario";
      hasAccessTo = {
        orakel.root = true;
      };
    };
  };

  ssh.access.protos = {
    hostKeys.ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHhT5zE+T+Lp/nsvjSiWZqDLMRt+z8MQYQgTg2273FlU";
    hostNames = [
      "206.81.23.189"
      "10.99.3.1"
    ];
    keys.infinisil.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDxTjdV4VVoPZiJ/dql5ttABrsNDPNWWFi0sSKGA3rJP infinisil@protos";
    keys.root.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ6LOZBtnicSdL12eFDEVmiqBu7++qerEQvQ90qi+BMP root@protos";
    keys.root.hasAccessTo = {
      vario.root = true;
    };
    keys.infinisil.hasAccessTo.vario.infinisil = true;
  };

  ssh.access.orakel = {
    hostKeys.ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH40I0FtZBAdqgiOvygJGwyIEPteTqSJjrhOQPgKaZkB";
    hostNames = [
      "51.15.187.150"
      "10.99.2.1"
    ];
    keys.infinisil.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMfXy5p2YiuHMyqgsyhv0qJL7/uA3TL72yvTq49C+V0 infinisil@orakel";
    keys.root.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN7ZoWP2pfikvwyxQYmDtU0h2luN6JnsKVKStepQP+SV root@orakel";
    keys.root.hasAccessTo = {
      vario.root = true;
    };
  };

  ssh.access.zion = {
    hostKeys.ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICD5VQnx4jNFo5zG3ytUSZr07+HlzsB9P63VdDbO56hZ";
    hostKeys.libreelec = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHeYHPkNEzBSL3S8q1catVqNwVD2yMn4IImvAaEVSjI3";
    hostNames = [
      "192.168.0.17"
    ];
    keys.infinisil.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP5yjcQ8Urk+uenJq96BopO/hnGow/hwiZtrM/+OBr8t infinisil@zion";
    keys.infinisil.hasAccessTo = {
      zion.root = true;
      protos.git = true;
      protos.root = true;
      orakel.root = true;
      vario.root = true;
      vario.infinisil = true;
    };
  };

  ssh.access.phone.keys.me = {
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIElZSK+qNC1Wt5R6YrJ799nJWjyxDLsV0UzyRt+fK/km phone";
    hasAccessTo = {
      protos.git = true;
    };
  };

  ssh.access.mac.keys.silvan = {
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGsGWjqENARDU/Jh7uToFtZwGlJwFM6cqq9ErLHZkdw4 silvan@mac";
    hasAccessTo = {
      protos.infinisil = true;
    };
  };
}
