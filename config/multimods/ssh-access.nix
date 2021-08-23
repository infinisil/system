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
        orakel.music = true;
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


  ssh.access.phone.keys.me = {
    publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCu+YVXYiN1beODUJTUvBhB/WoqM4sbHgdq279YONM8pz6EP2JQ4SQ/6O+mdg8jGxqft3eSz3h6YnSkaLAepnrUogwmhKb1pbOMenJcVoe7LjGNPKORy/rk5VcSBdRPQk0yUA7onm3T6l8VQGrefqRtMGcAJ2U1DmU5S1bxCd9NV0TiTb8qubH7y06YJFkfqRewIj3bOO97lPPC/+hkjGJlaj/kKviHfywylDP3rTxTYjvHKR/MbQN2MmTpKub1Y+BARL8OgGX5kgrljoYrVPD+pYzEUHkw1PSEgNPsVXqDLNSUdQBgMwUxxeONhNl4Inqt5PZVTjZnQg3MjjOVR78PNE0x7v+/PHo5qnwqjxg4IaBKtOJHG4N2R66U+wKmiV8kRmhvUhfb/QKS51vCq00vnUmNke8I8EJkY75XYyIhfVzqkb3ZiPkVL4/lAXM/yQiUY9YUpUvpZmxgtm6CUaikMyyJg3GierGUibxzNfiSA7aYJe0InB3wePmWkLKd0HpkF6eiXUBZ1Ny1G67Guw+n+kjpVJ6Fw5IoHoEKKcSi80b7OOZPUAyB3ynjYmFWb/hAxOd1Q0/Ma4Z6QXUbA5YlBwQqT+jrnAcEsaR2k/t0MNzGuZz7m17hYpmWyNfPa0d6L6uO21UxFKNNGAMIydgHnUnzCfUuPQvxIB63tiwT9Q== phone";
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
