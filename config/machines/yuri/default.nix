{ nodes, ... }: {

  imports = [
    ../../hardware/do
    ../../../external/private/server.nix
    ./hardware-configuration.nix
    /home/infinisil/prj/nixbot/module.nix
  ];

  mine.profiles.server.enable = true;

  services.ipfs = {
    enable = true;
    autostart = true;
    enableGateway = true;
  };

  boot = {
    loader.grub.device = "/dev/vda";
    zfs.devNodes = "/dev";
    zfs.enableUnstable = true;
    kernelParams = [ "net.ifnames=0" ];
  };

  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC7zf2O8yBXxh2tX9v/3ZztXtYeV4W9vTY2iSrm92HSErjz5KcIY/AAKaqbWXHZgsZk2pehBqNbQMOwn0WWdLvil2+Ah97cvl7d9b9XdCkfOPhNB6FKcTzPmMp5Rivi/IodVMhT2xO9S1zO0Y2Q7dsYgk5leKyiD10pkcw23p6MPMKhKV2DPgY6BiszrTEVmtyOHpGkji9rE1iB9MyOINY9eC4etmnNINXMlwttV0GjbJI9WXXEQN2mRaPPp1PBWaPOgoP3ufKi9MR1hEhAantyrfBm2SeqjUvXG5JN1RyooohIWIHWXNJlYFldFPsCD/C1HnE5ylJeLBbZEw0TPb6x infinisil@NixOS"
  ];

  services.znapzend = {
    enable = true;
    autoCreation = true;
    pure = true;
    zetup = {
      "main/root/data" = {
        plan = "1hour=>10mins,1day=>1hour,1week=>1day,1month=>1week";
        recursive = true;
        destinations.pc = {
          host = "pc";
          dataset = "main/backup/server";
          plan = "1day=>1hour,1week=>1day,1month=>1week,1year=>1month";
        };
      };
    };
  };


  mine.rpf.server = {
    enable = true;
    clients = with nodes; [
      emma
      nepnep
    ];
  };

  networking = {
    hostName = "yuri";
    hostId = "84efc532";
    domain = "infinisil.com";
    defaultGateway = "207.154.192.1";
    defaultGateway6 = "2a03:b0c0:3:d0::1";
    interfaces.eth0 = {
      ipv4.addresses = [{
        address = "207.154.198.134";
        prefixLength = 20;
      }];
      ipv6.addresses = [{
        address = "2a03:b0c0:3:d0::af7:3001";
        prefixLength = 64;
      }];
    };

    firewall.allowedTCPPorts = [ 12345 ];
  };
}
