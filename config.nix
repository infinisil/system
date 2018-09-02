{ config, ... }:

let

  hosts = { name, machines, ... }: {

    networking.hostName = "${toString machines.foo.confi.services.xserver.enable}+1";

  };

in
{

  machines = {

    foo = {

      imports = [];
      confi = { ... }: {

        imports = [ hosts ];


        boot.loader.grub.device = "nodev";
        fileSystems."/".device = "x";

      };
    };

  };

}
