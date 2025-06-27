{ config, ... }: {

  services.on-demand-minecraft.instances.main = {
    enable = false;
    node = "protos";
    domain = "infinisil.com";
    imageConfiguration = ./minecraft.nix;
    settings = {
      whitelist.infinisil = "01e2780a-1334-4891-95dd-506e58dcebb9";
      digitalOcean = {
        region = "fra1";
        size = "g-2vcpu-8gb";
        sshKey = "25879389";
        volume = "8b787688-52d2-11ea-9e33-0a58ac14d123";
        # TODO: This is nasty, make it better
        tokenFile = config.nodes.protos.configuration.secrets.files.doauth.file;
      };
    };
  };

  nodes.protos.configuration.systemd.services.on-demand-minecraft-main.serviceConfig.SupplementaryGroups = "do-api";

  services.on-demand-minecraft.instances.ss = {
    enable = false;
    node = "protos";
    domain = "ss.infinisil.com";
    imageConfiguration = ./minecraft-1.16.5.nix;
    version = "1.16.5";
    settings = {
      port = 25568;
      whitelist.infinisil = "01e2780a-1334-4891-95dd-506e58dcebb9";
      digitalOcean = {
        region = "fra1";
        size = "g-2vcpu-8gb";
        sshKey = "25879389";
        volume = "3a00c480-d504-11ea-9c34-0a58ac14d166";
        # TODO: This is nasty, make it better
        tokenFile = config.nodes.protos.configuration.secrets.files.doauth.file;
      };
    };
  };

  nodes.protos.configuration.systemd.services.on-demand-minecraft-ss.serviceConfig.SupplementaryGroups = "do-api";

  services.on-demand-minecraft.instances.creative = {
    enable = false;
    node = "protos";
    domain = "creative.infinisil.com";
    imageConfiguration = ./minecraft.nix;
    settings = {
      port = 25566;
      whitelist.infinisil = "01e2780a-1334-4891-95dd-506e58dcebb9";
      digitalOcean = {
        region = "fra1";
        size = "g-2vcpu-8gb";
        sshKey = "25879389";
        volume = "a2a89a37-514a-11eb-954c-0a58ac14d103";
        # TODO: This is nasty, make it better
        tokenFile = config.nodes.protos.configuration.secrets.files.doauth.file;
      };
    };
  };

  nodes.protos.configuration.systemd.services.on-demand-minecraft-creative.serviceConfig.SupplementaryGroups = "do-api";

  services.on-demand-minecraft.instances.qbp = {
    enable = false;
    node = "protos";
    domain = "qbp.infinisil.com";
    imageConfiguration = ./minecraft.nix;
    settings = {
      port = 25567;
      whitelist.infinisil = "01e2780a-1334-4891-95dd-506e58dcebb9";
      digitalOcean = {
        region = "fra1";
        size = "g-2vcpu-8gb";
        sshKey = "25879389";
        volume = "79264106-a503-11eb-a32c-0a58ac14d077";
        # TODO: This is nasty, make it better
        tokenFile = config.nodes.protos.configuration.secrets.files.doauth.file;
      };
    };
  };

  nodes.protos.configuration.systemd.services.on-demand-minecraft-qbp.serviceConfig.SupplementaryGroups = "do-api";
}
