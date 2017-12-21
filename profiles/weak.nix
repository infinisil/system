{ nodes, ... }: {

  nix.distributedBuilds = false;

  nix.buildMachines = [

    {
      hostName = "pc";
      sshUser = "root";
      sshKey = "/root/.ssh/id_rsa";
      system = "x86_64-linux";
      maxJobs = 8;
    }

  ];
}
