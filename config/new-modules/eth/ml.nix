{ config, lib, pkgs, ... }:

with lib;

{

  options.mine.eth.mlServer = mkEnableOption "ML server for collaborating";

  config = mkIf config.mine.eth.mlServer {
    users.extraUsers.ml = {
      description = "ML group";
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDDb+mJdmU9j3R4gWYJHyPhJ1eQ15B8/naEP6VBfGIXZR7AThT9J1tZPvXtILQ+8YaO7NLDOgRfsRqNVRMsckbiXQIuAPPxd9uX2vDI3tie0Y5E4LlwZ9lDYgt2/G4ETH01+2H1xROnQKf1/0NZFZDvvBZb0t+1knho6sWJ4+cRTBpnfheDZmm9Kuj/1c0e5qfGoc0+X7kRZ3MNjeXrZoK43Dd0EPQrMRH1pa+zU6h+2QjR+i2Yb9IplIrl7lti9a9pJEDZD7CI/XLU/5YXio5n92zGyTH7JkQb/Mvw6SvNeL9r1Be6AkKf3IVuWoVLbnndhHu/RAvtzKpoJKQ2kdOF joris"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC1CtqHLMY1svDlf7SkfBsjLv1tzdZacdaIWpmmPyVdtdbZLFwu3bmZGjbIVY+VvUTijdnwgsmQfAex1U8hZynNnlZ1rxI7/bWk+TaUuvzcLPrOgAsiLX9ubSV8x0xnPmuRNjJZ7VRJ5XJGxUNfxmpN7lDlCqp5tTXtwpIvB8nYWyIgjC4qT+XiNTNcTVGsFtRwimJcO4ohes6cjE3rAPkIIt572u1YXQwaHKw3cpMn7oSey5Oa4uHBD3U+TZdw1dZKyzVDe6kycqJDwBhtTJKtnftzQruTwijeGFY5yJiKeDVAflo3Kfv9Eer6ry9Br4VQA0a+lLVltrCK/S0nkRNN jan"
        config.mine.sshkeys.ninur.infinisil
      ];
      shell = pkgs.bash;
    };
  };

}
