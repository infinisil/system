{ config, ... }:
{

  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    networks = {

      infinisil = {
        psk = "${config.private.passwords."wlan/iPhone"}";
        priority = 10;
      };

      eth-5 = {
        auth = ''
          key_mgmt=WPA-EAP
          eap=TTLS
          identity="msilvan"
          password="${config.private.passwords."wlan/eth"}"
          phase2="auth=MSCHAPV2"
        '';
        priority = 10;
      };

      Swisscom.priority = 2;

      "FRITZ!Box 7490".priority = 3;

    };
  };

  networking.extraHosts = ''
    192.168.1.1 swisscom.mobile
  '';

}
