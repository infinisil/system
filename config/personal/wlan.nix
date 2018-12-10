{ config, ... }: {
  networking.wireless.networks = {
    infinisil = {
      psk = "${config.private.passwords."wlan/iPhone"}";
      priority = 60;
    };

    eduroam-5 = {
      auth = ''
        key_mgmt=WPA-EAP
        eap=TTLS
        identity="msilvan@student-net.ethz.ch"
        password="${config.private.passwords."wlan/eth"}"
        phase2="auth=MSCHAPV2"
      '';
      priority = 80;
    };

    Idyll = {
      priority = 10;
      psk = config.private.passwords."internet/Idyll";
    };
    "FRITZ!Box 7490".priority = 30;
  };
}
