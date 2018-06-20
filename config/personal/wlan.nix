{
  networking.wireless.networks = {
    infinisil = {
      psk = "${config.private.passwords."wlan/iPhone"}";
      priority = 60;
    };

    eth-5 = {
      auth = ''
        key_mgmt=WPA-EAP
        eap=TTLS
        identity="msilvan"
        password="${config.private.passwords."wlan/eth"}"
        phase2="auth=MSCHAPV2"
      '';
      priority = 80;
    };

    Swisscom.priority = 10;
    "FRITZ!Box 7490".priority = 30;

  };
}
