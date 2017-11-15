{ config, ... }:
let
  conf = ''
    network={
      ssid="infinisil"
      psk="${config.private.passwords."wlan/iPhone"}"
      priority=10
    }
    network={
      ssid="eth-5"
      key_mgmt=WPA-EAP
      eap=TTLS
      identity="msilvan"
      password="${config.private.passwords."wlan/eth"}"
      priority=5
      phase2="auth=MSCHAPV2"
    }
    network={
      ssid="Swisscom"
      key_mgmt=NONE
      priority=1
    }
    network={
      ssid="FRITZ!Box 7490"
      key_mgmt=NONE
      priority=3
    }
  '';
in
{
  environment.etc."wpa_supplicant.conf".text = conf;

  networking = {
    wireless.enable = true;
    extraHosts = ''
      192.168.1.1 swisscom.mobile
    '';
  };
}
