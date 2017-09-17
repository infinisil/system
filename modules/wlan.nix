{ config, ... }:
let
  conf = ''
    network={
      ssid="infinisil"
      psk="${config.passwords.iPhoneWlan}"
      priority=10
    }
    network={
      ssid="eth-5"
      key_mgmt=WPA-EAP
      eap=TTLS
      identity="msilvan"
      password="${config.passwords.ethWlan}"
      priority=5
      phase2="auth=MSCHAPV2"
    }
    network={
      ssid="Swisscom"
      key_mgmt=NONE
      priority=2
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
}
