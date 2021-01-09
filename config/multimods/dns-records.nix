{ lib, ... }: let
  dkimKey = "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDqEBkfzMeMpXcHmMnasi5sE98SGIphwuWMHFmtXAtqGKsr8gjOQ5rZLTRhqOZR2CZc6xY2iCBtQ6nxFOHfJ/UW5tNanvi2nuo4jhrq9+ZNupdsKwxDpBNm7W9HVO2a0FP6dGa9bme0Zc4wqf9Socialr02YuZqRKwU3kBQtfRg4wIDAQAB";
in {


  dns.zones."infinisil.com" = {
    ttl = 600;
    primaryNode = "protos";
    soa.master = "ns3.infinisil.com.";
    soa.email = "hostmaster.infinisil.com.";
    soa.refresh = 3 * 60 * 60;
    soa.retry = 15;
    soa.expire = 7 * 24 * 60 * 60;
    soa.negativeTtl = 3 * 60 * 60;
  };

  dns.records =
    let
      subdomains = [ "keys" "www" "private" "dav" "public" "paste" "pc" "nixbot" ];
    in {
      "infinisil.com.".A = "206.81.23.189";
      "infinisil.com.".AAAA = "2a03:b0c0:3:d0::5f7f:5001";
      "infinisil.com.".NS = [ "ns3" "ns4" ];
      "infinisil.com.".CAA = "letsencrypt.org";
      "infinisil.com.".MX = "mail";
      "mail.infinisil.com.".A = "206.81.23.189";
      "mail.infinisil.com.".AAAA = { ttl = 10; address = "2a03:b0c0:3:d0::5f7f:5001"; };
      "infinisil.com.".TXT = "v=spf1 ip4:206.81.23.189 -all";
      "_dmarc.infinisil.com.".TXT = "v=DMARC1; p=quarantine; rua=mailto:dmarc_rua@infinisil.com; ruf=mailto:dmarc_ruf@infinisil.com";
      "mail._domainkey.infinisil.com.".TXT = "v=DKIM1; p=${dkimKey}";
      "ns3.infinisil.com.".A = "206.81.23.189";
      "ns4.infinisil.com.".A = "206.81.23.189";

      "tune.infinisil.com.".A = "51.15.187.150";
      "torrent.infinisil.com.".A = "51.15.187.150";
    } // lib.listToAttrs (map (s: lib.nameValuePair "${s}.infinisil.com." { CNAME = "infinisil.com."; }) subdomains);
}
