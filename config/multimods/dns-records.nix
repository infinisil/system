{ lib, config, ... }: let
  dkimKey = "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDqEBkfzMeMpXcHmMnasi5sE98SGIphwuWMHFmtXAtqGKsr8gjOQ5rZLTRhqOZR2CZc6xY2iCBtQ6nxFOHfJ/UW5tNanvi2nuo4jhrq9+ZNupdsKwxDpBNm7W9HVO2a0FP6dGa9bme0Zc4wqf9Socialr02YuZqRKwU3kBQtfRg4wIDAQAB";
in {

  options.subdomains = lib.mkOption {
    type = lib.types.listOf lib.types.str;
  };

  config = {

    subdomains = [ "keys" "www" "private" "dav" "public" "paste" "pc" ];

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

    dns.records = {
      "infinisil.com.".A = "206.81.23.189";
      "infinisil.com.".AAAA = "2a03:b0c0:3:d0::5f7f:5001";
      "infinisil.com.".NS = [ "ns3" "ns4" ];
      "infinisil.com.".CAA = "letsencrypt.org";
      "mail.infinisil.com.".A = "206.81.23.189";
      "mail.infinisil.com.".AAAA = { ttl = 10; address = "2a03:b0c0:3:d0::5f7f:5001"; };
      "infinisil.com.".MX = [
        {
          domain = "in1-smtp.messagingengine.com.";
          preference = 10;
        }
        {
          domain = "in2-smtp.messagingengine.com.";
          preference = 20;
        }
      ];
      "_submission._tcp.infinisil.com.".SRV = {
        priority = 0;
        weight = 0;
        port = 0;
        target = ".";
      };
      "_imap._tcp.infinisil.com.".SRV = {
        priority = 0;
        weight = 0;
        port = 0;
        target = ".";
      };
      "_pop3._tcp.infinisil.com.".SRV = {
        priority = 0;
        weight = 0;
        port = 0;
        target = ".";
      };
      "_submissions._tcp.infinisil.com.".SRV = {
        priority = 0;
        weight = 1;
        port = 465;
        target = "smtp.fastmail.com.";
      };
      "_imaps._tcp.infinisil.com.".SRV = {
        priority = 0;
        weight = 1;
        port = 993;
        target = "imap.fastmail.com.";
      };
      "_pop3s._tcp.infinisil.com.".SRV = {
        priority = 10;
        weight = 1;
        port = 995;
        target = "pop.fastmail.com.";
      };
      "_jmap._tcp.infinisil.com.".SRV = {
        priority = 0;
        weight = 1;
        port = 443;
        target = "api.fastmail.com.";
      };
      "_autodiscover._tcp.infinisil.com.".SRV = {
        priority = 0;
        weight = 1;
        port = 443;
        target = "autodiscover.fastmail.com.";
      };
      "infinisil.com.".TXT = "v=spf1 include:spf.messagingengine.com ?all";
      "_dmarc.infinisil.com.".TXT = "v=DMARC1; p=quarantine; rua=mailto:dmarc_rua@infinisil.com; ruf=mailto:dmarc_ruf@infinisil.com";
      "fm1._domainkey.infinisil.com.".CNAME = "fm1.infinisil.com.dkim.fmhosted.com.";
      "fm2._domainkey.infinisil.com.".CNAME = "fm2.infinisil.com.dkim.fmhosted.com.";
      "fm3._domainkey.infinisil.com.".CNAME = "fm3.infinisil.com.dkim.fmhosted.com.";
      "ns3.infinisil.com.".A = "206.81.23.189";
      "ns4.infinisil.com.".A = "206.81.23.189";
      "torrent.infinisil.com.".A = "51.15.187.150";
      "media.infinisil.com.".A = "51.15.187.150";
      "tune.infinisil.com.".A = "51.15.187.150";
      "tune.infinisil.com.".AAAA = "2a03:b0c0:3:d0::5f7f:5001";
    } // lib.listToAttrs (map (s: lib.nameValuePair "${s}.infinisil.com." { CNAME = "infinisil.com."; }) config.subdomains);
  };
}
