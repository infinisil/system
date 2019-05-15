{

  mine.dns.zones."infinisil.com" = {
    master = "protos";
    slaves = [ "orakel" ];
    records = serial: ''
      $TTL 600
      $ORIGIN infinisil.com.


      @ IN SOA ns1.infinisil.com. hostmaster.infinisil.com. (
        ${serial}
        3H
        15
        1w
        3h
      )

      infinisil.com. IN MX 10 mail.infinisil.com.

      infinisil.com. IN TXT "v=spf1 ip4:104.248.129.84 -all"
      _dmarc IN TXT "v=DMARC1; p=none; rua=mailto:dmarc_agg@infinisil.com"

      mail._domainkey.infinisil.com. IN TXT "v=DKIM1; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDqEBkfzMeMpXcHmMnasi5sE98SGIphwuWMHFmtXAtqGKsr8gjOQ5rZLTRhqOZR2CZc6xY2iCBtQ6nxFOHfJ/UW5tNanvi2nuo4jhrq9+ZNupdsKwxDpBNm7W9HVO2a0FP6dGa9bme0Zc4wqf9Socialr02YuZqRKwU3kBQtfRg4wIDAQAB"

      @ IN TXT "dnslink=/ipns/QmcF3xqxFZxDLBJ5fNmr8vZ5p83SoS5zuavYMhizh2L1dp"


      mail.infinisil.com. IN A 104.248.129.84

      infinisil.com. IN NS ns1.infinisil.com.
      infinisil.com. IN NS ns2.infinisil.com.

      ns1 IN A ${config.networking.connectivitySpec.public.protos}
      ns2 IN A ${config.networking.connectivitySpec.public.orakel}

      @ IN A 104.248.129.84
      @ IN AAAA 2a03:b0c0:3:e0::96:6001

      keys IN CNAME infinisil.com.
      www IN CNAME infinisil.com.
      dav IN CNAME infinisil.com.
      public IN CNAME infinisil.com.
      paste IN CNAME infinisil.com.
      tune IN CNAME infinisil.com.
      ipfs IN CNAME infinisil.com.
      vario IN CNAME infinisil.com.
      ninur IN CNAME infinisil.com.
      nixbot IN CNAME infinisil.com.
    '';
  };

}
