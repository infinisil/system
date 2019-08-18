with import <nixpkgs/lib>;
rec {
  hosts = [
    "protos"
    "vario"
    "ninur"
    "orakel"
    "protos2"
  ];

  public = {
    protos = "104.248.129.84";
    protos2 = "206.81.23.189";
    orakel = "51.15.187.150";
  };

  vpn = {
    protos = "10.149.76.1";
    vario = "10.149.76.2";
    ninur = "10.149.76.3";
    orakel = "10.149.76.5";
  };

  local = {
    ninur.vario.ethernet = "192.168.178.28";
    vario.ninur.wireless = "192.168.178.21";
    vario.ninur.ethernet = "192.168.178.53";
  };

  preferred = genAttrs hosts (from:
    genAttrs hosts (to:
      (optional (from == to) "localhost") ++
      (optional (local ? ${from}.${to}.ethernet) local.${from}.${to}.ethernet) ++
      (optional (local ? ${from}.${to}.wireless) local.${from}.${to}.wireless) ++
      (optional (public ? ${to}) public.${to}) ++
      (optional (vpn ? ${to}) vpn.${to})
    )
  );

  connections = {
    ninur = {
      ninur = "localhost";
      protos = public.protos;
      vario = vpn.vario;
      vario-e = local.ninur.vario.ethernet;
      orakel = public.orakel;
    };
    vario = {
      vario = "localhost";
      protos = public.protos;
      protos2 = public.protos2;
      ninur = vpn.ninur;
      ninur-w = local.vario.ninur.wireless;
      ninur-e = local.vario.ninur.ethernet;
      orakel = public.orakel;
    };
    protos = {
      protos = "localhost";
      vario = vpn.vario;
      ninur = vpn.ninur;
      orakel = public.orakel;
    };
    orakel = {
      orakel = "localhost";
      protos = public.protos;
      ninur = vpn.ninur;
      vario = vpn.vario;
    };
    protos2 = {
      protos2 = "localhost";
      vario = vpn.vario;
    };
  };

}
