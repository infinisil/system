with import <nixpkgs/lib>;
rec {
  hosts = [
    "protos"
    "vario"
    "ninur"
    "orakel"
  ];

  public = {
    protos = "206.81.23.189";
    orakel = "51.15.187.150";
  };

  vpn = {
    protos = {
      protos = "10.99.0.1";
      vario = "10.99.0.2";
    };
    orakel = {
      orakel = "10.99.1.1";
      vario = "10.99.1.2";
      protos = "10.99.1.3";
    };
  };

  local = {
    ninur.vario.ethernet = "192.168.178.28";
    vario.ninur.wireless = "192.168.178.21";
    vario.ninur.ethernet = "192.168.178.52";
  };

  preferred = genAttrs hosts (from:
    genAttrs hosts (to:
      (optional (from == to) "localhost") ++
      (optional (local ? ${from}.${to}.ethernet) local.${from}.${to}.ethernet) ++
      (optional (local ? ${from}.${to}.wireless) local.${from}.${to}.wireless) ++
      (optional (public ? ${to}) public.${to}) ++
      concatLists (mapAttrsToList (vpnServer: clients:
        optional (clients ? ${from} && clients ? ${to})
          clients.${to}
      ) vpn)
    )
  );

  connections = {
    ninur = {
      ninur = "localhost";
      protos = public.protos;
      vario = vpn.orakel.vario;
      vario-e = local.ninur.vario.ethernet;
      orakel = public.orakel;
    };
    vario = {
      vario = "localhost";
      protos = public.protos;
      ninur-w = local.vario.ninur.wireless;
      ninur-e = local.vario.ninur.ethernet;
      orakel = public.orakel;
    };
    orakel = {
      orakel = "localhost";
      protos = public.protos;
      vario = vpn.orakel.vario;
    };
    protos = {
      protos = "localhost";
      vario = vpn.orakel.vario;
      orakel = public.orakel;
    };
  };

}
