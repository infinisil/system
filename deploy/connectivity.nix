with import <nixpkgs/lib>;
rec {
  hosts = [
    "protos"
    "vario"
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


  preferred = genAttrs hosts (from:
    genAttrs hosts (to:
      (optional (from == to) "localhost") ++
      (optional (public ? ${to}) public.${to}) ++
      concatLists (mapAttrsToList (vpnServer: clients:
        optional (clients ? ${from} && clients ? ${to})
          clients.${to}
      ) vpn)
    )
  );

  connections = {
    vario = {
      vario = "localhost";
      protos = public.protos;
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
