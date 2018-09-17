let
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ca2ba44cab47767c8127d1c8633e2b581644eb8f.tar.gz";
    sha256 = "1jg7g6cfpw8qvma0y19kwyp549k1qyf11a5sg6hvn6awvmkny47v";
  };

  nixpkgs = (import nixpkgsSrc {}).srcOnly {
    name = "nixpkgs-patched";
    src = nixpkgsSrc;
    patches = [
      # https://github.com/NixOS/nixpkgs/pull/46453
      (builtins.fetchurl {
        url = "https://github.com/NixOS/nixpkgs/commit/e6dd03d554e65badd9cdc8b9c137a5998a642e42.patch";
        sha256 = "0aisra3arv6x6z59dfw4bfxyj40pm6liixgkwpj1rjrr0ql4yc9s";
      })
    ];
  };
in
{ pkgs ? import nixpkgs {}
}:

with pkgs.haskell.lib;

let

  hpkgs = pkgs.haskell.packages.ghc822.extend (self: super: {
    # https://github.com/NixOS/nixpkgs/pull/46766
    ListLike = addBuildDepend super.ListLike self.semigroups;

    # https://github.com/NixOS/nixpkgs/pull/46767
    gi-glib = super.gi-glib.override { haskell-gi-overloading = self.haskell-gi-overloading_0_0; };
    gi-cairo = super.gi-cairo.override { haskell-gi-overloading = self.haskell-gi-overloading_0_0; };
    gi-xlib = super.gi-xlib.override { haskell-gi-overloading = self.haskell-gi-overloading_0_0; };
  });
in

hpkgs.extend (packageSourceOverrides {
  mytaffybar = ./.;
})
