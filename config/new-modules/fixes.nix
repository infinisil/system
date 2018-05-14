{

  nixpkgs.overlays = [(self: super: {
    # nixpkgs commit 3c74f80e00ed6dacc2cf498ab238247e54baf77d broke znapzend
    znapzend = super.znapzend.override {
      perlPackages = self.perlPackages // {
        Mojolicious = self.perlPackages.Mojolicious.overrideAttrs (old: rec {
          name = "Mojolicious-6.56";
          src = super.fetchurl {
           url = "mirror://cpan/authors/id/S/SR/SRI/${name}.tar.gz";
           sha256 = "82f73553836ac378edf825fd9f24be982653be9e0d78f8ba38b7841aabdafb02";
          };
          propagatedBuildInputs = [ self.perlPackages.JSONPP ];
        });
      };
    };
  })];

}
