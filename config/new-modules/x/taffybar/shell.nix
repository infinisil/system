with import ./. {};

(shellFor {
  packages = p: with p; [ mytaffybar ];
  withHoogle = true;
  nativeBuildInputs = [ cabal-install ];
}).overrideAttrs (drv: {
  shellHook = drv.shellHook + ''
    export GDK_PIXBUF_MODULE_FILE="${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"
  '';
})
