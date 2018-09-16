with import ./. {};

shellFor {
  packages = p: with p; [ mytaffybar ];
  withHoogle = true;
  nativeBuildInputs = [ cabal-install ];
}
