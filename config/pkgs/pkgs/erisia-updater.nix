{ pkgs }:

with pkgs;
with xorg;

let
  jar = (import (import ../../sources).erisia-builder).ServerPack + "/MCUpdater-Bootstrap.jar";

  desktopItem = makeDesktopItem {
    name = "mcupdater";
    exec = "mcupdater";
    icon = "mcupdater";
    comment = "A sandbox-building game";
    desktopName = "Minecraft";
    genericName = "mcupdater";
    categories = "Game;";
  };

  libPath = stdenv.lib.makeLibraryPath [
    libpulseaudio
    libXxf86vm # Needed only for versions <1.13
  ];

in stdenv.mkDerivation {
  name = "mcupdater-2018-12-17";

  src = jar;

  nativeBuildInputs = [ makeWrapper ];

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out/bin $out/share/mcupdater

    makeWrapper ${jre}/bin/java $out/bin/mcupdater \
      --add-flags "-jar $out/share/mcupdater/mcupdater.jar" \
      --suffix LD_LIBRARY_PATH : ${libPath} \
      --set _JAVA_AWT_WM_NONREPARENTING 1

    cp $src $out/share/mcupdater/mcupdater.jar
    cp -r ${desktopItem}/share/applications $out/share
  '';

  meta = with stdenv.lib; {
    description = "A sandbox-building game";
    homepage = https://mcupdater.net;
    maintainers = with maintainers; [ Baughn ];
    license = licenses.apache2;
  };
}
