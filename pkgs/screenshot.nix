{ stdenv, writeScriptBin, imagemagick, coreutils }:

writeScriptBin "screenshot" ''
  #!${stdenv.shell}
  sleep 1
  ${imagemagick}/bin/import -window root "$HOME/pics/screenshots/$(${coreutils}/bin/date +%F-%T).png"
''
