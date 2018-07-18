{ stdenv, writeScriptBin, curl, wget, ripgrep }:
writeScriptBin "imgurdl" ''
  #!${stdenv.shell}
  ${curl}/bin/curl "$*" | ${ripgrep}/bin/rg '<div id="([[:alnum:]]{7})"' -or 'https://i.imgur.com/$1.png' | ${wget}/bin/wget -i-
''
