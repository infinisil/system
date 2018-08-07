{ stdenv, writeScriptBin, curl, wget, ripgrep }:
writeScriptBin "imgurdl" ''
  #!${stdenv.shell}
  set -uo pipefail

  albumurl=$1
  albumid=$(echo "$albumurl" \
    | ${ripgrep}/bin/rg 'imgur.com/a/([[:alnum:]]+)' -or '$1')

  if [ $? -ne 0 ]; then
    echo "$albumurl doesn't appear to be an imgur album url"
    exit 1
  fi

  echo "Downloading image list.."

  urls=$(${curl}/bin/curl -sS "$albumurl" \
    | ${ripgrep}/bin/rg '<div id="([[:alnum:]]{7})"' -or 'https://i.imgur.com/$1.png')

  if [ $? -ne 0 ]; then
    echo "Error extracting image list"
    exit 1
  fi

  num=$(echo "$urls" | wc -l)
  numlength=$(echo -n "$num" | wc -m)
  prettyNum=$(printf "%.''${numlength}d" "$num")

  echo "Found $num image links"

  n=1
  echo "$urls" | while read url; do
    prettyN=$(printf "%.''${numlength}d" "$n")
    echo "[$prettyN/$prettyNum] Downloading $url.."
    ${wget}/bin/wget -q -O "$albumid-$prettyN.png" "$url"
    n=$(( n + 1 ))
  done
''
