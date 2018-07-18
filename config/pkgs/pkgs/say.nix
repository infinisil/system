{ stdenv, writeScriptBin, awscli, vorbis-tools, passwords }:

writeScriptBin "say" ''
  #!${stdenv.shell}
  tosay="''${*}"
  cachedir="$HOME/.cache/say"
  mkdir -p "$cachedir"
  hash=$(echo "$tosay" | md5sum - | cut -d' ' -f1)
  file="$cachedir/$hash"
  if [ ! -s "$file" ]; then
    export AWS_ACCESS_KEY_ID="${passwords.say.key}"
    export AWS_SECRET_ACCESS_KEY="${passwords.say.secret}"
    ${awscli}/bin/aws polly synthesize-speech --output-format ogg_vorbis --voice-id Justin "$file" --text "$tosay" --region eu-west-1
  fi
  ${vorbis-tools}/bin/ogg123 "$file"
''
