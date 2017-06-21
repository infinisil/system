{ pkgs, ...}:
let
  say = pkgs.writeScriptBin "say" ''
    #!/usr/bin/env bash
    tosay="''${@}"
    cachedir=$HOME/.cache/say
    mkdir -p $cachedir
    hash=$(echo "$tosay" | ${pkgs.coreutils}/bin/md5sum - | ${pkgs.gawk}/bin/gawk '{ print $1 }')
    file=$cachedir/"$hash"
    if [ ! -s "$file" ]; then
      ${pkgs.awscli}/bin/aws polly synthesize-speech --output-format ogg_vorbis --voice-id Justin "$file" --text "$tosay"
    fi
    ${pkgs.vorbis-tools}/bin/ogg123 "$file"
  '';
in
{
  environment.systemPackages = [ say ];
}
