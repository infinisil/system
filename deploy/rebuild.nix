{ stdenv, writeScriptBin, git, nixops }:

writeScriptBin "rb" ''
  #!${stdenv.shell}
  label=$(printf "%.35s" \
    "$(${git}/bin/git -C ${toString ../.} log --pretty=format:'%h-%f' -n 1)")

  sudo ${nixops}/bin/nixops set-args -s ${toString ../external/private/deployments.nixops} -d infinisil --argstr label "$label"
  sudo ${nixops}/bin/nixops deploy -s ${toString ../external/private/deployments.nixops} -d infinisil $@
''
