{ stdenv, writeScriptBin, git, nixops }:

writeScriptBin "rb" ''
  #!${stdenv.shell}
  label=$(printf "%.35s" \
    "$(${git}/bin/git -C /cfg log --pretty=format:'%h-%f' -n 1)")

  sudo ${nixops}/bin/nixops set-args -d infinisil --argstr label "$label"
  sudo ${nixops}/bin/nixops deploy -d infinisil $@
''
