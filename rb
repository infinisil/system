#!/usr/bin/env bash


# TODO:
# Run both nixos-rebuild build and home-manager build
# Then check whether they need to be activated
# something like that, but then we won't be able to have the home-manager config depend on the system
# Or will we?? What if there's an interface for having home-manager depend on the system
# Something like "nixos-rebuild will write /var/lib/home-manager-defaults/defaults.json, which home-manager will/can then read if enabled by the user

set -euo pipefail

ROOT=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd "$ROOT"

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' exit

deployHost=$(hostname)
deploySystem=$(nix-instantiate --eval --strict --json -E builtins.currentSystem | jq -r .)

if [[ "$#" -eq 0 ]]; then
  set -- "$deployHost"
fi

nixpkgs=$(nix-instantiate --eval -E '(import ./nix/sources.nix {}).nixpkgs.outPath' --json | tr -d '"')

export NIX_PATH=nixpkgs="$nixpkgs"

echo "Building Nix..."
PATH=$(nix-build "$nixpkgs" -A nix --no-out-link)/bin:$(nix-build "$nixpkgs" -A nix-output-monitor --no-out-link)/bin:$PATH

nodes="[ "
for node in "$@"; do
  nodes+="\"$node\" "
done
nodes+="]"

echo "Building nix.conf..."
mkdir "$tmp/confdir"
nix-build --out-link "$tmp/confdir/nix.conf" >/dev/null \
  -A config.nodes."$(hostname)".configuration.environment.etc.'"nix/nix.conf"'.source
export NIX_CONF_DIR="$tmp/confdir"

echo "Evaluating..."
nix-instantiate --show-trace --add-root "$tmp"/drv --indirect >/dev/null \
  --arg nodes "$nodes" --argstr deployHost "$deployHost" --argstr deploySystem "$deploySystem" \
  --expr 'import (builtins.path { path = ./.; sha256 = "'$(nix-hash --type sha256 --base32 .)'"; })'

# --pure-eval is broken :/

echo "Building..."
nom-build "$tmp"/drv --out-link "$tmp/result" >/dev/null


echo "Deploying..."
"$tmp/result"

echo "Finished deploying"
