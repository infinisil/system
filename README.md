# System configuration

This repository contains everything needed to build the NixOS system configuration for all my machines.

## Structure

The `config` directory is where the meat is at, it contains almost everything that actually sets configuration. A small selection: keyboard layout, public ssh keys, packages, themes, and much more. The folder contains a Readme with additional information on how its structured.

The `deploy` directory contains everything related to (you guessed it) deployment. This includes a script for deployment, the nixops configuration for all machines and a directory that can easily evaluate machines configurations.

The `deploy/eval` directory contains another Readme with an explanation of that.

Lastly, the `external` directory contains external sources for my config as git submodules (an argument could be made that it should really be in the `config` directory though). This currently includes nixpkgs, home-manager, nixops and a private repo.

`external/private` contains all things I can't share publicly such as the nixops state (it includes the private ssh keys), openvpn keys, and passwords. To make it usable in other parts of my config, it defines a `private` option, which I can use e.g. like `config.private.passwords.foobar`. Eventually I might share the password nix file generator of that directory, which doesn't really have to be private.
