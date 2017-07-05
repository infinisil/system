.ssh/config.nix : .ssh/config
	echo 'with import <nixpkgs> {};' > $@
	echo "cfg: ''" >> $@
	cat $^ >> $@
	echo "''" >> $@

irssi/config.nix : irssi/config
	echo 'with import <nixpkgs> {};' > $@
	echo "cfg: ''" >> $@
	cat $^ >> $@
	echo "''" >> $@

~/.ssh/config : .ssh/config.nix default.nix config.nix
	nix-build
	cp result $@

~/.irssi/config : irssi/config.nix default.nix config.nix
	nix-build
	cp result $@

ssh : ~/.ssh/config

irssi : ~/.irssi/config
