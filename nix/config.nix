with import <nixpkgs> {};
cfg: ''
${lib.concatMapStringsSep "\n" (host: ''
Host ${host.alias}
	HostName ${host.name}
	${lib.optionalString (host?user) "User ${host.user}"}
	${lib.optionalString (host?port) "Port ${host.port}"}
'') cfg.hosts} 
''
