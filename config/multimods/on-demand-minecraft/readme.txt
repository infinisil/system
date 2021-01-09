- Create a new DO volume of 1GB size and attach it to your existing droplet
- sudo zpool create -m legacy -o autoexpand=on -O compress=on -O xattr=off -O atime=off minecraft /dev/sda
- Create ops.json if needed
- Create whitelist.json if needed
- chown 114:nogroup the folder recursively
- Load the world into there, or let it autogenerate

- After expanding disk (if necessary), run `sudo zpool online -e minecraft /dev/sda`

