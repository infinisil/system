- Create a new DO volume of 1GB size and attach it to your existing droplet
- sudo zpool create -m legacy -o autoexpand=on -O compress=on -O xattr=off -O atime=off minecraft /dev/sda
- Load the world into there

- After expanding disk (if necessary), run `sudo zpool online -e minecraft /dev/sda`
