{ runCommand, imagemagick7 }:

runCommand "display-manager.png" {
  buildInputs = [ imagemagick7 ];
} ''
  magick -monitor ${./grub.png} -gaussian-blur 72,24 $out
''
