{ runCommand, imagemagick }:

runCommand "blurred.png" {
  buildInputs = [ imagemagick ];
} ''
  magick -monitor ${./bg.jpg} -gaussian-blur 72,24 $out
''
