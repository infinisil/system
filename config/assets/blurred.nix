{ runCommand, imagemagick7 }:

runCommand "blurred.png" {
  buildInputs = [ imagemagick7 ];
} ''
  magick -monitor ${./bg.jpg} -gaussian-blur 72,24 $out
''
