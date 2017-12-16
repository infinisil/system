{ stdenv, fetchFromGitHub, autoreconfHook, pkgconfig, gnome3, gtk-engine-murrine, gnome_themes_standard }:

let
  # treat versions newer than 3.22 as 3.22
  gnomeVersion = if stdenv.lib.versionOlder "3.22" gnome3.version then "3.22" else gnome3.version;
  pname = "arc-theme-red";

in stdenv.mkDerivation rec {
  name = "${pname}-${version}";
  version = "2017-05-12";

  src = fetchFromGitHub {
    owner  = "mclmza";
    repo   = pname;
    rev    = "b216c76f34232f6eb71df585b74de43f9367b3ec";
    sha256 = "067f3w65fdskhid65xlai85pdww9znzb23akr9xryzs5nb3sp485";
  };

  nativeBuildInputs = [ autoreconfHook pkgconfig ];

  buildInputs = [ gtk-engine-murrine gnome3.gtk gnome_themes_standard ];

  preferLocalBuild = true;

  configureFlags = [ "--disable-unity" "--with-gnome=${gnomeVersion}" ];

  meta = with stdenv.lib; {
    description = "A flat theme with transparent elements for GTK 3, GTK 2 and Gnome-Shell";
    homepage    = https://github.com/mclmza/arc-theme-red;
    license     = licenses.gpl3;
    maintainers = with maintainers; [ simonvandel romildo ];
    platforms   = platforms.unix;
  };
}
