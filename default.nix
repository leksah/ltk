{ mkDerivation, base, base-compat, Cabal, containers, filepath, ghc
, gi-cairo, gi-gdk, gi-glib, gi-gobject, gi-gtk, gi-gtk-hs, gnome3
, haskell-gi-base, mtl, parsec, pretty, stdenv, text, transformers
}:
mkDerivation {
  pname = "ltk";
  version = "0.16.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base base-compat Cabal containers filepath ghc gi-cairo gi-gdk
    gi-glib gi-gobject gi-gtk gi-gtk-hs haskell-gi-base mtl parsec
    pretty text transformers
  ];
  libraryPkgconfigDepends = [ gnome3.gtk3 ];
  homepage = "http://www.leksah.org";
  description = "Leksah tool kit";
  license = "GPL";
}
