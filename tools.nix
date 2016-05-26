with import <nixpkgs> {};

let isaplib = stdenv.mkDerivation {
name = "isaplib";
src = fetchgit {
url = "https://github.com/Warbo/isaplib.git";
sha256 = "1hfyilabdmz219p8r3dq1wl662ir2bwpa9gdlnagphbpikl54qby";
};
    };
    isaplanner = stdenv.mkDerivation {
  name = "IsaPlanner";
  src  = fetchsvn {
    url = "svn://svn.code.sf.net/p/isaplanner/code/trunk/IsaPlanner/IsaPlanner2013";
    sha256 = "0l30m2sr7640ljg9zf5b83l8a3sqnbyz5j6q48b8b6l2i0sryb9v";
  };

  #../NotMine/isaplanner-code/IsaPlanner;

  buildInputs = [ isaplib perl isabelle ];

  buildPhase = ''
    export HOME=/tmp
    isabelle build
  '';

  installPhase = ''
    find .
  '';
};
#isacosy =
in isaplanner
