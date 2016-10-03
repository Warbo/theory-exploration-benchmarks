with import <nixpkgs> {};
with lib;

rec {


 te-benchmarks = stdenv.mkDerivation {
      name = "te-benchmarks";
      src  = ./.;
      buildInputs = [ (racketWithDeps [ shellPipeline ]) ];
    };
}
