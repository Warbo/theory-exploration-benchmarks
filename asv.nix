# Useful for `nix-shell asv.nix`
with import ./. {};
with { pkgs = import <nixpkgs> {}; };

pkgs.stdenv.mkDerivation {
  name         = "te-benchmark-asv";
  buildInputs  = [ ((import <nixpkgs> {}).callPackage /home/chris/Programming/Python/asv-nix {}) /*asv*/ ];
  buildCommand = "exit 1";

  shellHook    = ''
    {
      echo "This shell is suitable for benchmarking the various scripts in this"
      echo "repository. Benchmarks are written in Python in the asv/ folder."

      echo "We use the 'asv' command to do this, e.g. with 'asv run'. For full"
      echo "documentation, see http://asv.readthedocs.io/en/latest/using.html"

      echo "Note that we use a custom 'asv_nix' environment plugin. See the"
      echo "comments in asv.conf.json"

      echo "Our policy is to keep benchmark data (from 'asv run') in git, even"
      echo "though it's volatile. Data is tagged with the machine it came from"
      echo "so your results won't overwrite others'. Feel free to overwrite and"
      echo "commit results from your own machine."

      echo "Use 'asv publish' to generate a HTML report of the results. Note"
      echo "that we don't store these in git, since they're generated"
      echo "deterministically from the results."
    } 1>&2
  '';
}
