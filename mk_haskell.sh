#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket -p haskellPackages.tip-lib pv

bash mk_defs.sh | bash mk_signature.sh
