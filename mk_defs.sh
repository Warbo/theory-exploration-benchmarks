#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket pv

bash qual_all.sh | bash norm_defs.sh
