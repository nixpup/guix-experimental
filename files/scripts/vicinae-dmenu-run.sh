#!/nix/store/j8645yndikbrvn292zgvyv64xrrmwdcb-bash-5.3p3/bin/bash
#!/usr/bin/env bash

compgen -c | sort -u | vicinae dmenu --placeholder "Run" | sh
