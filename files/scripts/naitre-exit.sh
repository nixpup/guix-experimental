#!/nix/store/j8645yndikbrvn292zgvyv64xrrmwdcb-bash-5.3p3/bin/bash
#!/usr/bin/env sh

pid=$(pidof naitre)
choice=$(printf 'Yes
No' | vicinae dmenu --placeholder "Exit NaitreHUD?")

if [ "$choice" = "Yes" ] && [ -n "$pid" ]; then
    kill "$pid"
fi
