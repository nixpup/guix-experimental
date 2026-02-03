(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu packages)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (guix gexp))


(home-environment
 (packages (specifications->packages
  (list "zsh"
        "git"
        "emacs"
        "emacs-emms"
        "emacs-erc"
        "emacs-erc-image"
        "emacs-company"
        "emacs-corfu"
        "emacs-corfu-terminal"
        "emacs-simple-httpd"
        "emacs-org"
        "emacs-pabbrev"
        "emacs-use-package"
        "emacs-lsp-mode"
        "emacs-lsp-ui"
        "emacs-markdown-mode"
        "emacs-multi-term"
        "emacs-multiple-cursors"
        "emacs-nix-mode"
        "emacs-rainbow-mode"
        "emacs-rust-mode"
        "emacs-rustic"
        "emacs-wttrin"
        "emacs-hydra"
        "emacs-all-the-icons"
        "emacs-all-the-icons-dired"
        "emacs-haskell-mode"
        "emacs-arduino-mode"
        "emacs-flycheck"
        "emacs-bongo"
        "emacs-compat"
        "emacs-xelb"
        "emacs-iedit"
        "emacs-anzu"
        "emacs-visual-regexp"
        "emacs-sudo-edit"
        "emacs-pdf-tools"
        "emacs-magit"
        "emacs-beacon"
        "emacs-doom-modeline"
        )))

 (services (list (service home-zsh-service-type
                           (home-zsh-configuration
                            (environment-variables '(("PS1" . "  %~ ")
                                                     ("EDITOR" . "emacs")))
                            (zshrc (list (local-file (string-append (current-source-directory) "/files/config/zshrc"))))
                            )
                           )
                 (simple-service 'mpv-config
                                 home-files-service-type
                                 `((".config/mpv" ,(local-file (string-append (current-source-directory) "/files/config/mpv"
                                                                              #:recursive? #t)))))
                 (simple-service 'dot-emacs-config
                                 home-files-service-type
                                 `((".emacs" ,(local-file (string-append (current-source-directory) "/files/config/guixmacs/emacs")))))

                 (simple-service 'guixmacs-themes-config
                                 home-files-service-type
                                 `((".guixmacs/themes" ,(local-file (string-append (current-source-directory) "/files/config/guixmacs/themes"
                                                                                   #:recursive? #t)))))

                 (simple-service 'guixmacs-logo-config
                                 home-files-service-type
                                 `((".guixmacs/files/nix_emacs_logo_small.png" ,(local-file (string-append (current-source-directory) "/files/config/guixmacs/files/nix_emacs_logo_small.png")))))

                 (simple-service 'discord-config
                                 home-files-service-type
                                 `((".config/vesktop" ,(local-file (string-append (current-source-directory) "/files/config/discord"
                                                                   #:recursive? #t)))))
                 (simple-service 'fastfetch-config
                                 home-files-service-type
                                 `((".config/fastfetch" ,(local-file (string-append (current-source-directory) "/files/config/fastfetch"
                                                                     #:recursive? #t)))))
                 (simple-service 'hyprlock-config
                                 home-files-service-type
                                 `((".config/hyprlock" ,(local-file (string-append (current-source-directory) "/files/config/hyprlock"
                                                                    #:recursive? #t)))))
                 (simple-service 'kitty-config
                                 home-files-service-type
                                 `((".config/kitty" ,(local-file (string-append (current-source-directory) "/files/config/kitty"
                                                                 #:recursive? #t)))))
                 (simple-service 'waybar-config
                                 home-files-service-type
                                 `((".config/waybar" ,(local-file (string-append (current-source-directory) "/files/config/waybar"
                                                                  #:recursive? #t)))))
                 (simple-service 'zathura-config
                                 home-files-service-type
                                 `((".config/zathura" ,(local-file (string-append (current-source-directory) "/files/config/zathura"
                                                                   #:recursive? #t)))))
                 (simple-service 'btop-config
                                 home-files-service-type
                                 `((".config/btop" ,(local-file (string-append (current-source-directory) "/files/config/btop"
                                                                               #:recursive? #t)))))
                 (simple-service 'scripts-config
                                 home-files-service-type
                                 `((".scripts" ,(local-file (string-append (current-source-directory) "/files/scripts"
                                                                           #:recursive? #t)))))
                 (simple-service 'wallpapers-config
                                 home-files-service-type
                                 `(("Pictures/Wallpapers" ,(local-file (string-append (current-source-directory) "/files/pictures"
                                                                                      #:recursive? #t)))))
                 ))
 )
