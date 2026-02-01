(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services shells))

(home-environment
 (packages (specifications->packages
            (list "zsh"
                  "git"
                  "emacs")))

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
