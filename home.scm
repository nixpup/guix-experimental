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
                            (zshrc (list (local-file "files/config/zshrc")))
                            )
                           )
                 (simple-service 'mpv-config
                                 home-files-service-type
                                 `((".config/mpv" ,(local-file "files/config/mpv"
                                                               #:recursive? #t))))
                 (simple-service 'discord-config
                                 home-files-service-type
                                 `((".config/vesktop" ,(local-file "files/config/discord"
                                                                   #:recursive? #t))))
                 (simple-service 'fastfetch-config
                                 home-files-service-type
                                 `((".config/fastfetch" ,(local-file "files/config/fastfetch"
                                                                     #:recursive? #t))))
                 (simple-service 'hyprlock-config
                                 home-files-service-type
                                 `((".config/hyprlock" ,(local-file "files/config/hyprlock"
                                                                    #:recursive? #t))))
                 (simple-service 'kitty-config
                                 home-files-service-type
                                 `((".config/kitty" ,(local-file "files/config/kitty"
                                                                 #:recursive? #t))))
                 (simple-service 'waybar-config
                                 home-files-service-type
                                 `((".config/waybar" ,(local-file "files/config/waybar"
                                                                  #:recursive? #t))))
                 (simple-service 'zathura-config
                                 home-files-service-type
                                 `((".config/zathura" ,(local-file "files/config/zathura"
                                                                   #:recursive? #t))))
                 (simple-service 'btop-config
                                 home-files-service-type
                                 `((".config/btop" ,(local-file "files/config/btop"
                                                                #:recursive? #t))))
                 ))
 )
