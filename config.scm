(add-to-load-path (dirname (current-filename)))

(use-modules (gnu)
             (gnu system)
             (gnu system nss)
             (gnu packages)
             (gnu packages xorg)
             (gnu packages certs)
             (gnu packages shells)
             (gnu services)
             (gnu services xorg)
             (gnu services desktop)
             (gnu services nix) ; Nix Package Manager
             (gnu services sound) ; PipeWire Audio
             (gnu services audio)
             (gnu services networking)
             (gnu utils)
             ;; Nonguix Modules
             (nongnu packages linux)
             (nongnu packages nvidia)
             (nongnu services nvidia)
             (nongnu system linux-initrd)
             (nonguix transformations)
             ;; Custom Modules/Packages
             (files packages NaitreHUD package))
(use-service-modules desktop networking ssh xorg dbus)
(use-package-modules wm bootloaders certs shells editors version-control xorg pipewire)

;;---
;; After first install, run:
;;  'guix pull'
;;  'sudo guix system reconfigure /etc/config.scm'
;;---
;; Useful Guix Commands include:
;;  + List System Generations:
;;    'guix system list-generations'
;;  + Describe System Generation:
;;    'guix system describe'
;;  + Delete Generations older than 1 Month:
;;    'sudo guix system delete-generations 1m'
;;  + Roll Back to Older Generation:
;;    'sudo guix system roll-back'
;;  + Guix Garbage Collector:
;;    'guix gc -d 1m -F 10G'
;;    - Delete Generations older than 1 Month, and try to free up at least 10GiB.
;;  + Deduplicate/Optimize Guix Store:
;;    'guix gc --optimize'
;;  + Reconfigure Guix Home:
;;    'guix home reconfigure /etc/home.scm'
;;  + Describe/List Channels:
;;    'guix describe'
;;  + Upgrade Packages:
;;    'guix upgrade'
;;---
;; Reformat/Style Scheme File:
;;  'guix style -f config.scm'
;;  - This can help to find syntax errors, such as misplaced brackets, easier.
;;---

; Added ((compose (nonguix-transformation-nvidia)) - Experimental!
; If to be removed, remember to remove last bracket at end of file as well.
(((compose (nonguix-transformation-nvidia))(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list intel-microcode linux-firmware %base-firmware))
 ;; Nvidia
 (kernel-arguments (append
                    '("modprobe.blacklist=nouveau")
                    %default-kernel-arguments))
 (kernel-loadable-modules (list nvidia-driver))
 (host-name "guix")
 (timezone "Europe/Berlin")
 (locale "en_US.utf8")
 (keyboard-layout (keyboard-layout "us" "colemak"))

 ;; Bootloader
 ;- (U)EFI
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets '("/boot/efi"))))
 ;- Legacy/BIOS
 ;(bootloader (bootloader-configuration
 ;             (bootloader grub-bootloader)
 ;             (targets '("/dev/sda1"))))

 ;; File Systems
 ;- Regular
 (file-systems (cons* (file-system
                       (mount-point "/")
                       (device (file-system-label "guix-root"))
                       (type "ext4"))
                      (file-system
                       (mount-point "/boot/efi")
                       (device (file-system-label "guix-efi"))
                       ;; or: (device (uuid "PARTITION_UUID" 'fat32))
                       (type "vfat"))
                      %base-file-systems))
 ;- Encrypted
 ;(file-systems (append
 ;               (list (file-system
 ;                      (device "/dev/mapper/cryptroot")
 ;                      (mount-point "/")
 ;                      (type "ext4")
 ;                      (dependencies mapped-devices))
 ;                     (file-system
 ;                      (device (uuid "PARTITION_1_UUID" 'fat32))
 ;                      (mount-point "/boot/efi")
 ;                      (type "vfat")))
 ;               %base-file-systems))

 ;; Users
 (users (cons (user-account
               (name "puppy")
               (comment "Puppy")
               (group "users")
               (home-directory "/home/puppy")
               (supplementary-groups '("wheel" "netdev" "audio" "video" "input" "tty" "nix-users"))
               (shell (file-append zsh "/bin/zsh")))
              %base-user-accounts))

 ;; Packages
 (packages (append (map specification->package+output
                        '("zsh"
                          "naitre-git"
                          "nix"
                          "nixfmt"
                          "xmonad"
                          "ghc-xmonad-contrib"
                          "pipewire"
                          "wireplumber"
                          "flatpak"
                          "polybar"
                          "btop"
                          "git"
                          "wpa-supplicant"
                          "curl"
                          "emacs"
                          "nss-certs"
                          "firefox"
                          "icecat"
                          "icedove"
                          "guix-backgrounds"
                          "icedove-wayland"
                          "kitty"
                          "font-dejavu"
                          "dunst"
                          "feh"
                          "zathura"
                          "texlive"
                          "emacs-org-texlive-collection"
                          "rsync"
                          "eza"
                          "bat"
                          "zoxide"
                          "bottom"
                          "redshift"
                          "flameshot"
                          "mpv-nvidia" ; From Nonguix, .-nvidia Variant
                          "mpvpaper"
                          "sway-audio-idle-inhibit"
                          "imv"
                          "steam-nvidia" ; From Nonguix, .-nvidia Variant
                          "cmus"
                          "alsa-utils"
                          "hyfetch"
                          "neofetch"
                          "fastfetch"
                          "pfetch"
                          "pavucontrol"
                          "pulseaudio"
                          "pulsemixer"
                          "krita"
                          "pciutils"
                          "fd"
                          "imagemagick"
                          "yt-dlp"
                          "wl-clipboard"
                          "dank-material-shell"
                          "grim"
                          "grimblast"
                          "unrar-free"
                          "unzip"
                          "p7zip"
                          "openssl"
                          "coreutils"
                          "thunar"
                          "tumbler"
                          "dbus"
                          "ntfs-3g"
                          "cryptsetup"
                          "testdisk"
                          "encfs"
                          "usbutils"
                          "pamixer"
                          "node"
                          "glib"
                          "librewolf"
                          "torbrowser"
                          "rust"
                          "rust-analyzer"
                          "gcc"
                          "wf-recorder"
                          "swaybg"
                          "waybar"
                          "swayidle"
                          "hyprlock"
                          "wlsunset"
                          "wofi"
                          "wtype"
                          "mako"
                          "xwayland-satellite"
                          "xwayland-run"
                          "slurp"
                          "grimshot"
                          "hyprpicker"
                          "cliphist"
                          "fuzzel"
                          "xdg-desktop-portal"
                          "xdg-desktop-portal-wlr"
                          "ripgrep"
                          "discord" ; From Git Repo
                          ))))

 ;;---
 ;; WiFi Configuration:
 ;; + 'rfkill unblock all'
 ;; + 'ifconfig -a' | Find WiFi Device/Cards Name
 ;; + 'wpa_supplicant -c wifi.conf -i interface1s0 -B'
 ;; + 'dhclient -v interface1s0'
 ;;---
 ;; wifi.conf:
 ;; + network={
 ;; +   ssid="ssid-name"
 ;; +   ket_mgmt=WPA-PSK
 ;; +   psk="password"
 ;; + }
 ;;---

 ;; Services
 (services
  (append
   (list
    (service gdm-service-type
             (gdm-configuration
             (wayland? #t)))
    (service nvidia-driver-service-type)
    (service kernel-module-loader-service-type
             '("ipmi_devintf"
               "nvidia"
               "nvidia_modeset"
               "nvidia_uvm"))
    (service nix-service-type)
    (service pipewire-service-type)
    (service alsa-service-type
             (alsa-configuration
              (jack? #t)))
    (service dhcpcd-service-type)
    (service network-manager-service-type)
    (set-xorg-configuration
     (xorg-configuration
      (keyboard-layout (keyboard-layout "us" "colemak"))
      (modules (cons nvidia-driver %default-xorg-modules))
      (drivers '("nvidia")))))

   (modify-services %desktop-services
                    (delete pulseaudio-service-type))

   (modify-services %base-services ;; or: %desktop-services
                    (guix-service-type config =>
                                       (guix-configuration
                                        (inherit config)
                                        (substitute-urls
                                         (append (list "https://ci.guix.gnu.org"
                                               "https://berlin.guix.gnu.org"
                                               "https://bordeaux.guix.gnu.org"
                                               "https://substitutes.nonguix.org"
                                               "https://hydra-guix-129.guix.gnu.org")
                                                 %default-substitute-urls))))
                    (mingetty-service-type config =>
                                           (mingetty-configuration
                                            (inherit config)
                                            (auto-login "puppy"))))
   ))
 )
 )
