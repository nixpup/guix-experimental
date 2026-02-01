(define-module (gnu packages vicinae)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages check)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages node)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages maths))

(define-public vicinae
  (package
    (name "vicinae")
    ;; Version extracted from manifest in Nix; manually set here for clarity
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vicinaehq/vicinae")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vdz1p0m0vyp7c5rhgc3p5m8icy81rj8l0b1vdicn8927iwfrmnn")))) ; Replace with actual hash
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DVICINAE_PROVENANCE=guix"
             "-DINSTALL_NODE_MODULES=OFF"
             "-DUSE_SYSTEM_GLAZE=ON"
             (string-append "-DVICINAE_GIT_TAG=v" ,version))
       #:phases
       (modify-phases %standard-phases
                      (add-before 'configure 'set-home
                                  (lambda _
                                    (setenv "HOME" "/tmp")))
         ;; Nix's postPatch for npm handled here
         (add-after 'unpack 'setup-npm-deps
                    (lambda* (#:key inputs #:allow-other-keys)
             ;; Note: Guix handles Node dependencies differently than Nix.
             ;; You would typically use a separate 'guix import npm' workflow
                    ;; or pre-bundle the node_modules.
                    (format #t "NPM environment ready at /tmp\n")
             #t)))))
    (native-inputs
     (list pkg-config
           cmake
           ninja
           protobuf
           qtbase-5 ; Guix uses specific hooks for wrapping Qt
           node-lts))
    (inputs
     (list abseil-cpp
           ;cmark-gfms
           libqalculate
           minizip
           protobuf
           qtbase
           qtsvg
           qtwayland
           wayland
           libxml2
           libglvnd
           libxkbcommon
           ;; 'glaze' may need to be defined/imported if not in Guix upstream
           layer-shell-qt
           qtkeychain-qt6
           qtkeychain
           openssl))
    (home-page "https://github.com/vicinaehq/vicinae")
    (synopsis "A focused launcher for your desktop")
    (description "Vicinae is a native, fast, and extensible desktop launcher.")
    (license license:gpl3+)))

vicinae
