(define-module (gnu packages vicinae)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages icu4c) ;; Added for libicu
  #:use-module (gnu packages elf))

(define-public vicinae
  (package
    (name "vicinae")
    (version "0.19.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/vicinaehq/vicinae/releases/download/v"
                                  version "/vicinae-linux-x86_64-v" version ".tar.gz"))
              (sha256
               (base32
                "14w4rfj3wmqn1wixja1sgll5zxbpg0v4s1ha8b9q8xy7lwg64rky"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (srfi srfi-1))
         (let* ((source   (assoc-ref %build-inputs "source"))
                (tar      (assoc-ref %build-inputs "tar"))
                (gzip     (assoc-ref %build-inputs "gzip"))
                (patchelf (assoc-ref %build-inputs "patchelf"))
                (glibc    (assoc-ref %build-inputs "glibc"))
                (bash     (assoc-ref %build-inputs "bash-minimal"))
                (out      (assoc-ref %outputs "out"))
                (bin-dir  (string-append out "/bin"))
                (lib-dir  (string-append out "/lib"))
                (bin-path (string-append bin-dir "/vicinae")))

           (setenv "PATH" (string-append tar "/bin:" gzip "/bin:" patchelf "/bin"))
           (mkdir-p bin-dir)
           (mkdir-p lib-dir)

           (invoke "tar" "xvf" source)

           (let ((executable (car (find-files "." "vicinae$"))))
             (copy-file executable bin-path)
             (chmod bin-path #o755))

           ;; Patch the dynamic linker
           (let ((ld-linux (string-append glibc "/lib/ld-linux-x86-64.so.2")))
             (invoke "patchelf" "--set-interpreter" ld-linux bin-path))

           ;; Compatibility Shim: libqalculate
           (let* ((qalculate (assoc-ref %build-inputs "libqalculate"))
                  (actual-lib (car (find-files (string-append qalculate "/lib") "libqalculate.so.*"))))
             (symlink actual-lib (string-append lib-dir "/libqalculate.so.23")))

           ;; Compatibility Shim: ICU (libicuuc and libicui18n)
           (let* ((icu (assoc-ref %build-inputs "icu4c"))
                  (icu-lib-path (string-append icu "/lib")))
             ;; We link the common and i18n libraries to the .78 version expected by the binary
             (symlink (car (find-files icu-lib-path "libicuuc.so.*"))
                      (string-append lib-dir "/libicuuc.so.78"))
             (symlink (car (find-files icu-lib-path "libicui18n.so.*"))
                      (string-append lib-dir "/libicui18n.so.78"))
             (symlink (car (find-files icu-lib-path "libicudata.so.*"))
                      (string-append lib-dir "/libicudata.so.78")))

           ;; QT6
           (let* ((qtbase (assoc-ref %build-inputs "qtbase"))
                  (qt-lib (string-append qtbase "/lib")))
             (for-each (lambda (lib)
                         (let ((actual (first (find-files qt-lib (string-append lib ".so.6$")))))
                           (symlink actual (string-append lib-dir "/" lib ".so.6"))))
                       '("libQt6Core" "libQt6Widgets" "libQt6Gui" "libQt6Network" "libQt6Sql")))
           ;; Wrap with all required libraries
           (let ((qtbase       (assoc-ref %build-inputs "qtbase"))
                 (qtsvg        (assoc-ref %build-inputs "qtsvg"))
                 (qtwayland    (assoc-ref %build-inputs "qtwayland"))
                 (wayland      (assoc-ref %build-inputs "wayland"))
                 (layer-shell  (assoc-ref %build-inputs "layer-shell-qt"))
                 (minizip      (assoc-ref %build-inputs "minizip"))
                 (zlib         (assoc-ref %build-inputs "zlib"))
                 (libxml2      (assoc-ref %build-inputs "libxml2"))
                 (openssl      (assoc-ref %build-inputs "openssl"))
                 (libxkbcommon (assoc-ref %build-inputs "libxkbcommon"))
                 (libxcb       (assoc-ref %build-inputs "libxcb"))
                 (libglvnd     (assoc-ref %build-inputs "libglvnd"))
                 (qtkeychain-qt6 (assoc-ref %build-inputs "qtkeychain-qt6"))
                 (gcc-lib      (assoc-ref %build-inputs "gcc-lib"))
                 (mesa         (assoc-ref %build-inputs "mesa")))
             (wrap-program bin-path
               #:sh (string-append bash "/bin/bash")
               `("LD_LIBRARY_PATH" ":" prefix
                 (,(string-append lib-dir ":"
                                  qtbase "/lib:"
                                  qtsvg "/lib:"
                                  qtwayland "/lib:"
                                  wayland "/lib:"
                                  layer-shell "/lib:"
                                  libglvnd "/lib:"
                                  minizip "/lib:"
                                  libxkbcommon "/lib:"
                                  libxcb "/lib:"
                                  zlib "/lib:"
                                  libxml2 "/lib:"
                                  qtkeychain-qt6 "/lib:"
                                  openssl "/lib:"
                                  gcc-lib "/lib:"
                                  mesa "/lib")))))
           #t))))
    (native-inputs
     (list tar gzip patchelf bash-minimal))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtwayland" ,qtwayland)
       ("wayland" ,wayland)
       ("libglvnd" ,libglvnd)
       ("layer-shell-qt" ,layer-shell-qt)
       ("minizip" ,minizip)
       ("zlib" ,zlib)
       ("icu4c" ,icu4c) ;; Added icu4c
       ("libqalculate" ,libqalculate)
       ("libxml2" ,libxml2)
       ("openssl" ,openssl)
       ("libxkbcommon" ,libxkbcommon)
       ("qtkeychain-qt6" ,qtkeychain-qt6)
       ("libxcb" ,libxcb)
       ("glibc" ,glibc)
       ("mesa" ,mesa)
       ("gcc-lib" ,gcc "lib")))
    (home-page "https://github.com/vicinaehq/vicinae")
    (synopsis "A focused launcher for your desktop")
    (description "Vicinae is a native, fast, and extensible desktop launcher.")
    (license license:gpl3+)))

vicinae
