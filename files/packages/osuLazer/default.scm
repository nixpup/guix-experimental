(define-module (my-config osu-lazer)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages appimage-runtime)
  #:use-module (gnu packages fuse))

(define-public osu-lazer
  (package
    (name "osu-lazer")
    (version "2025.12.1")  ;; Update with latest release
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ppy/osu/releases/download/"
                           version "/osu.AppImage"))
       (sha256
        (base32 "UPDATE_WITH_ACTUAL_HASH"))  ;; Run `guix download URL` to get this
       (modules '((guix build utils)))
       (snippet
        '(begin
           (use-modules (guix build utils))
           (substitute* "osu.AppImage"
             (("#!/bin/bash") "#!/usr/bin/env bash"))))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-1))
                   (let ((out (assoc-ref %outputs "out"))
                         (source (assoc-ref %build-inputs "source"))
                         (appimage-runtime (assoc-ref %build-inputs "appimage-runtime")))
                     (mkdir-p (string-append out "/bin"))
                     (mkdir-p (string-append out "/share/applications"))
                     
                     ;; Copy AppImage
                     (copy-file source (string-append out "/bin/osu-lazer.AppImage"))
                     (chmod (string-append out "/bin/osu-lazer.AppImage") #o755)
                     
                     ;; Create wrapper script
                     (call-with-output-file (string-append out "/bin/osu-lazer")
                       (lambda (port)
                         (format port "#!/bin/sh~@
exec ~a/bin/appimage-run ~a/bin/osu-lazer.AppImage \"$@\"~%"
                                 appimage-runtime out)))
                     (chmod (string-append out "/bin/osu-lazer") #o755)
                     
                     ;; Desktop file
                     (call-with-output-file (string-append out "/share/applications/osu-lazer.desktop")
                       (lambda (port)
                         (format port 
"[Desktop Entry]~@
Name=osu!lazer~@
Exec=~a/bin/osu-lazer~@
Icon=osu~@
Type=Application~@
Categories=Game;~@
Comment=rhythm is just a click away~%" out)))
                     #t))))
    (inputs (list appimage-runtime-type2 fuse))  ;; FUSE2 support
    (home-page "https://osu.ppy.sh/")
    (synopsis "Rhythm is just a click away")
    (description "osu!lazer is the open-source rewrite of the famous osu! rhythm game.")
    (license license:expat)))
