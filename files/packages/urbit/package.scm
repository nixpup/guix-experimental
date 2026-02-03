(define-module (packages urbit)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public urbit
  (package
    (name "urbit")
    (version "4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/urbit/vere/releases/download/vere-v"
                           version "/"
                           (let ((system (or (%current-system) (%current-target-system))))
                             (cond
                              ((string-prefix? "x86_64-linux" system) "linux-x86_64")
                              ((string-prefix? "aarch64-linux" system) "linux-aarc64")
                              ((string-prefix? "x86_64-darwin" system) "macos-x86_64")
                              ((string-prefix? "aarch64-darwin" system) "macos-aarch64")
                              (else (error "Unsupported system" system))))
                           ".tgz"))
       (sha256
        (base32
         ;; Note: You will need to convert the Nix SHA256 to Guix base32 format
         ;; using `guix hash -rx .` on the extracted source or `nix-hash --to-base32`.
         "0cmgm0cjcj0089dd7dnncl5lhw6ji669byjbzd72va51abdirnkq"))))
    (build-system copy-build-system) ; Guix equivalent for binary installs
    (arguments
     '(#:phases
       (modify-phases %standard-phases
                      (add-after 'install 'rename-binary
                                 (lambda* (#:key outputs #:allow-other-keys)
                                          (let* ((out (assoc-ref outputs "out"))
                                                 (bin (string-append out "/bin")))
                                            (with-directory-excursion bin
                                                                      (let ((vere-bin (car (find-files "." "^vere.*"))))
                                                                        (rename-file vere-bin "urbit"))))))
                      #:install-plan
                      '(("./" "bin/" #:include-regexp ("^vere.*$"))))))

    (home-page "https://urbit.org")
    (synopsis "Urbit binary execution environment")
    (description "A clean-slate OS and network for the 21st century.")
    (license license:expat)))

urbit
