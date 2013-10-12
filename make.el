;;; Evaluate this entire file to prepare the package.
;;; Switch to the *clips-mode-build* buffer to see the results.

(setq TMPDIR "~/tmp")
;; TODO: Determine this from a file dynamically
(setq RELNAM "clips-mode-0.7")
(setq PKGDIR (concat TMPDIR "/" RELNAM))
(setq PKGNAM (concat PKGDIR ".tar"))

;; Check if /home/<username>/tmp exists and create it if it does not.
;; If this fails, the script will expectedly and rightly fail.
(if (not (file-accessible-directory-p TMPDIR))
    (make-directory TMPDIR))

;; Check if a build directory exists, and if it does, delete it, otherwise
;; create from scratch.
(if (not (file-accessible-directory-p PKGDIR))
    (make-directory PKGDIR)
  (delete-directory PKGDIR t))

;; Copy over files
(copy-file "clips-mode-pkg.el" (concat PKGDIR "/" "clips-mode-pkg.el"))
(copy-file "clips-mode.el" (concat PKGDIR "/" "clips-mode.el"))
(copy-file "inf-clips.el" (concat PKGDIR "/" "inf-clips.el"))
(copy-file "COPYING" (concat PKGDIR "/" "COPYING"))

;; Check if the package file already exists and delete it if it does.
(if (file-exists-p PKGNAM)
    (delete-file PKGNAM))

;; Create the TAR package
(start-process "clips-mode-build"
               (get-buffer-create "*clips-mode-build*")
               "/bin/bash"
               "-c"
               (concat "tar -cvf " PKGNAM " " PKGDIR))
