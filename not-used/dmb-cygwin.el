
(require 'cygwin-mount)
(setq cygwin-mount-cygwin-bin-directory  (concat cygwin-dir "/bin"))
(if (file-exists-p cygwin-mount-cygwin-bin-directory)
    (cygwin-mount-activate))



(when (not is-darwin)
  (progn
    (setenv "PATH" (concat cygwin-dir "/bin" ";" (getenv "PATH")))
    (setenv "PATH" (concat (getenv "EMACSPATH") ";" (getenv "PATH")))
    ;;(setenv "PATH" (concat devtools-dir "/svn-1.4.3/bin" ";" (getenv "PATH")))
))
