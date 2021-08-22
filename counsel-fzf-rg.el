;;; counsel-fzf-rg.el --- ripgrep fzf search counsel.el extension  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Illia A. Danko <i@idanko.net>
;; URL: https://github.com/idanko/counsel-fzf-rg.el

;;; Commentary:

;; counsel-fzf-rg.el extends the ripgrep search capabilities by wrapping results with fzf.
;; One of the use cases would be to do fuzzy search filename along with its content.

;;; Code:

(require 'counsel)

(defvar counsel-fzf-rg-cmd "rg --no-column --line-number --no-heading --color=never --smart-case -- \"%s\""
  "rg command and arguments. Used along with fzf.")

(ivy-configure 'counsel-fzf-rg
  :occur #'counsel-ag-occur
  :unwind-fn #'counsel--grep-unwind
  :display-transformer-fn #'counsel-git-grep-transformer
  :grep-p t
  :exit-codes '(1 "No matches found"))

;;;###autoload
(defun counsel-fzf-rg (&optional initial-input initial-directory fzf-prompt)
  "Grep for a string in the current directory using `fzf' wrap over `rg'.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
FZF-PROMPT, if non-nil, is passed as `ivy-read' prompt argument."
  (interactive
   (let ((fzf-basename (car (split-string counsel-fzf-rg-cmd))))
     (list nil
           (when current-prefix-arg
             (counsel-read-directory-name (concat
                                           fzf-basename
                                           " in directory: "))))))
  (counsel-require-program counsel-fzf-rg-cmd)
  (setq counsel--fzf-dir
        (or initial-directory
            (funcall counsel-fzf-dir-function)))
  (let* ((fzf-command-env "FZF_DEFAULT_COMMAND")
         (previous-fzf-rg-cmd (getenv fzf-command-env)))
    (unwind-protect
        (progn
          (setenv fzf-command-env (format counsel-fzf-rg-cmd initial-input))
          (ivy-read (or fzf-prompt "fuzzy rg: ")
                    #'counsel-fzf-function
                    :initial-input initial-input
                    :re-builder #'ivy--regex-fuzzy
                    :dynamic-collection t
                    :action #'counsel-git-grep-action
                    :caller 'counsel-fzf-rg))
      (setenv fzf-command-env previous-fzf-rg-cmd))))

(provide 'counsel-fzf-rg)
