;;; counsel-fzf-rg.el --- ripgrep fzf search counsel.el extension  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Illia A. Danko <i@idanko.net>
;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/idanko/counsel-fzf-rg.el

;;; Commentary:

;; counsel-fzf-rg.el extends ripgrep search capabilities by wrapping results in
;; fzf. One of use cases is to make the fuzzy finding filename, plus line
;; number, plus line's content.


;;; Code:

(require 'counsel)

(defvar counsel-fzf-rg-cmd "rg --no-column --line-number --no-heading --color=never --smart-case -- \"%s\""
  "rg command. Will be wrapped in fzf.")

(ivy-configure 'counsel-fzf-rg
  :occur #'counsel-ag-occur
  :unwind-fn #'counsel--grep-unwind
  :display-transformer-fn #'counsel-git-grep-transformer
  :grep-p t
  :exit-codes '(1 "No matches found"))

(defun counsel-fzf-rg-action (x)
  "Go to candidate X."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x)))
      (find-file (expand-file-name
                  file-name
                  counsel--fzf-dir))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (when (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
        (when swiper-goto-start-of-match
          (goto-char (match-beginning 0))))
      (swiper--ensure-visible)
      (run-hooks 'counsel-grep-post-action-hook)
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

;;;###autoload
(defun counsel-fzf-rg (&optional initial-input initial-directory fzf-prompt)
  "Search for a string in `initial-directory' directory using `rg' wrapped in `fzf'.
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
         (previous-fzf-rg-cmd (getenv fzf-command-env))
         (empty-input? (string-empty-p initial-input))
         ;; Use \S to filter out empty lines.
         (fzf-command (format counsel-fzf-rg-cmd (if empty-input?
                                                     "\\S"
                                                   initial-input))))
    (unwind-protect
        (progn
          (setenv fzf-command-env fzf-command)
          (ivy-read (or fzf-prompt "fuzzy rg: ")
                    #'counsel-fzf-function
                    :initial-input initial-input
                    :re-builder #'ivy--regex-fuzzy
                    :dynamic-collection t
                    :action #'counsel-fzf-rg-action
                    :caller 'counsel-fzf-rg))
      (setenv fzf-command-env previous-fzf-rg-cmd))))

(provide 'counsel-fzf-rg)
