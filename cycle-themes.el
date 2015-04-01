
;;; cycle-themes.el --- Minor mod for theme cycling

;; Copyright (C) 2015 Katherine Whitlock
;;
;; Authors: Katherine Whitlock <toroidalcode@gmail.com>
;; URL: http://github.com/toroidal-code/cycle-themes.el
;; Version: 1.0
;; Keywords: Themes, Utility, Global Minor Mode

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Allows switching between themes easily.

;;; Installation

;; In your emacs config, define a list of themes you want to be
;; able to switch between. This should ideally happen before
;;`require`ing cycle-themes. Then, enable the global minor mode.
;;
;;     (setq cycle-themes-theme-list
;;           '(leuven monokai solarized-dark))
;;     (require 'cycle-themes)
;;     (cycle-themes-mode)
;;
;; `cycle-themes' is bound to 'C-c C-t' by default.
;;
;; You can optionally add hooks to be run after switching themes:
;;
;; (add-hook 'cycle-themes-after-cycle-hook
;;           #'(lambda () (Do-something-fn ...)))
;;

;;; License:

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:

(require 'cl-lib)

(defgroup cycle-themes nil
  "The cycle-themes group"
  :group 'emacs
  :prefix "cycle-themes-")

(defcustom cycle-themes-after-cycle-hook nil
  "Hooks that are run after switching themes"
  :group 'cycle-themes
  :type 'hook)

(defcustom cycle-themes-theme-list nil
  "The list of themes to cycle through 
on calling `cycle-themes'"
  :group 'cycle-themes
  :type '(list symbol))

(defcustom cycle-themes-current-theme
  (unless (or (null cycle-themes-theme-list)
              (not (custom-theme-p (car cycle-themes-theme-list))))
    (car cycle-themes-theme-list))
  "Our current theme"
  :type 'symbol
  :group 'cycle-themes)

(defun cycle-themes ()
  "Cycle through a list of themes, my-themes"
  (interactive)
  (cond ((and cycle-themes-theme-list cycle-themes-current-theme)
         (progn
           (when (custom-theme-p cycle-themes-current-theme)
             (disable-theme cycle-themes-current-theme))
           ;; move to the next valid theme in the list
           (let ((start-theme cycle-themes-current-theme))
             (while (progn
                      ;; Fancy way to move to the next theme
                      ;; with modular arithmetic so we never reach the end.
                      (setq cycle-themes-current-theme
                            (nth (mod (1+ (cl-position cycle-themes-current-theme cycle-themes-theme-list))
                                      (length cycle-themes-theme-list))
                                 cycle-themes-theme-list))
                      ;; Make sure we didn't loop all the way through
                      (when (eq cycle-themes-current-theme start-theme)
                        (error "No valid themes in cycle-themes-theme-list"))
                      (not (custom-theme-p cycle-themes-current-theme)))))
           ;; Load the theme we found.
           (load-theme cycle-themes-current-theme t)
           (run-hooks 'cycle-themes-after-cycle-hook)))
        
        ;; There isn't a current theme, but we've got a valid list
        ((and (not cycle-themes-current-theme) cycle-themes-theme-list)
         (progn (setq cycle-themes-current-theme (car cycle-themes-theme-list))
                (cycle-themes)))

        ;; No themes in the cycle list
        (t (error "No themes in cycle-themes-list"))))

;;;###autoload
(define-minor-mode cycle-themes-mode
  "Minor mode for cycling between themes."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-t") 'cycle-themes)
            map)
  :global t
  (when (and (not (null cycle-themes-current-theme))
            (custom-theme-p cycle-themes-current-theme))
    (load-theme cycle-themes-current-theme)
    (run-hooks 'cycle-themes-after-cycle-hook)))

(provide 'cycle-themes)
;;; cycle-themes.el ends here
