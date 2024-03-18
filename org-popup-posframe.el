;;; org-popup-posframe.el --- Show org mode popup buffers in posframe -*- lexical-binding: t; -*-

;;; Commentary:

;; Display org mode popup buffers in posframe

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;; Installation and usage

;;;; Tips

;;; Code:

;;;; Requirements

(require 'posframe)
(require 'org)

;;;; Customs & Faces

(defgroup org-popup-posframe nil
  "Show org mode popup buffers in posframe."
  :group 'org-popup-posframe
  :prefix "org-popup-posframe")

(defcustom org-popup-posframe-font nil
  "The font used by org-popup-posframe."
  :group 'org-popup-posframe
  :type 'string)

(defvar org-popup-posframe--org-mks-poshandler nil)

(defcustom org-popup-posframe-org-capture-poshandler #'posframe-poshandler-window-bottom-right-corner
  "The posframe poshandler of org-insert."
  :group 'org-popup-posframe
  :type 'function)

(defcustom org-popup-posframe-org-insert-link-poshandler #'posframe-poshandler-window-bottom-right-corner
  "The posframe poshandler of org-insert-structure-template."
  :group 'org-popup-posframe
  :type 'function)

(defcustom org-popup-posframe-org-insert-structure-template-poshandler #'posframe-poshandler-point-1
  "The posframe poshandler of org-insert-structure-template."
  :group 'org-popup-posframe
  :type 'function)

(defcustom org-popup-posframe-org-todo-poshandler #'posframe-poshandler-point-1
  "The posframe poshandler of org-todo."
  :group 'org-popup-posframe
  :type 'function)

(defcustom org-popup-posframe-min-width 0
  "The width of org-popup-min-posframe."
  :group 'org-popup-posframe
  :type 'number)

(defcustom org-popup-posframe-min-height 0
  "The height of org-popup-min-posframe."
  :group 'org-popup-posframe
  :type 'number)

(defcustom org-popup-posframe-border-width 1
  "The border width used by org-popup-posframe.
When 0, no border is showed."
  :group 'org-popup-posframe
  :type 'number)

(defcustom org-popup-posframe-parameters nil
  "The frame parameters used by org-popup-posframe."
  :group 'org-popup-posframe
  :type 'string)

(defface org-popup-posframe
  '((t (:inherit default)))
  "Face used by the org-popup-posframe."
  :group 'org-popup-posframe)

(defface org-popup-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face used by the org-popup-posframe's border."
  :group 'org-popup-posframe)



;;;; Functions

(defun org-popup-posframe--show-buffer (buffer poshandler)
  (when (posframe-workable-p)
    (posframe-show buffer
		   :position (point)
		   :poshandler poshandler
		   :font org-popup-posframe-font
		   :background-color (face-attribute 'org-popup-posframe :background nil t)
		   :foreground-color (face-attribute 'org-popup-posframe :foreground nil t)
		   :min-width org-popup-posframe-min-width
		   :min-height org-popup-posframe-min-height
		   :internal-border-width org-popup-posframe-border-width
		   :internal-border-color (face-attribute 'org-popup-posframe-border :background nil t)
		   :override-parameters org-popup-posframe-parameters)))


(defun org-popup-posframe--org-mks-advice (func table title &optional prompt specials)
  (let ((original-buffer (current-buffer))
        (buffer (get-buffer-create "*Org Select*")))
    (cl-letf (;; set buffer to "*Org Select*"
                ((symbol-function 'org-switch-to-buffer-other-window) #'set-buffer)
                ((symbol-function 'org-fit-window-to-buffer)
                 (lambda ()
                   ;; set buffer back
                   (set-buffer original-buffer)
                   ;; posframe show
                   (org-popup-posframe--show-buffer
                    buffer
                    org-popup-posframe--org-mks-poshandler))))
        (funcall func table title prompt specials))))


(defun org-popup-posframe--org-capture-advice (&rest r)
  (ignore r)
  (setq org-popup-posframe--org-mks-poshandler
        org-popup-posframe-org-capture-poshandler))


(defun org-popup-posframe--org-insert-structure-template-mks-advice (&rest r)
  (ignore r)
  (setq org-popup-posframe--org-mks-poshandler
        org-popup-posframe-org-insert-structure-template-poshandler))


(defun org-popup-posframe--org-fast-todo-selection-advice (func &optional current-todo-keyword)
  ;; If using (with-current-buffer buffer ... ) or (set-buffer buffer)
  ;; before calling org-fast-todo-selection
  ;; inside org-fast-todo-selection, (apply max ...) will error
  (let ((original-buffer (current-buffer))
        (buffer (get-buffer-create " *Org todo*")))
    (fset 'original-set-window-buffer (symbol-function 'set-window-buffer))
    (unwind-protect
        (cl-letf* (((symbol-function 'delete-other-windows) (lambda () nil))
                   ((symbol-function 'split-window-below) (lambda () nil))
                   ((symbol-function 'set-window-buffer) (lambda (a b) (ignore a b)))
                   ;; set buffer to " *Org todo*"
                   ((symbol-function 'org-switch-to-buffer-other-window) #'set-buffer)
                   ((symbol-function 'org-fit-window-to-buffer)
                    (lambda ()
                      ;; set buffer back
                      (set-buffer original-buffer)
                      (cl-letf (((symbol-function 'set-window-buffer)
                                 ;; avoid set-window-buffer redefinition
                                 #'original-set-window-buffer))
                        ;; posframe show
                        (org-popup-posframe--show-buffer
                         buffer
                         org-popup-posframe-org-todo-poshandler)))))
          (funcall func current-todo-keyword))
      (when buffer (kill-buffer buffer)))))


(defun org-popup-posframe--org-insert-link-advice
    (func &optional COMPLETE-FILE LINK-LOCATION DESCRIPTION)
  (let ((original-buffer (current-buffer))
        (buffer (get-buffer-create "*Org Links*")))
    (cl-letf (;; set buffer to "*Org Select*"
              ((symbol-function 'org-switch-to-buffer-other-window)
               (lambda (a) (ignore a)))
              ((symbol-function 'org-format-prompt)
               (lambda (PROMPT DEFAULT &rest FORMAT-ARGS)
                 (org-popup-posframe--show-buffer
                  buffer
                  org-popup-posframe-org-insert-link-poshandler)
                 (format-prompt PROMPT DEFAULT FORMAT-ARGS))))
      (funcall func COMPLETE-FILE LINK-LOCATION DESCRIPTION))))


;;;###autoload
(define-minor-mode org-popup-posframe-mode
  "Show org mode popup buffers in posframe"
  :group 'org-popup-posframe
  :global t
  :lighter nil
  (if org-popup-posframe-mode
      (progn
        (advice-add 'org-mks :around
                    #'org-popup-posframe--org-mks-advice)
        (advice-add 'org-capture :before
                    #'org-popup-posframe--org-capture-advice)
        (advice-add 'org--insert-structure-template-mks :before
                    #'org-popup-posframe--org-insert-structure-template-mks-advice)
        (advice-add 'org-fast-todo-selection :around
                    #'org-popup-posframe--org-fast-todo-selection-advice)
        (advice-add 'org-insert-link :around
                    #'org-popup-posframe--org-insert-link-advice))
    (advice-remove 'org-mks
                   #'org-popup-posframe--org-mks-advice)
    (advice-remove 'org-capture
                   #'org-popup-posframe--org-capture-advice)
    (advice-remove 'org-insert-structure-template
                   #'org-popup-posframe--org-insert-structure-template-mks-advice)
    (advice-remove 'org-fast-todo-selection
                   #'org-popup-posframe--org-fast-todo-selection-advice)
    (advice-remove 'org-insert-link
                   #'org-popup-posframe--org-insert-link-advice)))

(provide 'org-popup-posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; org-popup-posframe.el ends here
