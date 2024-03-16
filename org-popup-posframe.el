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

(defcustom org-popup-posframe-poshandler #'posframe-poshandler-window-bottom-right-corner
  "The poshandler of org-popup-posframe."
  :group 'org-popup-posframe
  :type 'function)

(defcustom org-popup-posframe-org-mks-poshandler org-popup-posframe-poshandler
  "The poshandler of org-popup-posframe."
  :group 'org-popup-posframe
  :type 'function)

(defcustom org-popup-posframe-org-todo-poshandler org-popup-posframe-poshandler
  "The poshandler of org-popup-posframe."
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

(defun org-popup-posframe--org-mks-show-buffer ()
  (when (posframe-workable-p)
    (posframe-show (current-buffer)
		   :position (point)
		   :poshandler org-popup-posframe-org-mks-poshandler
		   :font org-popup-posframe-font
		   :background-color (face-attribute 'org-popup-posframe :background nil t)
		   :foreground-color (face-attribute 'org-popup-posframe :foreground nil t)
		   ;; :min-width org-popup-posframe-min-width
		   ;; :min-height org-popup-posframe-min-height
		   :internal-border-width org-popup-posframe-border-width
		   :internal-border-color (face-attribute 'org-popup-posframe-border :background nil t)
		   :override-parameters org-popup-posframe-parameters)))

(defun org-popup-posframe--org-todo-show-buffer ()
  (when (posframe-workable-p)
    (posframe-show (get-buffer-create " *Org todo*")
		   :position (point)
		   :poshandler org-popup-posframe-org-todo-poshandler
		   :font org-popup-posframe-font
		   :background-color (face-attribute 'org-popup-posframe :background nil t)
		   :foreground-color (face-attribute 'org-popup-posframe :foreground nil t)
		   ;; :min-width org-popup-posframe-min-width
		   ;; :min-height org-popup-posframe-min-height
		   :internal-border-width org-popup-posframe-border-width
		   :internal-border-color (face-attribute 'org-popup-posframe-border :background nil t)
		   :override-parameters org-popup-posframe-parameters)))


(defun org-popup-posframe--org-mks-advice (func table title &optional prompt specials)
  (let ((buffer (get-buffer-create "*Org Select*")))
    (with-current-buffer buffer
      (cl-letf (((symbol-function 'org-switch-to-buffer-other-window) #'get-buffer)
                ((symbol-function 'org-fit-window-to-buffer)
                 #'org-popup-posframe--org-mks-show-buffer))
        (funcall func table title prompt specials)))))

(defun org-popup-posframe--org-todo-advice (func &optional current-todo-keyword)
  ;; If using (with-current-buffer buffer ... ) or (set-buffer buffer)
  ;; before calling org-fast-todo-selection
  ;; inside org-fast-todo-selection, (apply max ...) will error
  (declare (indent 1) (debug t))
  (save-current-buffer
    (fset 'original-set-window-buffer (symbol-function 'set-window-buffer))
    (let ((buffer (get-buffer-create " *Org todo*"))
          (result
           (cl-letf* (((symbol-function 'delete-other-windows) (lambda () nil))
                      ((symbol-function 'split-window-below) (lambda () nil))
                      ((symbol-function 'set-window-buffer) (lambda (a b) nil))
                      ;; set buffer to " *Org todo*"
                      ((symbol-function 'org-switch-to-buffer-other-window) #'set-buffer)
                      ;; avoid set-window-buffer redefinition inside posframe-show
                      ((symbol-function 'org-fit-window-to-buffer)
                       (lambda () (cl-letf (((symbol-function 'set-window-buffer)
                                             #'original-set-window-buffer))
                                    (org-popup-posframe--org-todo-show-buffer)))))
                    (funcall func current-todo-keyword))))
      (kill-buffer buffer)
      result)))





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
        (advice-add 'org-fast-todo-selection :around
                    #'org-popup-posframe--org-todo-advice))
    (advice-remove 'org-mks
                   #'org-popup-posframe--org-mks-advice)
    (advice-remove 'org-fast-todo-selection
                   #'org-popup-posframe--org-todo-advice)))

(provide 'org-popup-posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; org-popup-posframe.el ends here
