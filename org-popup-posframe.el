;;; org-popup-posframe.el --- Display org mode popup buffers in posframe -*- lexical-binding: t; -*-

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

;;;; Faces

;;;; Customization

;;;; Constants

;;;; Functions

(defun org-popup-posframe--org-mks-advice (table title &optional prompt specials)
  (let ((inhibit-quit t)
	(buffer (get-buffer-create "*Org Select*"))
	(prompt (or prompt "Select: "))
	case-fold-search
	current)
    (unwind-protect
	(catch 'exit
	  (while t
	    (let ((des-keys nil)
		  (allowed-keys '("\C-g"))
		  (tab-alternatives '("\s" "\t" "\r"))
		  (cursor-type nil))
	      (with-current-buffer buffer
		(erase-buffer)
		(insert title "\n\n")
		;; Populate allowed keys and descriptions keys
		;; available with CURRENT selector.
		(let ((re (format "\\`%s\\(.\\)\\'"
				  (if current (regexp-quote current) "")))
		      (prefix (if current (concat current " ") "")))
		  (dolist (entry table)
		    (pcase entry
		      ;; Description.
		      (`(,(and key (pred (string-match re))) ,desc)
		       (let ((k (match-string 1 key)))
			 (push k des-keys)
			 ;; Keys ending in tab, space or RET are equivalent.
			 (if (member k tab-alternatives)
			     (push "\t" allowed-keys)
			   (push k allowed-keys))
			 (insert prefix "[" k "]" "..." "  " desc "..." "\n")))
		      ;; Usable entry.
		      (`(,(and key (pred (string-match re))) ,desc . ,_)
		       (let ((k (match-string 1 key)))
			 (insert prefix "[" k "]" "     " desc "\n")
			 (push k allowed-keys)))
		      (_ nil))))
		;; Insert special entries, if any.
		(when specials
		  (insert "----------------------------------------------------\n")
		  (pcase-dolist (`(,key ,description) specials)
		    (insert (format "[%s]     %s\n" key description))
		    (push key allowed-keys))))
	      ;; Display UI and let user select an entry or
	      ;; a sub-level prefix.
	      (posframe-show buffer)
	      (let ((pressed (org--mks-read-key
			      allowed-keys prompt
			      (not (pos-visible-in-window-p (1- (point-max)))))))
		(setq current (concat current pressed))
		(cond
		 ((equal pressed "\C-g") (user-error "Abort"))
		 ;; Selection is a prefix: open a new menu.
		 ((member pressed des-keys))
		 ;; Selection matches an association: return it.
		 ((let ((entry (assoc current table)))
		    (and entry (throw 'exit entry))))
		 ;; Selection matches a special entry: return the
		 ;; selection prefix.
		 ((assoc current specials) (throw 'exit current))
		 (t (error "No entry available")))))))
      (when buffer (kill-buffer buffer)))))



(define-minor-mode org-popup-posframe-mode
  "Show org mode popup buffers in posframe"
  :group 'org-popup-posframe
  :global t
  :lighter nil
  (if org-popup-posframe-mode
      (progn
        (advice-add 'org-mks :override
                    #'org-popup-posframe--org-mks-advice))
    (advice-remove 'org-mks
                   #'org-popup-posframe--org-mks-advice)))
