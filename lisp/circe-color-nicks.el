;;; circe-color-nicks.el --- Color nicks in the channel

;; Copyright (C) 2012  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>

;; This file is part of Circe.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301  USA

;;; Commentary:

;; This Circe module adds the ability to assign a color to each
;; nick in a channel.

;; Some ideas/code copied from rcirc-colors.el.

;; To use it, put the following into your .emacs:

;; (require 'circe-color-nicks)
;; (enable-circe-color-nicks)

;;; Code:

(require 'circe)
(require 'color)

;;;###autoload
(defun enable-circe-color-nicks ()
  "Enable the Color Nicks module for Circe.
This module colors all encountered nicks in a cross-server fashion."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'circe-channel-mode)
        (add-circe-color-nicks))))
  (add-hook 'circe-channel-mode-hook
            'add-circe-color-nicks))

(defun disable-circe-color-nicks ()
  "Disable the Color Nicks module for Circe.
See `enable-circe-color-nicks'."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'circe-channel-mode)
        (remove-circe-color-nicks))))
  (remove-hook 'circe-channel-mode-hook
               'add-circe-color-nicks))

(defun add-circe-color-nicks ()
  "Add `circe-color-nicks' to `lui-pre-output-hook'."
  (add-hook 'lui-pre-output-hook 'circe-color-nicks))

(defun remove-circe-color-nicks ()
  "Remove `circe-color-nicks' from `lui-pre-output-hook'."
  (remove-hook 'lui-pre-output-hook 'circe-color-nicks))


(defgroup circe-color-nicks nil
  "Nicks colorization for Circe"
  :prefix "circe-color-nicks-"
  :group 'circe)

(defcustom circe-color-nicks-min-contrast-ratio 7
  "Minimum contrast ratio from background for generated colors;
recommended is 7:1, or at least 4.5:1 (7 stands for 7:1 here).
Lower value allows higher color spread, but could lead to less
readability."
  :group 'circe-color-nicks)

(defcustom circe-color-nicks-min-difference 20
  "Minimum difference from each other for generated colors."
  :group 'circe-color-nicks)

(defcustom circe-color-nicks-min-fg-difference 20
  "Minimum difference from foreground for generated colors."
  :group 'circe-color-nicks)

(defcustom circe-color-nicks-everywhere nil
  "Whether nicks should be colored in message bodies too."
  :type 'boolean
  :group 'circe)

(defcustom circe-color-nicks-message-blacklist nil
  "Blacklist for nicks that shall never be highlighted inside
  images."
  :type '(repeat string)
  :group 'circe)


;;; See http://www.w3.org/TR/2013/NOTE-WCAG20-TECHS-20130905/G18

(defsubst circe-w3-contrast-c-to-l (c)
  (if (<= c 0.03928)
      (/ c 12.92)
    (expt (/ (+ c 0.055) 1.055) 2.4)))

(defsubst circe-w3-contrast-relative-luminance (rgb)
  (apply #'+
         (cl-mapcar (lambda (color coefficient)
                      (* coefficient
                         (circe-w3-contrast-c-to-l color)))
                    rgb
                    '(0.2126 0.7152 0.0722))))

(defsubst circe-w3-contrast-contrast-ratio (color1 color2)
  (let ((l1 (+ 0.05 (circe-w3-contrast-relative-luminance color1)))
        (l2 (+ 0.05 (circe-w3-contrast-relative-luminance color2))))
    (if (> l1 l2)
        (/ l1 l2)
        (/ l2 l1))))


(defun circe-color-name-to-rgb (color)
  "Like `color-name-to-rgb' but also handles \"unspecified-bg\"
and \"unspecified-fg\"."
  (let ((rgb (color-name-to-rgb color)))
    (cond
     (rgb rgb)
     ((equal color "unspecified-bg") '(0 0 0))
     ((equal color "unspecified-fg") '(1 1 1)))))

(defun circe-nick-color-appropriate-p (color)
  "Tells whether a color is appropriate for being a nick color."
  (let ((color (circe-color-name-to-rgb color))
        (bg (circe-color-name-to-rgb (face-background 'default)))
        (fg (circe-color-name-to-rgb (face-foreground 'default))))
    (and (>= (circe-w3-contrast-contrast-ratio color bg)
             circe-color-nicks-min-contrast-ratio)
         (>= (color-cie-de2000 (apply #'color-srgb-to-lab fg)
                               (apply #'color-srgb-to-lab color))
             circe-color-nicks-min-fg-difference))))

(defun circe-nick-colors-too-similar-p (color1 color2)
  "Tells whether COLOR1 and COLOR2 are too similar per
`circe-color-nicks-min-difference'."
  (let ((color1 (circe-color-name-to-rgb color1))
        (color2 (circe-color-name-to-rgb color2)))
    (< (color-cie-de2000 (apply #'color-srgb-to-lab color1)
                         (apply #'color-srgb-to-lab color2))
       circe-color-nicks-min-difference)))

(defun circe-nick-colors-delete-similar (colors)
  "Mutate and return list COLORS to delete colors too similar to each other.
See `circe-color-nicks-min-difference'."
  (let ((list colors))
    (while list
      (let ((color (car list))
            (list2 list))
        (while (cdr list2)
          (if (circe-nick-colors-too-similar-p color (cadr list2))
              (setcdr list2 (cddr list2))
            (setq list2 (cdr list2)))))
      (setq list (cdr list)))
    colors))

(defun circe-nick-color-generate-pool ()
  "Return a list of appropriate nick colors."
  (circe-nick-colors-delete-similar
   (cl-remove-if-not
    #'circe-nick-color-appropriate-p
    (mapcar #'car color-name-rgb-alist))))

(defun circe-nick-color-pool-test ()
  "Display all appropriate nick colors in a temp buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Circe color test*"))
  (erase-buffer)
  (let ((pool (circe-nick-color-generate-pool)))
    (while pool
      (let ((pt (point)))
        (insert "The quick brown fox jumped over the lazy dog.\n")
        (put-text-property pt (point) 'face `(:foreground ,(pop pool)))))))

(defvar circe-nick-color-pool (circe-nick-color-generate-pool)
  "List of unused nick colors.  Once this is exhausted, it's
useless; new colors are picked by finding the one whose assigned
nick spoke least recently.")

(defvar circe-nick-color-mapping (make-hash-table :test 'equal)
  "Hash-table from nicks to (color . timestamp) tuples where the
timestamp tells when the color was least used, i.e. any of the
nicks the color is assigned to last had an activity.")

(defun circe-nick-color-nick-list ()
  "Return list of all nicks that have a color assigned to them.
Own nick is excluded."
  (let (nicks)
    (maphash (lambda (nick color)
               (unless (circe-server-my-nick-p nick)
                 (push nick nicks)))
             circe-nick-color-mapping)
    nicks))

(defun circe-nick-color-for-nick (nick)
  "Return the color for NICK.  Assigns a color to NICK if one
wasn't assigned already."
  (let ((entry (gethash nick circe-nick-color-mapping)))
    (if (not entry)
        (let ((color (circe-nick-color-pick)))
          (puthash nick (cons color (cadr (current-time)))
                   circe-nick-color-mapping)
          color)
      (setcdr entry (cadr (current-time)))
      (car entry))))

(defun circe-nick-color-pick ()
  "Picks either a color from the pool of unused colors, or the
color that was used least recently (i.e. nicks that have it
assigned have been least recently active)."
  (if (not (null circe-nick-color-pool))
      (pop circe-nick-color-pool)
    (circe-nick-color-pick-least-recent)))

(defun circe-nick-color-pick-least-recent ()
  "Pick the color that was used least recently.
See `circe-nick-color-pick', which is where this is used."
  (let ((least-recent-color nil)
        (biggest-time-gap -1))
    (maphash
     (lambda (nick entry)
       (let ((color (car entry))
             (time-gap (- (cadr (current-time)) (cdr entry))))
         (if (> time-gap biggest-time-gap)
             (progn
               (setq biggest-time-gap time-gap)
               (setq least-recent-color color)))))
     circe-nick-color-mapping)
    (if least-recent-color
        least-recent-color
      ;; Someone must have messed with `circe-nick-color-mapping', recover by
      ;; re-filling the pool.
      (setq circe-nick-color-pool (circe-nick-color-generate-pool))
      (pop circe-nick-color-pool))))

(defun circe-color-nicks ()
  "Color nicks on this lui output line."
  (when (eq major-mode 'circe-channel-mode)
    (let ((nickstart (text-property-any (point-min) (point-max)
                                        'lui-format-argument 'nick)))
      (when nickstart
        (goto-char nickstart)
        (let* ((nickend (next-property-change nickstart))
               (nick (buffer-substring-no-properties nickstart nickend)))
          (when (not (circe-server-my-nick-p nick))
            (let ((color (circe-nick-color-for-nick nick)))
              (add-face-text-property nickstart nickend
                                      `(:foreground ,color)))))))
    (when circe-color-nicks-everywhere
      (let ((body (text-property-any (point-min) (point-max)
                                     'lui-format-argument 'body)))
        (when body
          (goto-char body)
          (let* ((nicks (circe-nick-color-nick-list))
                 (regex (regexp-opt nicks 'words)))
            (let (case-fold-search)
              (while (re-search-forward regex nil t)
                (let* ((nick (match-string-no-properties 0))
                       (color (circe-nick-color-for-nick nick)))
                  (add-face-text-property (match-beginning 0) (match-end 0)
                                          `(:foreground ,color)))))))))))

(provide 'circe-color-nicks)
;;; circe-color-nicks.el ends here
