;;; micromap --- Graphical buffer percentage indicator -*- lexical-binding: t -*-

;; Author: Lyn Levenick
;; Keywords: mode-line
;; Package-Requires: ((emacs "26.3") (m "1.0.0"))
;; URL: https://github.com/lynlevenick/emacs-micromap

;;; Commentary:

;; Provides a minor-mode, ‘micromap-mode’, which replaces
;; the standard textual percentage indicator in the mode
;; line with a graphical indicator.

;;; Code:

(require 'cl-lib)
(require 'color)

(eval-when-compile
  (require 'm)
  (require 'rx))

(defgroup micromap nil
  "A minor-mode percent position indicator for the mode line."
  :group 'mode-line)

(defun micromap--update-color (symbol newval &rest _)
  "Update parsed colors when raw colors are changed.

SYMBOL and NEWVAL are as in ‘add-variable-watcher’."

  (put symbol :parsed
       (if (string-match (rx ?#
                             (group (repeat 2 hex))
                             (group (repeat 2 hex))
                             (group (repeat 2 hex)))
                         newval)
           (list (/ (string-to-number (match-string 1 newval) 16) 255.0)
                 (/ (string-to-number (match-string 2 newval) 16) 255.0)
                 (/ (string-to-number (match-string 3 newval) 16) 255.0))
         (color-name-to-rgb newval))))

(add-variable-watcher 'micromap-foreground #'micromap--update-color)
(defcustom micromap-foreground "#FFFFFF"
  "Color for ‘micromap-mode’s visible region."
  :group 'micromap
  :type '(choice (string :tag "Hex color") color))

(add-variable-watcher 'micromap-background #'micromap--update-color)
(defcustom micromap-background "#000000"
  "Color for ‘micromap-mode’s background."
  :group 'micromap
  :type '(choice (string :tag "Hex color") color))

(defconst micromap--percent-position
  `(:eval (if (display-graphic-p)
              (micromap--xpm (* 2 (frame-char-width)) (frame-char-height)
                             (micromap--line-number-at-point (window-start))
                             (micromap--line-number-at-point (window-end))
                             (micromap--last-line-number)
                             (get 'micromap-foreground :parsed)
                             (get 'micromap-background :parsed))
            ',mode-line-percent-position))
  "Graphical display of percentage offset when ‘display-graphic-p’.
Falls back to ‘mode-line-percent-position’.")

;;;###autoload
(define-minor-mode micromap-mode
  "Display a graphical percentage indicator in the mode line.
This replaces the textual percentage indicator that is usually
present in the mode line when graphics are enabled."
  :group 'micromap
  :global t

  (if micromap-mode
      (setf mode-line-percent-position micromap--percent-position)
    (setf mode-line-percent-position
          (default-value 'mode-line-percent-position))))

(defun micromap--color-blend (c1 c2 alpha)
  "Blend the two colors C1 and C2 with ALPHA.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (declare (pure t) (side-effect-free t))

  (pcase-let ((`(,c1r ,c1g ,c1b) c1)
              (`(,c2r ,c2g ,c2b) c2)
              (inv-alpha (- 1 alpha)))
    (color-rgb-to-hex
     (+ (* c1r alpha) (* c2r inv-alpha))
     (+ (* c1g alpha) (* c2g inv-alpha))
     (+ (* c1b alpha) (* c2b inv-alpha))
     2)))

(defun micromap--xpm-header (width height start-frac end-frac)
  "Return XPM header for ‘micromap--xpm’.

WIDTH and HEIGHT declare the size of the XPM.
START-FRAC and END-FRAC determine the blending between
‘micromap-foreground’ and ‘micromap-background’."
  (declare (pure t) (side-effect-free t))

  (let ((parsed-foreground (get 'micromap-foreground :parsed))
        (parsed-background (get 'micromap-background :parsed)))
    (format "/* XPM */static char*_[]={\"%i %i 4 1\",\"0 c %s\",\"1 c %s\",\"2 c %s\",\"3 c %s\","
            width height
            micromap-background
            (micromap--color-blend parsed-foreground parsed-background
                                   start-frac)
            micromap-foreground
            (micromap--color-blend parsed-foreground parsed-background
                                   end-frac))))

(defun micromap--xpm-row (character width)
  "Return a row of XPM data of WIDTH made up of CHARACTER."
  (declare (pure t) (side-effect-free t))

  (concat "\"" (make-string width character) "\""))

(m-defun micromap--xpm-row-off (width)
  "Return a row of XPM data of WIDTH made up of the character 0."

  (micromap--xpm-row ?0 width))

(m-defun micromap--xpm-row-start-frac (width)
  "Return a row of XPM data of WIDTH made up of the character 1."

  (micromap--xpm-row ?1 width))

(m-defun micromap--xpm-row-on (width)
  "Return a row of XPM data of WIDTH made up of the character 2."

  (micromap--xpm-row ?2 width))

(m-defun micromap--xpm-row-end-frac (width)
  "Return a row of XPM data of WIDTH made up of the character 3."

  (micromap--xpm-row ?3 width))

(m-defun micromap--xpm
    (width height hl-start hl-end hl-max on-color off-color)
  "Generate WIDTH by HEIGHT xpm image.

Highlight from HL-START to HL-END within [1 HL-MAX] with ON-COLOR
against OFF-COLOR."
  :buffer-local t

  (unless (floatp hl-max)
    (cl-callf float hl-max))
  (let* ((height- (1- height))
         (start (* height- (/ (1- hl-start) hl-max)))
         (end (* height- (/ hl-end hl-max)))
         (start- (1- start))
         (end+ (1+ end))
         (line-on (micromap--xpm-row-on width))
         (line-off (micromap--xpm-row-off width)))
    (propertize "??%" 'display
                (create-image
                 (apply #'concat
                        (micromap--xpm-header width height
                                              (- (fceiling start) start)
                                              (- end (ffloor end)))
                        (cl-loop for i below height
                                 collect (cond
                                          ((<= start i end) line-on)
                                          ((< start- i end)
                                           (micromap--xpm-row-start-frac width))
                                          ((< start i end+)
                                           (micromap--xpm-row-end-frac width))
                                          (line-off))
                                 collect (if (< i height-)
                                             ","
                                           "};")))
                 'xpm t :ascent 'center))))

(m-defun micromap--last-line-number ()
  "Return line number at ‘point-max’.

Unreliable if there have been modifications to the buffer
since the last ‘redisplay’.

Does not work for buffers which are not displayed.

Does not work if either ‘line-number-display-limit’ or
‘line-number-display-limit-width’ are exceeded at ‘point-max’."
  :buffer-local t
  :clear-on edit

  (save-excursion
    (goto-char (point-max))
    (string-to-number (format-mode-line "%l"))))

(defun micromap--line-number-at-point (point)
  "Return line number at POINT.

Unreliable if there have been modifications to the buffer
since the last ‘redisplay’.

Does not work for buffers which are not displayed.

Does not work if either ‘line-number-display-limit’ or
‘line-number-display-limit-width’ are exceeded at POINT."
  (declare (side-effect-free t))

  (save-excursion
    (goto-char point)
    (string-to-number (format-mode-line "%l"))))

(provide 'micromap)
;;; micromap.el ends here
