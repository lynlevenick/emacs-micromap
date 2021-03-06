;;; micromap.el --- Graphical buffer percentage indicator -*- lexical-binding: t -*-

;; Author: Lyn Levenick
;; Package-Requires: ((emacs "26.3") (memo "2.0.0"))
;; Package-Version: 1.0.1
;; URL: https://github.com/lynlevenick/emacs-micromap

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute and/or
;; modify it under the terms of the Apache License Version 2.

;;; Commentary:

;; Provides a minor-mode, ‘micromap-mode’, which replaces
;; the standard textual percentage indicator in the mode
;; line with a graphical indicator.

;;; Code:

(require 'color)

(eval-when-compile
  (require 'cl-lib)
  (require 'memo))

(defgroup micromap nil
  "A minor-mode percent position indicator for the mode line."
  :group 'mode-line)

(defun micromap--update-color (symbol newval &rest _)
  "Update parsed colors when raw colors are changed.

SYMBOL and NEWVAL are as in ‘add-variable-watcher’."

  (put symbol :micromap-parsed (apply #'vector (color-name-to-rgb newval))))

(add-variable-watcher 'micromap-foreground #'micromap--update-color)
(defcustom micromap-foreground "#FFFFFF"
  "Foreground color of micromap."
  :group 'micromap
  :type '(choice (string :tag "Hex color") color))

(add-variable-watcher 'micromap-background #'micromap--update-color)
(defcustom micromap-background "#000000"
  "Background color of micromap."
  :group 'micromap
  :type '(choice (string :tag "Hex color") color))

(defconst micromap--keymap
  '(keymap (mode-line keymap
                      (down-mouse-1 . micromap--scroll)))
  "Keymap for ‘micromap--xpm’.")

(defconst micromap--former-percent-position nil
  "Preserved ‘mode-line-position’ value for ‘mode-line-percent-position’.")

(defconst micromap--percent-position
  `(:eval (if (display-graphic-p)
              (micromap--xpm (* 2 (frame-char-width)) (frame-char-height)
                             (micromap--line-number-at-point (window-start))
                             (micromap--line-number-at-point (window-end))
                             (micromap--line-number-at-point (point-max)))
            ',mode-line-percent-position))
  "Graphical display of percentage offset when ‘display-graphic-p’.
Falls back to ‘mode-line-percent-position’.

‘mode-line-percent-position’ is evaluated when micromap is loaded.
If modified elsewhere, results may be inconsistent.")

;;;###autoload
(define-minor-mode micromap-mode
  "Display a graphical percentage indicator in the mode line.

This replaces the textual percentage indicator that is usually
present in the mode line when graphics are enabled."
  :group 'micromap
  :global t

  (if micromap-mode
      (let ((position
             (cl-position-if #'(lambda (elt) (and (eq (car-safe (cdr-safe elt))
                                                      'mode-line-percent-position)
                                                  elt))
                             mode-line-position)))
        (setf micromap--former-percent-position
              (elt mode-line-position position)
              (elt mode-line-position position)
              micromap--percent-position))
    (let ((position
           (cl-position micromap--percent-position mode-line-position)))
      (setf (elt mode-line-position position)
            micromap--former-percent-position))))

(defun micromap--scroll-to (frac window)
  "Scroll to approximate buffer position FRAC in WINDOW."

  (with-selected-window window
    (let* ((line (micromap--line-number-at-point (point)))
           (last-line (micromap--line-number-at-point (point-max)))
           (target (round (* last-line frac))))
      (forward-line (- target line)))))

(defun micromap--scroll (event)
  "Handle click and drag events on micromap."
  (interactive "e")

  (let* ((posn (event-start event))
         (win (posn-window posn))
         (height (cdr (posn-object-width-height posn)))
         (y (cdr (posn-object-x-y posn)))
         (y-origin (- (cdr (mouse-absolute-pixel-position)) y))
         (mouse-fine-grained-tracking t))
    (micromap--scroll-to (/ (float y) height) win)
    (track-mouse
      (setf track-mouse 'dragging)
      (while (and (setf event (read-event))
                  (mouse-movement-p event))
        (setf posn (event-end event)
              y (min height (max 0 (- (cdr (mouse-absolute-pixel-position)) y-origin))))
        (micromap--scroll-to (/ (float y) height) win)))))

(memo-defun micromap--xpm (width height hl-start hl-end hl-max)
  "Generate WIDTH by HEIGHT xpm image.

Highlights from HL-START to HL-END within [1 HL-MAX]."
  (memo :buffer-local t)

  (unless (floatp hl-max)
    (cl-callf float hl-max))
  (let* ((height- (1- height))
         (start (* height- (/ (1- hl-start) hl-max)))
         (end (* height- (/ hl-end hl-max))))
    (propertize "??%" 'help-echo "mouse-1: Scroll"
                'keymap micromap--keymap
                'display
                (create-image
                 (apply #'concat
                        (micromap--xpm-header width height
                                              (- (fceiling start) start)
                                              (- end (ffloor end)))
                        (micromap--xpm-body width height start end))
                 'xpm t :ascent 'center :scale 1))))

(defun micromap--xpm-header (width height start-frac end-frac)
  "Return XPM header for ‘micromap--xpm’.

WIDTH and HEIGHT declare the size of the XPM.
START-FRAC and END-FRAC determine the blending between
‘micromap-foreground’ and ‘micromap-background’."
  (declare (pure t) (side-effect-free t))

  (let ((parsed-foreground (get 'micromap-foreground :micromap-parsed))
        (parsed-background (get 'micromap-background :micromap-parsed)))
    (format "/* XPM */static char*_[]={\"%i %i 4 1\",\"0 c %s\",\"1 c %s\",\"2 c %s\",\"3 c %s\","
            width height
            micromap-background
            (micromap--color-blend parsed-foreground parsed-background
                                   start-frac)
            micromap-foreground
            (micromap--color-blend parsed-foreground parsed-background
                                   end-frac))))

(defun micromap--xpm-body (width height start end)
  "Return XPM body for ‘micromap--xpm’.

WIDTH and HEIGHT declare the size of the XPM.
START and END determine where foreground lines appear."

  (let ((line-on (micromap--xpm-row-on width))
        (line-off (micromap--xpm-row-off width))
        (start- (1- start))
        (end+ (1+ end))
        (height- (1- height)))
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
                       "};"))))

(defun micromap--color-blend (c1 c2 alpha)
  "Blend the two colors C1 and C2 with ALPHA.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (declare (pure t) (side-effect-free t))

  (let ((inv-alpha (1- alpha)))
    (color-rgb-to-hex
     (- (* (aref c1 0) alpha) (* (aref c2 0) inv-alpha))
     (- (* (aref c1 1) alpha) (* (aref c2 1) inv-alpha))
     (- (* (aref c1 2) alpha) (* (aref c2 2) inv-alpha))
     2)))

(memo-defun micromap--line-number-at-point (point)
  "Return line number at POINT.

Unreliable if there have been modifications to the buffer
since the last ‘redisplay’.

Does not work for buffers which are not displayed.

Does not work if either ‘line-number-display-limit’ or
‘line-number-display-limit-width’ are exceeded at POINT."
  (memo :buffer-local t
        :invalidate-on edit
        :storage hash)

  (let ((line-number-display-limit nil)
        (line-number-display-limit-width 2000000))
    (save-excursion
      (goto-char point)
      (string-to-number (format-mode-line "%l")))))

(defun micromap--xpm-row (character width)
  "Return a row of XPM data of WIDTH made up of CHARACTER."
  (declare (pure t) (side-effect-free t))

  (concat "\"" (make-string width character) "\""))

(memo-defun micromap--xpm-row-off (width)
  "Return a row of XPM data of WIDTH made up of the character 0."

  (micromap--xpm-row ?0 width))

(memo-defun micromap--xpm-row-start-frac (width)
  "Return a row of XPM data of WIDTH made up of the character 1."

  (micromap--xpm-row ?1 width))

(memo-defun micromap--xpm-row-on (width)
  "Return a row of XPM data of WIDTH made up of the character 2."

  (micromap--xpm-row ?2 width))

(memo-defun micromap--xpm-row-end-frac (width)
  "Return a row of XPM data of WIDTH made up of the character 3."

  (micromap--xpm-row ?3 width))

(provide 'micromap)
;;; micromap.el ends here
