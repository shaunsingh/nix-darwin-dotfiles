;;; selectric-mode.el --- NK/Kalih Creams mode for Emacs  -*- lexical-binding: t; -*-

;; Author: Shaurya Singh <shaunsingh0207@gmail.com>
;; Maintainer: Shaurya Singh <shaunsingh0207@gmail.com>
;; URL: https://github.com/shaunsingh/nix-darwin-dotfiles/tree/main/configs/doom/lisp/selectric-mode
;; Keywords: multimedia, convenience, keyboard, nk-creams
;; Version: 1.4.1

;; Copyright (C) 2021-2021 Shaurya Singh

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

;;; Commentary:

;; This minor mode plays the sound of an keyboard with NK Cream mechanical switches as
;; you type.

;;; Code:

(defconst selectric-files-path (file-name-directory load-file-name)
  "Directory containing the typewriter audio files.")

(defvar-local selectric-last-state nil
  "The last (buffer-size . point) seen by `selectric-mode'.")

(defun selectric-play (sound-file)
  "Play sound from file SOUND-FILE using platform-appropriate program."
  (let ((absolute-path (expand-file-name sound-file selectric-files-path)))
    (if (eq system-type 'darwin)
        (start-process "*Messages*" nil "afplay" absolute-path)
      (start-process "*Messages*" nil "aplay" absolute-path))))

(defun selectric-type ()
  "Make the sound of the printing element hitting the paper."
  (selectric-play "selectric-type.wav")
  (when (= (current-column) (current-fill-column))
    (selectric-play "ping.wav")))

(defun selectric-move ()
  "Make the carriage movement sound."
  (selectric-play "selectric-move.wav"))

(defun selectric-post-command ()
  "Added to `post-command-hook' to decide what sound to play."
  (unless (minibufferp)
    (let ((last-size (car selectric-last-state))
          (last-point (cdr selectric-last-state)))
      (setf selectric-last-state (cons (buffer-size) (point)))
      (cond ((not (eql (buffer-size) last-size)) (selectric-type))
            ((not (eql (point) last-point)) (selectric-move))))))

;;;###autoload
(define-minor-mode selectric-mode
  "Toggle Selectric mode.
When Selectric mode is enabled, your Emacs will sound like a keyboard with NK Cream Mechanical Switches."
  :global t
  :group 'selectric
  (if selectric-mode
      (add-hook 'post-command-hook 'selectric-post-command)
    (remove-hook 'post-command-hook 'selectric-post-command)))

(provide 'selectric-mode)

;;; selectric-mode.el ends here
