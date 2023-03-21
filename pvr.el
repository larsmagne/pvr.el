;;; pvr.el --- Personal Video Recorder
;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <lmi@gnus.org>
;; Keywords: extensions, processes

;; This file is not part of GNU Emacs.

;; pvr.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; pvr.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'xml)

(defvar pvr-home-directory "~/xmltv/"
  "Where PVR will write and read files from.")

(defvar pvr-program-file (expand-file-name "no.xml" pvr-home-directory)
  "The location of the xmltv program listing file.")

(defvar pvr-configuration-file (expand-file-name "pvr.conf" pvr-home-directory)
  "The location of your configuration file.")

(defvar pvr-channels-file (expand-file-name "channels.conf" pvr-home-directory)
  "The location of your configuration file.")

(defvar pvr-binary-directory "~/xmltv/bin/"
  "Where PVR will find executables.")

(defvar pvr-directory "/tv/"
  "Where PVR will store the recorded programs.")

(defvar pvr-denied-channels '("nrk3"))

(defvar pvr-recording-list nil)
(defvar pvr-program-tree nil)
(defvar pvr-channels nil)
(defvar pvr-channel-tuner-map nil)
(defvar pvr-wanted-programs nil)
(defvar pvr-timers nil)
(defvar pvr-record-kill-timer nil)
(defvar pvr-recordings nil)

(defun pvr-mangle-spec (spec)
  (let ((out nil)
	(type nil))
    (dolist (elem spec)
      (if (string-match ":" elem)
	  (setq type
		(cond
		 ((string= elem "c:")
		  'channel)
		 (t
		  'unknown)))
	(unless type
	  (setq type 'title))
	(push (list type elem) out)
	(setq type nil)))
    out))
      
(defun pvr-parse-configuration-file ()
  "Parse the configuration file."
  (interactive)
  (setq pvr-recording-list nil)
  (with-temp-buffer
    (insert-file-contents pvr-configuration-file)
    (while (not (eobp))
      (let ((line nil)
	    elem)
	(while (not (looking-at " *$"))
	  (setq elem (read (current-buffer)))
	  (when (symbolp elem)
	    (setq elem (symbol-name elem)))
	  (push elem line))
	(push (pvr-mangle-spec (nreverse line)) pvr-recording-list))
      (forward-line 1))))

(defun pvr-parse-program ()
  "Parse the program file."
  (interactive)
  (setq pvr-program-tree (xml-parse-file pvr-program-file)))

(defun pvr-get-channels ()
  (setq pvr-channels nil)
  (dolist (channel (cddr (car pvr-program-tree)))
    (when (and (consp channel)
	       (eq (car channel) 'programme))
      (let ((name (cdr (assq 'channel (cadr channel)))))
	(unless (assoc name pvr-channels)
	  (push (list name (car (split-string name "\\.")))
		pvr-channels))))))

(defun pvr-wanted-program-p (program)
  (let ((title (nth 2 (assq 'title program)))
	(channel (cadr (assoc (cdr (assq 'channel (cadr program)))
				    pvr-channels)))
	(wantedp nil))
    (dolist (elem pvr-recording-list)
      (when (and (string-match (cadr (assq 'title elem)) title)
		 (not (member channel pvr-denied-channels))
		 (or (null (cadr (assq 'channel elem)))
		     (string-match (cadr (assq 'channel elem)) channel)))
	(setq wantedp t)))
    wantedp))

(defun pvr-find-matching-programs ()
  (setq pvr-wanted-programs nil)
  (dolist (program (cddr (car pvr-program-tree)))
    (when (and (consp program)
	       (eq (car program) 'programme))
      (when (pvr-wanted-program-p program)
	(let ((channel (cadr (assoc (cdr (assq 'channel (cadr program)))
				    pvr-channels)))
	      (title (nth 2 (assq 'title program)))
	      (episode (nth 2 (assq 'episode-num program)))
	      (start (car (split-string
			   (cdr (assq 'start (cadr program))) " ")))
	      (stop (car (split-string
			  (cdr (assq 'stop (cadr program))))))
	      (description (nth 2 (assq 'desc program))))
	  (when episode
	    (setq episode (replace-regexp-in-string "[^-0-9/:]" "" episode))
	    (setq title (format "%s (%s)" title episode)))
	  (push (list start stop channel title description nil nil nil nil)
		pvr-wanted-programs)))))
  (setq pvr-wanted-programs (sort pvr-wanted-programs
				  (lambda (p1 p2)
				    (string< (car p1) (car p2)))))
  (while (and pvr-wanted-programs
	      (string< (cadar pvr-wanted-programs)
		       (format-time-string "%Y%m%d%H%M%S" (current-time))))
    (pop pvr-wanted-programs)))

(defun pvr-write-recording-list ()
  (with-temp-file (expand-file-name "recording.el" pvr-home-directory)
    (prin1 pvr-wanted-programs (current-buffer))))

(defun pvr-read-recording-list ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "recording.el" pvr-home-directory))
    (read (current-buffer))))
				  
(defun pvr-parse-time (xml-time)
  (let* ((time (car (split-string xml-time " ")))
	 (itime (apply
		 'encode-time
		 (mapcar
		  'string-to-number
		  (list (substring time 12 14)
			(substring time 10 12)
			(substring time 8 10)
			(substring time 6 8)
			(substring time 4 6)
			(substring time 0 4))))))
    itime))

(defun pvr-format-time (xml-time)
  (let ((time (pvr-parse-time xml-time)))
    (format-time-string "%Y%m%dT%H%M%S" time)))

(defun pvr-format-date (xml-time)
  (let ((time (pvr-parse-time xml-time)))
    (format-time-string "%a %d/%m-%Y" time)))

(defun pvr-format-clock (xml-time)
  (let ((time (pvr-parse-time xml-time)))
    (format-time-string "%H:%M" time)))

(defun pvr-insert-program (program)
  (let ((point (point)))
    (insert (format "%-10s %s %s-%s%s\n"
		    (nth 2 program)
		    (pvr-format-date (car program))
		    (pvr-format-clock (car program))
		    (pvr-format-clock (cadr program))
		    (if (nth 5 program)
			" (skipped)"
		      "")))
    (insert (format "    %s\n" (nth 3 program)))
    (save-restriction
      (narrow-to-region
       (point)
       (progn
	 (insert (format "    %s\n" (or (nth 4 program) "")))
	 (point)))
      (goto-char (point-min))
      (fill-paragraph nil)
      (goto-char (point-max)))
    (put-text-property point (point) 'pvr-program program)))

(defun pvr-remove-program (program)
  (let ((start (text-property-any (point-min) (point-max) 'pvr-program
				  program)))
    (when start
      (goto-char start)
      (while (and (not (eobp))
		  (eq (get-text-property (point) 'pvr-program)
		      program))
	(forward-char 1))
      (delete-region start (point)))))

(defun pvr-list-wanted-programs ()
  "Display programs that will be recorded."
  (interactive)
  (pvr-parse-configuration-file)
  (pvr-find-matching-programs)
  (pvr-optimize-schedule)
  (pop-to-buffer "*Recordings*")
  (erase-buffer)
  (pvr-mode)
  (dolist (program pvr-wanted-programs)
    (pvr-insert-program program)
    (insert "\n"))
  (goto-char (point-min)))

(defun pvr-kill-process (process)
  (ignore-errors
    (delete-process process)))

(defun pvr-optimize-schedule ()
  (let ((programs pvr-wanted-programs)
	program)
    (while (setq program (pop programs))
      (when (and nil
		 programs
		 (string< (caar programs) (cadr program))
		 (null (nth 5 program))
		 (null (nth 5 (car programs))))
	(message "Collision between %s and %s"
		 (nth 3 program) (nth 3 (car programs)))
	;; We have over-lapping programs.
	(cond
	 ((string-match "(r)" (nth 3 program))
	  (setcar (nthcdr 5 program) 'skip))
	 (t
	  (setcar (nthcdr 5 (car programs)) 'skip)))))

    (setq programs pvr-wanted-programs)
    
    (while (setq program (pop programs))
      (if (not (cl-equalp (cadr program) (caar programs)))
	  (setcar (nthcdr 6 program) (* 5 60))
	;;(message "Matching stop and start: %s %s" (cadr program)
	;; (caar programs))
	(setcar (nthcdr 6 program) 0)))))

(defun pvr-record (file-name device stop &optional grace)
  (let ((stop-time
	 (pvr-parse-time stop))
	(process
	 (start-process
	  "recording" (get-buffer-create "*Cat Recording*")
	  "bash" "-c"
	  (format "cat %s > %s" device (shell-quote-argument file-name)))))
    (setq pvr-record-kill-timer
	  (run-at-time (time-subtract stop-time
				      (list 0 (* -1 (or grace 0))))
		       nil 'pvr-kill-process process))
    process))

(defun pvr-file-name (channel title)
  (expand-file-name
   (format "%s-%s-%s.mpg"
	   (format-time-string "%Y-%m-%d %H-%M" (current-time))
	   channel
	   (replace-regexp-in-string "/" "-" 
				     title))
   pvr-directory))

(defun pvr-canonical-channel (channel)
  (downcase (replace-regexp-in-string "[^0-9a-z]" "" channel)))

(defun pvr-find-frequency (channel)
  (setq channel (pvr-canonical-channel channel))
  (let ((frequency nil))
    (dolist (elem pvr-channel-tuner-map)
      (let ((cname (car elem))
	    (freq (cadr elem)))
	(when (equal channel (pvr-canonical-channel cname))
	  (setq frequency freq))))
    frequency))

(defun pvr-execute (program &rest arguments)
  (apply 'call-process (expand-file-name program pvr-binary-directory)
	 nil (get-buffer-create "*exe*") nil arguments))

(defun pvr-set-channel (channel device)
  (let* ((freq (pvr-find-frequency channel))
	 (arg1 "-c")
	 (arg2 freq))
    (when (string-match "^X" freq)
      (setq arg1 "-f"
	    arg2 (substring freq 1)))
    (pvr-execute "/usr/bin/ivtv-tune" "-d" device "-t" "europe-west"
		 arg1 arg2)))

(defun pvr-choose-device ()
  (let ((devices (list "/dev/video0")))
    ;; Prune off old processes.
    (while (and pvr-recordings
		(not (memq (process-status (cadar pvr-recordings))
			   '(open run))))
      (pop pvr-recordings))
    (dolist (rec pvr-recordings)
      (when (memq (process-status (cadr rec))
		  '(open run))
	(setq devices (delete (car rec) devices))))
    (car devices)))

(defun pvr-start-recording (title channel stop grace)
  (let ((device (pvr-choose-device)))
    (if (not device)
	(message "No free devices")
      (pvr-set-channel channel device)
      (push (list device (pvr-record (pvr-file-name channel title)
				     device stop grace))
	    pvr-recordings)
      (message "Starting to record %s:%s" channel title))))

(defun pvr-unschedule ()
  "Unschedule all recordings."
  (interactive)
  (while pvr-timers
    (ignore-errors
      (cancel-timer (pop pvr-timers)))))

(defun pvr-schedule ()
  "Schedule all recordings."
  (interactive)
  (pvr-unschedule)
  (dolist (program pvr-wanted-programs)
    (let* ((channel (nth 2 program))
	   (title (nth 3 program))
	   (start (pvr-parse-time (car program)))
	   (stop (cadr program)))
      (when (and (string< (format-time-string "%Y%m%d%H%M%S" (current-time))
			  stop)
		 (null (nth 5 program)))
	(push
	 (run-at-time
	  start nil #'pvr-start-recording title channel stop
	  (nth 6 program))
	 pvr-timers)))))

(defun pvr-read-channel-file ()
  (with-temp-buffer
    (setq pvr-channel-tuner-map nil)
    (insert-file-contents pvr-channels-file)
    (let ((channel nil)
	  (freq nil))
      (while (re-search-forward
	      "^\\(\\[\\([^]]+\\)]\\)\\|channel += +\\([^ \n\t]+\\)"
	      nil t)
	(if (match-beginning 2)
	    (setq channel (match-string 2))
	  (setq freq (match-string 3))
	  (push (list channel freq) pvr-channel-tuner-map))))
    (reverse pvr-channel-tuner-map)))

(defun pvr-choose-channel (channel)
  "If there's a free video channel, change the channel and return the device."
  (let ((device (pvr-choose-device)))
    (if (not device)
	nil
      (pvr-set-channel channel device)
      device)))

(defun pvr-start-server ()
  (setq server-use-tcp t
	server-host (system-name)
	server-name (car (split-string (system-name) "[.]")))
  (server-start))

(defun pvr ()
  "Start up the PVR."
  (interactive)
  (pvr-start-server)
  (pvr-parse-configuration-file)
  (pvr-read-channel-file)
  (pvr-parse-program)
  (pvr-get-channels)
  (pvr-list-wanted-programs)
  (pvr-schedule))

(defvar pvr-mode-map nil)
(put 'pvr-mode 'mode-class 'special)

(unless pvr-mode-map
  (setq pvr-mode-map (make-keymap))
  (suppress-keymap pvr-mode-map)

  (define-key pvr-mode-map " " 'pvr-toggle-skip)
  (define-key pvr-mode-map "s" 'pvr-edit-start)
  (define-key pvr-mode-map "e" 'pvr-edit-end))

(defun pvr-mode ()
  "Major mode for browsing program listsings."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'pvr-mode)
  (setq mode-name "PVR")
  (use-local-map pvr-mode-map)
  (buffer-disable-undo)
  (run-hooks 'pvr-mode-hook))

(defun pvr-current-program ()
  (get-text-property (point) 'pvr-program))

(defun pvr-toggle-skip ()
  "Toggle whether to skip this program"
  (interactive)
  (save-excursion
    (let ((program (pvr-current-program)))
      (when program
	(setcar (nthcdr 5 program) (not (nth 5 program)))
	(pvr-remove-program program)
	(pvr-insert-program program)
	(pvr-write-recording-list)
	(pvr-schedule)))))

(defun pvr-edit-start ()
  "Edit start time."
  (interactive)
  (save-excursion
    (let ((program (pvr-current-program)))
      (when program
	(setcar program
		(read-from-minibuffer "New start time: "
				      (car program)))
	(pvr-remove-program program)
	(pvr-insert-program program)
	(pvr-write-recording-list)))))

(defun pvr-edit-end ()
  "Edit end time."
  (interactive)
  (save-excursion
    (let ((program (pvr-current-program)))
      (when program
	(setcar (cdr program)
		(read-from-minibuffer "New end time: "
				      (cadr program)))
	(pvr-remove-program program)
	(pvr-insert-program program)
	(pvr-write-recording-list)))))

(defun pvr-first-timer ()
  (interactive)
  (message "%s" (current-time-string
		 (list (aref (car (last pvr-timers 1)) 1)
		       (aref (car (last pvr-timers 1)) 2)))))

(defun pvr-get-new-program ()
  (ignore-errors
    (when (file-exists-p pvr-program-file)
      (delete-file pvr-program-file))
    (with-temp-file pvr-program-file
      (call-process "tv_grab_no" nil t nil))))

(defun pvr-rescan-program ()
  (interactive)
  (message "Rescanning program...")
  (ignore-errors
    ;;(pvr-get-new-program)
    (pvr)
    (pvr-schedule)))

(defun pvr-start-rescanning ()
  (interactive)
  (run-at-time "05:00am" (* 24 60 60 1) 'pvr-rescan-program))

(defun start-pvr ()
  (interactive)
  (pvr)
  (pvr-start-rescanning)
  (pvr-schedule))

(defun pvr-view ()
  "View live tv."
  (interactive)
  ;;(pvr-execute "ant" "")
  (let ((process (start-process
		  "view" nil (expand-file-name "view" pvr-binary-directory)))
	(channel 1)
	command)
    (while (not (eq command ?q))
      (setq command (read-char))
      (cond
       ((eq command ?n)
	(cl-incf channel))
       ((eq command ?p)
	(cl-decf channel)))
      (pvr-set-channel
       (car
	(elt pvr-channel-tuner-map
	     (% channel (length pvr-channel-tuner-map))))
       "/dev/video0"))
    (delete-process process)))

(defun pvr-live-tv (channel)
  (interactive "sChannel: ")
  (pvr-set-channel channel "/dev/video0")
  (call-process "dd" nil nil nil "if=/dev/video0" "of=/tv/live"))

(defun pvr-watch-live-channels ()
  (interactive)
  (pvr-parse-configuration-file)
  (pvr-read-channel-file)
  (pvr-parse-program)
  (pvr-get-channels)
  (let ((channels '("TV 2"
		    "CNN" "NRK2" "SVT 1" "SVT 2" "NRK1" "TV2 Nyhetskanalen"
		    "BBC World"))
	(endp nil)
	(always-restart nil)
	cc-proc mplayer-proc char)
    (while (not endp)
      (setq char (string-to-number (format "%c" (read-char "Channel: "))))
      (if (zerop char)
	  (progn
	    (when cc-proc
	      (delete-process cc-proc)
	      (delete-process mplayer-proc))
	    (setq endp t))
	(pvr-set-channel (elt channels (1- char)) "/dev/video0")
	(when (or always-restart
		  (not cc-proc))
	  (when (and always-restart cc-proc)
	    (delete-process cc-proc)
	    (delete-process mplayer-proc))
	  (setq cc-proc (start-process "dd" nil "dd" "if=/dev/video0"
				       "of=/tv/live"))
	  (sleep-for (if always-restart 2 2))
	  (setq mplayer-proc (start-process "mplayer" nil "mplayer" "-fs"
					    "/tv/live")))))))

(provide 'pvr)
