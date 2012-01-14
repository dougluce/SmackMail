;
; This file steals heavily from Miles Bader's Batmail
;

(load "net/imap")

(defconst smack-display-buf "*smack-display*")
(defconst smack-composition-buf "*smack-composition*")
(defconst smack-scratch-buf "*smack-scratch*")
(defconst smack-metamail-buf "*smack-metamail*")
(defconst smack-captions-buf "*smack-captions*")
(defconst smack-version "0.1" "SmackMail version number")
(defconst smack-folder-update-buf " *smack-folder-update*")
(defconst smack-cuid-cols 4)

(defvar smack-state nil "String describing current state of smackmail.")
(defvar smack-new-mail-p nil "T if there is new mail.")
(defvar smack-new-mail-count-str "" "For displaying in modleine.")
(defvar smack-captions-help-mode-str "[? for help]")
(defvar smack-display-scroll-help-str "[v to scroll]")
(defvar smack-local-blind nil)

(defvar smack-stingy-display nil "*If non-nil, use less verbosity some places.")
(defvar smack-main-dir (or (getenv "SMACKDIR")
			   "/usr/var/smackmail")
  "*Place where smackmail system files can be found.")

(defvar smack-etc-dir (concat smack-main-dir "/etc")
  "*Directory where miscellaneous files are kept")

(defvar smack-composition-ckp-file (make-temp-file "smackcomp")
  "*File name used to checkpoint the smack-composition buffer or nil,
meaning don't checkpoint.")

(defvar smack-composition-help-mode-str
  "C-x C-s sends, C-x C-c aborts, C-x ? for help]")

(defvar smack-saved-window-configuration nil)

(defvar smack-active nil)
(defvar smack-reading nil)
(defvar smack-sending nil)
(defvar smack-suspended nil)
(defvar smack-started-from-shell nil)
(defvar smack-last-caption-mark (make-marker))
(defvar smack-first-new-msg (make-marker))
(defvar smack-last-cuid 0)
(defvar smack-exit-to-shell t
  "*If non-nil, quitting from a smackmail that was started from the shell
will exit emacs as well.")


(defvar smack-obnoxious-logo t
  "*If non-nil, displays huge obnoxious logo when smackmail is started from
the shell.")

(defvar smack-default-folders "*"
  "*Default set folders to prompt with when starting smackmail.")

(defmacro temp-set-buffer (bufname &rest body)
  "Make BUFFER the current buffer, then evaluate any remaining
arguments.  The original current buffer is restored afterwards."
  (` (let ((buf (get-buffer-create (, bufname)))
           (oldbuf (current-buffer)))
       (set-buffer buf)
       (unwind-protect (progn (,@ body))
         (set-buffer oldbuf)))))

(defmacro ignoring-read-only (&rest body)
  "Evaluates it's args while the current is writeable, restoring read-only
afterwards."
  (` (let ((bro buffer-read-only))
       (setq buffer-read-only nil)
       (unwind-protect (progn (,@ body))
         (setq buffer-read-only bro)))))

(defun goto-marker (mark)
  (set-buffer (marker-buffer mark))
  (goto-char (marker-position mark)))


(defun smack-add-caption (cuid caption isnew)
  "Add an entry for CUID and CAPTION to the folder currently being updated.
ISNEW should be non-0 if this is a new message."
  (temp-set-buffer smack-folder-update-buf
    (goto-marker smack-last-caption-mark)
    (if (and (/= isnew 0) (null smack-first-new-msg))
        (setq smack-first-new-msg cuid))
    (insert (concat cuid))
    (indent-to-column (1+ smack-cuid-cols)) ; +1 for update-mark column
    (insert caption "\n")
    (set-marker smack-last-caption-mark (point))))

(defun smack-quit ()
  "Gracefully exit smackmail."
  (interactive)
  (smack-exit))

(defun smack-exit ()
  "Exits smackmail."
  (if (not smack-suspended)
      (progn (set-window-configuration smack-saved-window-configuration)
             ; Work around apparent bug in set-window-configuration
             (set-buffer (window-buffer (selected-window)))))
  (setq smack-reading nil)
  (setq smack-active nil)
  (setq smack-last-cuid 0)
  (kill-buffer smack-captions-buf)
  (kill-buffer smack-display-buf)
  (kill-buffer smack-metamail-buf)
  (kill-buffer smack-scratch-buf)
  (bury-buffer smack-composition-buf)  ; Really want to kill it safely.

  (if (and smack-started-from-shell smack-exit-to-shell)
      (save-buffers-kill-emacs)))


(defun smack-fetch-and-insert (seq imapbuf)
  (let* ((envelope (imap-fetch seq "ENVELOPE" 'ENVELOPE nil imapbuf))
	 (env-date (aref envelope 0))
	 (env-subject (aref envelope 1))
	 (env-from (address (aref envelope 2)))
	 (env-sender (address (aref envelope 3)))
	 (env-reply-to (address (aref envelope 4)))
	 (env-to (address (aref envelope 5)))
	 (env-cc (address (aref envelope 6)))
	 (env-bcc (aref envelope 7))
	 (env-in-reply-to (aref envelope 8))
	 (env-message-id (aref envelope 9 )))
    (temp-set-buffer smack-captions-buf
		     (ignoring-read-only
		       (smack-add-caption (number-to-string seq)
					  (format "%s %s %s" env-date env-subject env-from) 0)))))

(defun smack-get-captions (&rest folders)
  "Update captions for FOLDERS, which is a list of comma/whitespace
separated folder names, each optionally suffixed with '*' to indicate
the entire subtree under it, since DATE."
  (interactive)
  (if (null folders) (setq folders "INBOX"))  ; Default.
  (setq bat-first-new-msg nil) ; what's this all about?
  (set-marker smack-last-caption-mark (point))  ; Fix, we may not be in the captions buffer.

  (unwind-protect
      (let ((imapbuf (imap-open "breakout.horph.com" 993 'tls)))
	(with-current-buffer imapbuf
	  (imap-authenticate "mailtest" "PASSWID" imapbuf)
	  (imap-mailbox-select "INBOX" imapbuf)
	  (let ((max (imap-mailbox-get 'exists "INBOX" imapbuf)))
	    (dolist (seq (number-sequence (if (> max 10) (- max 10) 1) max))
	      (smack-fetch-and-insert seq imapbuf)))))
    (kill-buffer imapbuf)))


(defun address (a)
  (condition-case nil
      (let ((addy (car a)))
	(if (null (aref addy 0))
	    (format "%s@%s" (aref addy 2) (aref addy 3))
	  (format "%s <%s@%s>" (aref addy 0) (aref addy 2) (aref addy 3))))
    (error a)))

(defun disabled-key ()
  "Displays an error message saying that this key is disabled."
  (interactive)
  (error "That key is disabled here"))


(defun smack-make-captions-keymap ()
  "Returns the default smack-captions-mode keymap."
  (let ((km (make-keymap)))
    ;; disable alphabetic keys
    (let ((i 32))
      (while (< i 128)
        (define-key km (char-to-string i) 'disabled-key)
        (setq i (1+ i))))
    (define-key km " " 'smack-pick)
    (define-key km "n" 'smack-next-caption)
    (define-key km "p" 'smack-prev-caption)
    (define-key km "v" 'smack-scroll-body)
    (define-key km "R" 'smack-get-captions)
    (define-key km "Q" 'smack-quit)
    (define-key km "q" 'smack-suspend)
    km))

(defvar smack-captions-keymap (smack-make-captions-keymap))

(defvar smack-suspend-on-start nil
  "*If non-nil, smackmail will automatically suspend itself when it starts up
(so you can do other things while it's churning away).")


(defun smack-make-composition-keymap ()
  "Returns the default smack-composition-mode keymap."
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-C\C-C" 'smack-submit-2) ; gnu-style
    (define-key km "\C-X\C-S" 'smack-submit)
    (define-key km "\C-X\C-N" 'smack-submit-no-validate)
    (define-key km "\C-XS" 'smack-submit-no-ispell)
    (define-key km "\C-X\C-C" 'smack-abort-mail)
    (define-key km "\C-Xq" 'smack-suspend)
    (define-key km "\C-M" 'smack-mail-newline)
    (define-key km "\n" 'smack-mail-newline); for braindamaged telnet server
    (define-key km "\C-X\C-Y" 'smack-insert-body)
    (define-key km "\C-X\C-B" 'smack-toggle-blind)
    (define-key km "\C-X?" 'smack-mail-help)
    (define-key km "\er" 'smack-validate)
    (define-key km "\e$" 'ispell-word)
    km))

(defvar smack-composition-keymap (smack-make-composition-keymap))
(defvar smack-suspended nil)
(defvar smack-saved-window-configuration nil)
(defvar smack-caption-percentage 30
  "*Percentage of screen height used for the captions.  If negative, splits
windows horizontally.")
(defvar smack-replying-to 0 "Cuid of mail we're replying to.")

(defun smack-create-display-buf ()
  "Create the smack-display buffer, put it in smack-display-mode,
and put the initial display text into it."
  (set-buffer (get-buffer-create smack-metamail-buf))
  (comint-mode)
  (set-buffer (get-buffer-create smack-display-buf))
  (erase-buffer)
  (smack-display-mode)
  (let ((introf (concat smack-etc-dir "/intro"))
	(newsf (concat smack-etc-dir "/news")))
    (if (file-readable-p introf)
        (insert-file-contents introf))
    (replace "$sv$" smack-version)
    (goto-char (point-max))
    (if (file-readable-p newsf)
        (progn (newline) (insert-file-contents newsf)))))


(defun smack-display-mode ()
  "Major mode for smackmail's main display buffer"
  (text-mode)
  (setq smack-more nil)
  (setq mode-line-format
        '("---- Smack" smack-display
          (smack-more ("    " smack-display-scroll-help-str "  --More--(%p)"))))
  (setq smack-display "Mail")
  (setq major-mode 'smack-display-mode)
  (setq mode-name "SmackMail-Display"))

(defun smack-create-composition-buf ()
  "Create the smack-composition buffer, and put it in to smack-composition-mode."
  (set-buffer (get-buffer-create smack-composition-buf))
  (smack-composition-mode))

(defun smack-composition-mode ()
  "Major mode for composing (smack)mail.
Most normal GNU Emacs commands work here. (for example, C-f to go forward,
C-b to go backward, C-n for next line, C-p for previous line.)

--key--     --description--
\\[smack-submit]      Send the message
\\[smack-submit-no-validate]          Send the message without validating addresses
RET works intelligently in the headers area.
Use \\[open-line] in the headers area to add a new header."
  (text-mode)
  (auto-fill-mode 1)
  (setq mode-line-format
        '("---- SmackComposition (" (smack-local-blind "Blind" "No blind")
          ")    " smack-composition-help-mode-str))
  (setq major-mode 'smack-composition-mode)
  (setq mode-name "SmackMailCompo")
  (setq buffer-auto-save-file-name smack-composition-ckp-file)
  (use-local-map smack-composition-keymap)

  (save-excursion (run-hooks 'smack-composition-mode-hook))

  ;; This is here so it will get any keybindings from the hook
  (setq smack-composition-help-mode-str
        (substitute-command-keys
"[\\[smack-submit] sends, \\[smack-abort-mail] aborts, \\[smack-mail-help] for help]")))


(defun smack-captions-mode ()
  "Major mode for browsing email captions using SmackMail.
The following keys have special meaning.  Most other Emacs commands
still work as they normally do.  To make one-handed browsing easier,
the return and j keys work like C-n (next-line) and the k key works
like C-p (previous-line).

--key--     --description--
SPC         Scroll forward in message or read next message
n           Read next message
p           Read previous message
v           Scroll currently displayed message forward"
  (setq track-eol nil)
  (use-local-map smack-captions-keymap)
  (setq smack-state nil)
  (setq mode-line-format '("---- SmackMail  "
                           (smack-new-mail-p
                            ("(Mail:" smack-new-mail-count-str ")  "))
                           (global-mode-string ("  " global-mode-string))
                           "  " smack-captions-help-mode-str "  (%p)"
                           (smack-state ("  " smack-state))))
  (setq truncate-lines t)
  (setq major-mode 'smack-captions-mode)
  (setq mode-name "SmackMail-Captions")
  (setq buffer-read-only t)

  (save-excursion (run-hooks 'smack-captions-mode-hook))

  ;; These are here so they will get any keybindings from the hook
  (setq smack-captions-help-mode-str
        (substitute-command-keys "[\\[smack-help] for help]"))
  (setq smack-display-scroll-help-str
        (substitute-command-keys "[\\[smack-scroll-body] to scroll]")))


(defun smack-create-captions-buf ()
  "Create the smack-captions buffer, and put it in to smack-captions-mode."
  (set-buffer (get-buffer-create smack-captions-buf))
  (erase-buffer)
  (set-mark 0)
  (set-buffer-modified-p nil)
  (smack-captions-mode))


(defun smack-set-up-comp-screen ()
  (cond ((buffer-visible-p smack-composition-buf)
         (pop-to-buffer smack-composition-buf))
        (t
         (pop-to-buffer smack-display-buf)
         (cond ((not (equal smack-replying-to 0))
                (split-window)
                (other-window 1)))
         (switch-to-buffer smack-composition-buf))))


(defun smack-set-up-screen ()
  "Takes over the display, putting the various smack-buffers in their
correct locations and sizes."
  (when smack-reading
    (switch-to-buffer smack-display-buf)
    (delete-other-windows)
    (let ((h (window-height))
          (w (window-width)))
      (if (>= smack-caption-percentage 0)
          (split-window (selected-window)
                        (/ (* h (- 100 smack-caption-percentage)) 100))
        (split-window (selected-window)
                      (/ (* w (+ 100 smack-caption-percentage)) 100)
                      t))
      (other-window 1)
      (switch-to-buffer smack-captions-buf)))
  (when smack-sending
    (smack-set-up-comp-screen)))

(defun smack-suspend ()
  "Temporarily exits smackmail, hiding all its windows, and restoring
the previous screen layout."
  (interactive)

  (setq smack-suspended t)

  (set-window-configuration smack-saved-window-configuration)

  ; (bat-save-last-update)
  ; (bat-command "U\n")

  (bury-buffer smack-display-buf)
  (bury-buffer smack-captions-buf)
  (bury-buffer smack-composition-buf)

  (message "%s" (substitute-command-keys "Type \\[smackmail] to resume smackmail")))

(defun smack-enter ()
  "Makes sure everything is intialized for smackmail to run."
  (when (not smack-active)
    (get-buffer-create smack-scratch-buf)
    (smack-create-captions-buf)
    (smack-create-display-buf)
    (smack-create-composition-buf)
    (setq smack-saved-window-configuration
          (current-window-configuration))
    (setq smack-active t)
    t))

(defun smackmail (&optional folders since)
  "Starts up smackmail reading FOLDERS since DATE, prompting for either
 missing parameter."
  (interactive)

  (cond ((not smack-reading)
	 (save-window-excursion
	   (cond ((and smack-started-from-shell
		       (not smack-stingy-display)
		       smack-obnoxious-logo)
		  (switch-to-buffer "*smack-obnoxious-logo*")
		  (setq mode-line-format "In color!")
		  (setq truncate-lines t)
		  (erase-buffer)
		  (delete-other-windows)
		  (let ((obnxf (concat smack-etc-dir "/" "initscreen")))
		    (if (file-readable-p obnxf)
			(insert-file-contents obnxf))))))
	 (if (get-buffer "*smack-obnoxious-logo*")
	     (kill-buffer "*smack-obnoxious-logo*"))
	 (setq smack-reading t)
	 (smack-enter)
	 (if smack-suspend-on-start
	     (smack-suspend)
	     (smack-set-up-screen))
	 (smack-get-captions))
	(smack-suspended
	 ;; redo the window config if un-suspending
	 (if smack-suspended 
	     (setq smack-saved-window-configuration
		   (current-window-configuration)))
	 (setq smack-suspended nil)
	 (smack-set-up-screen))
	(t
	 (smack-set-up-screen))))
