(require 'button)

(defun googler-sanitize-string (string)
  "Put escaped quotes around STRING."
  (concat "\"" (shell-quote-argument string) "\""))


(defun googler-get-results (query &optional results-number)
  (let ((googler-number-results (cond (results-number results-number)
				      (googler-number-results googler-number-results)
				      (t 10))))
				  
  "Run search query with Googler and convert results from JSON to vector."
  (json-read-from-string
   (shell-command-to-string
    (concat "googler --json -C "
	    (if googler-number-results (format "-n %d " googler-number-results) "")
	    (googler-sanitize-string query))))))


(defun insert-hyperlink (link text)
  (insert-button text
		 'action (lambda (x) (browse-url (button-get x 'url)))
		 'url link))


(defun render-search-entry (search-entry)
  (let ((title (cdr (assoc 'title search-entry)))
	(url (cdr (assoc 'url search-entry)))
	(abstract (cdr (assoc 'abstract search-entry))))
    (progn 
      (insert-hyperlink url title)
      (insert "\n\n" abstract "\n\n")
      ;; return position shift after insert
      ;; for user ability to move between results
      (+ (length title) (length abstract) 4))))


(defun googler-cumulative-list (in-list carry)
  "Make each value of IN-LIST the cumulative total of that value and all previous values."
  (if in-list
       (cons
	(+ (car in-list) carry)
	(googler-cumulative-list (cdr in-list) (+ carry (car in-list))))))


(defun render-all-entries (vector offset)
  "Inserts text for results from VECTOR as a side effect. Returns the buffer locations of the beginning of each entry as a list so that user can move between them. Offset is used to shift results by the length of any preamble at the beginnig of buffer."
  (googler-cumulative-list
  ;; put offset at beginning of list with cons
  (cons offset
	(seq-map 'render-search-entry vector)) 0))


(defun googler-results-buffer (query)
  "Update the results buffer based on a QUERY."
  (let ((results (googler-get-results query)))
    (progn 
      (get-buffer-create "*googler-results*")
      (with-current-buffer "*googler-results*"
	(progn
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    (insert "Results for " query "\n\n")
	    (setq googler-query-locations (cons 13 (+ 13 (length query))))
	    (setq googler-results-locations (render-all-entries results (+ (length query) 15))))
	    (googler-mode)
	    (if googler-use-eww
		(setq-local
		 browse-url-browser-function 'eww-browse-url))
	    (goto-char (car googler-results-locations)))))
      (switch-to-buffer "*googler-results*")))


(defun googler-search (begin end)
  "Enter search term and display Googler results in a new buffer."
  (interactive "rP")
  (let ((googler-number-results (if current-prefix-arg current-prefix-arg googler-number-results)))
    (googler-results-buffer
     (if (use-region-p)
	 (buffer-substring begin end)
       (read-from-minibuffer "Googler search: ")))))


(defun googler-lucky (query)
  "Return \"I'm feeling lucky\" search for QUERY."
  (shell-command
   (concat "googler --lucky " query)))


(defun googler-next ()
  "Move point to next result while on the Googler results page."
  (interactive)
  (goto-char
   (car (delq nil
	      (mapcar (lambda (x) (if (< (point) x) x))
		      googler-results-locations)))))


(defun googler-previous ()
  "Move point to next result while on the Googler results page."
  (interactive)
  (goto-char
   (car (last (delq nil
	      (mapcar (lambda (x) (if (> (point) x) x))
		      googler-results-locations))))))


(defun googler-get-first-result (query)
  (let* ((results (googler-get-results query 2)))
    (elt results 0)))


(defun googler-get-first-result-url (query)
  (let ((result (googler-get-first-result query)))
    (cdr (assoc 'url result))))


(defun googler-org-link-gen (link-text)
  "Create an orgmode-formatted link populated by LINK-TEXT and the first result of a Google search for that text."
  (concat "[[" (googler-get-first-result-url link-text) "][" link-text "]]"))


(defun googler-org-link ()
  (interactive)
  (if (use-region-p)
      (let ((begin (region-beginning))
	    (end (region-end)))
      (save-excursion
	(let ((link (googler-org-link-gen (buffer-substring (region-beginning) (region-end)))))
	  ;; (goto-char begin)
	;; (delete-region (region-beginning) (region-end))))
	  (delete-region begin end)
	  (insert link))))
    (insert (googler-org-link-gen (read-from-minibuffer "Generate link from text: ")))))


(define-derived-mode googler-mode special-mode "Googler"
  "Mode for searching Google.
   \\{googler-mode-map}")

(define-key googler-mode-map
  "n" 'googler-next)

(define-key googler-mode-map
  "p" 'googler-previous)


(define-key googler-mode-map
  "g" 'googler-search)

(defcustom googler-use-eww nil
  "If non-nil, googler-mode will use eww as the default web browser when opening links.")


(defcustom googler-number-results nil
    "If non-nil, googler-mode will return 10 results on a search. Otherwise, will return the specified number.")


(defun googler-self-insert-command (N)
  "Custom keypress handler for Googler mode."
  (interactive "p")
  (if (googler-edit-allowed-p)
      (let ((buffer-read-only nil))
	(progn
	  (googler-shift-offset 1)
	  (self-insert-command N)))))


(defun googler-key-backspace (&optional arg)
  "Backspaces in googler-mode if in area that allows it."
  (interactive "p")
  (let ((num-chars  (if arg arg 1)))
    (if (googler-edit-allowed-p)
	(let ((buffer-read-only nil))
	  (progn
	    (googler-shift-offset 1 t)
	    (backward-delete-char-untabify num-chars))))))


  (defun googler-edit-allowed-p ()
    "Checks if editing is allowed at point in googler-mode buffer."
    (if (and (>= (point) (car googler-query-locations)) (<= (point) (cdr googler-query-locations)))
	t))

  
    


(defun googler-shift-offset (num-characters &optional decrement)
  "Increase or decrease the locations for key points in the *googler-results* buffer by NUM-CHARACTERS. Used when inserting text into the buffer after its creation. Increments the buffer by default, decrements the buffer if DECREMENT is non-nil."
  (let ((plus-or-minus (if (not decrement)
			   '+
			 '-)))
    (progn
      (setq googler-results-locations (mapcar (lambda (x) (funcall plus-or-minus x num-characters)) googler-results-locations))
      (setq googler-query-locations
	    (cons (car googler-query-locations) (funcall plus-or-minus (cdr googler-query-locations) num-characters))))))


(define-key googler-mode-map [remap self-insert-command] 'googler-self-insert-command)


(define-key googler-mode-map
  "DEL" 'googler-key-backspace)
