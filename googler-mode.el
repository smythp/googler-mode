(defun googler-sanitize-string (string)
  "Put escaped quotes around STRING."
  (concat "\"" (shell-quote-argument string) "\""))


(defun googler-get-results (query &optional results-number)
  "Run search query with Googler and convert results from JSON to vector."
  (let ((googler-number-results (cond (results-number results-number)
				      (googler-number-results googler-number-results)
				      (t 10))))
  (json-read-from-string
   (shell-command-to-string
    (concat "googler --json -C "
	    (if googler-number-results (format "-n %d " googler-number-results) "")
	    (googler-sanitize-string query))))))


(defun googler-build-search-entry (search-entry &optional begin-location)
  (let* ((title (cdr (assoc 'title search-entry)))
	 
	 (url (cdr (assoc 'url search-entry)))
	 (abstract (cdr (assoc 'abstract search-entry)))
	 (begin-location (if begin-location begin-location 0))
	 (end-location (+ begin-location (length title) 2 (length abstract))))
    `((title . ,title)
      (url . ,url)
      (description . ,abstract)
      (title-start . ,begin-location)
      (title-end . ,(+ begin-location (length title)))
      (description-start . ,(+ begin-location (length title) 2))
      (description-end . ,end-location)
      (location-range . ,(cons begin-location end-location)))))


(defun googler-cumulative-build (results &optional carry)
  "Build lists of entry data based on RESULTS and bind global variables for navigation. CARRY is the starting point and can be used as offset for text at the beginning of the buffer before results."
  (if (eq results [])
      nil
    (let* ((built-list (googler-build-search-entry (elt results 0) carry))
	   (begin-location (+ 2(cdr (cdr (assoc 'location-range built-list))))))
      ;; end-location)))
      (cons built-list (googler-cumulative-build (seq-drop results 1) begin-location)))))


(defun googler-entity-list (entry-list assoc-symbol)
  "Takes built list of results as ENTRY-LIST and creates list based on an association with ASSOC-SYMBOL.

For example, 

(googler-build-navigation-locations entry-list 'title-start) 

creates a list of title locations."
  (mapcar (lambda (x) (cdr (assoc assoc-symbol x))) entry-list))


(defun googler-render-entry (entry)
  "Uses an ENTRY from the built list of results to insert text into a buffer in the correct format."
  (insert (cdr (assoc 'title entry)) "\n\n" (cdr (assoc 'description entry)) "\n\n"))


(defun googler-results-buffer (query)
  "Update the results buffer based on a QUERY."
  (let* ((preamble (concat "Results for: " query "\n\n"))
	 (built-list (googler-cumulative-build (googler-get-results query) (1+ (length preamble))))
	 (buffer (get-buffer-create "*googler-results*"))
	 (origin-buffer (buffer-name)))
    (with-current-buffer buffer
      (progn
	(let ((buffer-read-only nil))
	  (setq googler-origin-buffer origin-buffer)
	  (erase-buffer)
	  (insert preamble)
	  (mapcar 'googler-render-entry built-list)
	  (setq googler-entries-list built-list)
	  (setq googler-results-locations (mapcar 'car (googler-entity-list built-list 'location-range)))
	  (googler-mode)
	  (goto-char (car (cdr (assoc 'location-range (car built-list)))))
	  (switch-to-buffer buffer))))))


(defun googler-search (prefix)
  "Enter search term and display Googler results in a new buffer."
  (interactive "P")
  (let ((googler-number-results (if current-prefix-arg current-prefix-arg googler-number-results)))
    (googler-results-buffer
     (if (and (mark) (use-region-p))
	 (buffer-substring (region-beginning) (region-end))
       (read-from-minibuffer "Googler search: ")))))


(defun googler-lucky (query)
  "Return \"I'm feeling lucky\" search for QUERY."
  (shell-command
   (concat "googler --lucky " query)))


(defun googler-between-p (value cons-cell)
  "Return true if VALUE is between or equal to the values in CONS-CELL."
  (let ((minimum (min (car cons-cell) (cdr cons-cell)))
	(maximum (max (car cons-cell) (cdr cons-cell))))
    (if (and (<= minimum value) (<= value maximum))
	t)))


(defun googler-get-entry-at-point ()
  "Get entry at point in Googler buffer."
  (if (equal (buffer-name) "*googler-results*")
      (car
       (delq nil
	     (mapcar (lambda (entry)
		       (let ((range (cdr (assoc 'location-range entry))))
			 (if (googler-between-p (point) range)
			     entry)))
		     googler-entries-list)))))
  


(defun googler-open-result (&optional use-eww)
  (interactive)
  (if (equal (buffer-name) "*googler-results*")
      (let ((url (cdr (assoc 'url (googler-get-entry-at-point)))))
	(if use-eww
	    (eww-browse-url url)
	  (browse-url url)))))


(defun googler-open-result-eww ()
  (interactive)
  "Open link in eww."
  (googler-open-result t))


(defun googler-next ()
  "Move point to next result while on the Googler results page."
  (interactive)
  (let ((next-item (car (delq nil
			      (mapcar (lambda (x) (if (< (point) x) x))
				      googler-results-locations)))))
    (if next-item
	(goto-char next-item)
      (message "Last search entry"))))


(defun googler-previous ()
  "Move point to next result while on the Googler results page."
  (interactive)
  (let ((previous-item 
   (car (last (delq nil
	      (mapcar (lambda (x) (if (> (point) x) x))
		      googler-results-locations))))))
    (if previous-item
	(goto-char previous-item)
      (message "No previous search entry"))))


(defun googler-get-first-result (query)
  (let* ((results (googler-get-results query 2)))
    (elt results 0)))


(defun googler-get-first-result-url (query)
  (let ((result (googler-get-first-result query)))
    (cdr (assoc 'url result))))


(defun googler-insert-link-of-type (type)
  "Insert link of symbol TYPE into origin buffer."
  (interactive)
  (let* ((entry (googler-get-entry-at-point))
	 (url (cdr (assoc 'url entry)))
	 (title (cdr (assoc 'title entry))))
    (progn
      (switch-to-buffer googler-origin-buffer)
      (cond ((equal type (or 'org 'orgmode)) (insert (concat "[[" url "][" title "]]")))
	    ((equal type 'markdown) (insert (concat "[" title "](" url ")")))
	    ((equal type 'html) (insert (concat "<a href=\"" url "\">" title "</a>")))))))


(defun googler-insert-markdown-link ()
  "Insert markdown link of entry at point into origin buffer."
  (interactive)
  (googler-insert-link-of-type 'markdown))


(defun googler-insert-org-link ()
  "Insert orgmode link of entry at point into origin buffer."
  (interactive)
  (googler-insert-link-of-type 'org))


(defun googler-insert-html-link ()
  "Insert HTML link of entry at point into origin buffer."
  (interactive)
  (googler-insert-link-of-type 'html))


(defun googler-org-link ()
  (interactive)
  (if (and (mark) (use-region-p))
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
  (kbd "i i") 'googler-insert-link)

(define-key googler-mode-map
  (kbd "i o") 'googler-insert-org-link)

(define-key googler-mode-map
  (kbd "i m") 'googler-insert-markdown-link)

(define-key googler-mode-map
  (kbd "i h") 'googler-insert-html-link)


(define-key googler-mode-map
  (kbd "RET") 'googler-open-result)

(define-key googler-mode-map
  (kbd "<C-return>") 'googler-open-result-eww)

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



