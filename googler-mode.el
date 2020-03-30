(require 'json)

(defgroup googler-mode nil
  "Customization group for the googler-mode package for searching google and creating links."
  :group 'communication)


(defcustom googler-use-eww nil
  "If non-nil, googler-mode will use eww as the default web browser when opening links.")


(defcustom googler-number-results nil
  "If non-nil, googler-mode will return 10 results on a search. Otherwise, will return the specified number.")


(defcustom googler-title-font 'info-title-1
  "Font face as symbol for title in results.")


(defcustom googler-description-font nil
  "Font face as symbol for description in results.")


(defcustom googler-default-link-insert-type 'html
  "If mode is not successfully detected, insert this type of link.")




(defun char-bracket-p (char)
  "Check if a character in a sequence of characters is a square bracket."
  (if (= char 91)
      t))

(elt "fart" 0)

(defun find-first-bracket (string &optional iterator)
  "Recur through a string and find the index of the first square bracket."
  (if (> (1+ iterator) (length string))
      nil
    (let ((current-character (elt string iterator)))
      (if (= current-character 91)
	  iterator
	(find-first-bracket string (1+ iterator))))))


(defun remove-all-before-first-bracket (string)
  "Remove all in STRING before first square bracket."

       (let ((bracket-position (find-first-bracket string 0)))
	 (if bracket-position
	     (substring string bracket-position))))


(defun googler-sanitize-string (string)
  "Put escaped quotes around STRING."
  (concat "\"" (shell-quote-argument string) "\""))


(defun googler-get-results (query &optional results-number)
  "Run search query with Googler and convert results from json to vector."
  (let* ((googler-number-results (cond (results-number results-number)
				      (googler-number-results googler-number-results)
				      (t 10)))
	(googler-command-output    (shell-command-to-string
				    (concat "googler --json -C "
					    (if googler-number-results (format "-n %d " googler-number-results) "")
					    (googler-sanitize-string query))))
	;; If the search isn't exact there is a little message a at the beginning of the results
	;; that isn't JSON. This removes it if it's there.
	(cleaned-output (if (= (elt googler-command-output 0) 91)
			    googler-command-output
			  (remove-all-before-first-bracket googler-command-output))))
    (json-read-from-string
     cleaned-output)))



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


(defun googler-insert-fontified (string &optional font)
  "Insert text STRING with optional font face FONT. If font not specified or nil, fall back to a normal insert."
  (if font
      (put-text-property 0 (length string) 'font-lock-face font
			 string))
  (insert string))


(defun googler-render-entry (entry )
  "Uses an ENTRY from the built list of results to insert text into a buffer in the correct format."
  (progn
    (googler-insert-fontified (cdr (assoc 'title entry)) googler-title-font)
    (insert "\n\n")
    (googler-insert-fontified  (cdr (assoc 'description entry)) googler-description-font)
    (insert "\n\n")))


(defun googler-results-buffer (query)
  "Update the results buffer based on a QUERY."
  (let* ((preamble (concat "Results for: " query "\n\n"))
	 (built-list (googler-cumulative-build (googler-get-results query) (1+ (length preamble))))
	 (buffer (get-buffer-create "*googler-results*"))
	 (origin-buffer (buffer-name))
	 (origin-region-active-p (region-active-p))	 
	 (origin-region (if origin-region-active-p (cons (region-beginning) (region-end)))))
    (with-current-buffer buffer
      (progn
	(let ((buffer-read-only nil))
	  (setq googler-origin-region-active-p origin-region-active-p)
	  (setq googler-origin-region origin-region)
	  (setq googler-origin-buffer origin-buffer)
	  (erase-buffer)
	  (insert preamble)
	  (mapcar 'googler-render-entry built-list)
	  (setq googler-entries-list built-list)
	  (setq googler-results-locations (mapcar 'car (googler-entity-list built-list 'location-range)))
	  (googler-mode)
	  (switch-to-buffer buffer)
	  (goto-char (car (cdr (assoc 'location-range (car built-list))))))))))


(defun googler-search (prefix)
  "Enter search term and display Googler results in a new buffer."
  (interactive "P")
  (if (and prefix (= (car prefix) 4))
      (googler-lucky (read-from-minibuffer "Googler lucky search: "))
    (let ((googler-number-results (if current-prefix-arg current-prefix-arg googler-number-results)))
      (googler-results-buffer
       (if (and (mark) (use-region-p))
	   (buffer-substring (region-beginning) (region-end))
	 (read-from-minibuffer "Googler search: "))))))


(defun googler-lucky (query)
  "Open browser for \"I'm feeling lucky\" and search for QUERY."
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
  "Open entry at point in eww."
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


(defun googler-autolink ()
  "Replace text in buffer with working link based on the current mode."
  (interactive)
  (let* ((region-active (if (and (mark) (region-active-p)) t))
	 (query (if region-active
		   (buffer-substring (region-beginning) (region-end))
		  (read-from-minibuffer "Insert link for search: ")))
	 (first-result (googler-get-first-result query))
	 (title query)
	 (url (cdr (assoc 'url first-result))))
    (progn
      (if region-active (delete-region (region-beginning) (region-end)))
      (insert
       (cond ((googler-origin-link-p 'org t) (googler-generate-link 'org title url))
	     ((googler-origin-link-p 'html t) (googler-generate-link 'html title url))
	     ((googler-origin-link-p 'markdown t) (googler-generate-link 'markdown title url))
	     (t (message "No appropriate link type found for buffer.")))))))


(defun googler-get-first-result-url (query)
  (let ((result (googler-get-first-result query)))
    (cdr (assoc 'url result))))


(defun googler-generate-link (type title url)
  "Return link of TYPE with link title TITLE  and link url URL."
  (cond ((equal type (or 'org 'orgmode)) (concat "[[" url "][" title "]]"))
	((equal type 'markdown) (concat "[" title "](" url ")"))
	((equal type 'html) (concat "<a href=\"" url "\">" title "</a>"))
	((equal type 'title) title)))


(defun googler-insert-link-of-type (type &optional in-entry current-buffer)
  "Insert link of symbol TYPE into origin buffer. If optional IN-ENTRY specified, use that entry instead of the entry at point. If optional CURRENT-BUFFER specified, use current buffer instead of origin buffer."
  (interactive)
  (let* ((entry (if in-entry in-entry (googler-get-entry-at-point)))
	 (url (cdr (assoc 'url entry)))
	 (title (cdr (assoc 'title entry))))
    (progn
      (if (not current-buffer) (switch-to-buffer googler-origin-buffer))
      (if googler-origin-region-active-p
	  (delete-region (car googler-origin-region) (cdr googler-origin-region)))
      (insert (googler-generate-link type title url)))))

(defun googler-insert-markdown-link ()
  "Insert markdown link of entry at point into origin buffer."
  (interactive)
  (googler-insert-link-of-type 'markdown))


(defun googler-insert-org-link ()
  "Insert orgmode link for entry at point into origin buffer."
  (interactive)
  (googler-insert-link-of-type 'org))


(defun googler-insert-html-link ()
  "Insert HTML link for entry at point into origin buffer."
  (interactive)
  (googler-insert-link-of-type 'html))


(defun googler-insert-title ()
  "Insert title for entry at point into origin buffer."
  (interactive)
  (googler-insert-link-of-type 'title))


(defun googler-insert-link ()
  "Insert link for entry at point into origin buffer based on the mode active in origin buffer."
  (interactive)
  (cond ((googler-origin-link-p 'html) (googler-insert-html-link))
	((googler-origin-link-p 'org) (googler-insert-org-link))
	((googler-origin-link-p 'markdown) (googler-insert-markdown-link))
	(t (googler-insert-title))))


(defun googler-insert-all-org-link ()
  "Insert orgmode link for all entries into origin buffer."
  (interactive)
  (mapcar (lambda (entry) (progn (googler-insert-link-of-type 'org entry) (insert "\n")))
	  googler-entries-list))


(defun googler-insert-all-markdown-link ()
  "Insert markdown link for all entries into origin buffer."
  (interactive)
  (mapcar (lambda (entry) (progn (googler-insert-link-of-type 'markdown entry) (insert "  \n")))
	  googler-entries-list))


(defun googler-insert-all-html-link ()
  "Insert HTML link for all entries into origin buffer."
  (interactive)
  (mapcar (lambda (entry) (progn (googler-insert-link-of-type 'html entry) (insert "<br>\n") (indent-for-tab-command)))
	  googler-entries-list))


(defun googler-insert-all-titles ()
  "Insert titles for all entries into origin buffer."
  (interactive)
  (mapcar (lambda (entry) (progn (googler-insert-link-of-type 'title entry) (insert "\n")))
	  googler-entries-list))


(defun googler-origin-link-p (link-type &optional current-as-origin-buffer)
  "Return true if MODE in the category for LINK-TYPE. If CURRENT-AS-ORIGIN-BUFFER is non-nil, use current buffer instead of googler-origin-buffer."
  
  (member (if current-as-origin-buffer major-mode (with-current-buffer googler-origin-buffer major-mode))
	  (assoc link-type googler-mode-assoc)))


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
  (kbd "i t") 'googler-insert-title)

(define-key googler-mode-map
  (kbd "i o") 'googler-insert-org-link)

(define-key googler-mode-map
  (kbd "i m") 'googler-insert-markdown-link)

(define-key googler-mode-map
  (kbd "i h") 'googler-insert-html-link)

(define-key googler-mode-map
  (kbd "i a o") 'googler-insert-all-org-link)

(define-key googler-mode-map
  (kbd "i a m") 'googler-insert-all-markdown-link)

(define-key googler-mode-map
  (kbd "i a h") 'googler-insert-all-html-link)

(define-key googler-mode-map
  (kbd "i a t") 'googler-insert-all-titles)

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



(defvar googler-mode-assoc
  '((html html-mode web-mode)
    (markdown markdown-mode)
    (org org-mode))
  "Association list for functions that detect modes for their behavior.")

(provide 'googler-mode)
