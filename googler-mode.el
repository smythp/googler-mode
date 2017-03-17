(require 'button)

(defun googler-get-results (query)
  "Run search query with Googler and convert results from JSON to vector."
  (json-read-from-string
   (shell-command-to-string
    (concat "googler --json -C " query))))


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
      (insert "\n\n" abstract "\n\n"))))


(defun render-all-entries (sequence)
  (seq-map 'render-search-entry sequence))


(defun googler-results-buffer (query)
  (let ((results (googler-get-results query)))
    (progn 
      (get-buffer-create "*googler-results*")
      (with-current-buffer "*googler-results*"
	(let ((buffer-read-only nil))
	  (progn
	    (erase-buffer)
	    (insert "Results for " query "\n\n")
	    (render-all-entries results)
	    (read-only-mode)
	    (goto-char (point-min)))))
      (switch-to-buffer "*googler-results*"))))


(defun googler-search (&optional query)
  "Enter search term and display Googler results in a new buffer."
  (interactive)
  (googler-results-buffer
   (if query
       query
     (read-from-minibuffer "Googler search: "))))


