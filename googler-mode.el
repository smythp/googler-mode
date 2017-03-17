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
	  (let ((buffer-read-only nil))
	    (progn
	      (erase-buffer)
	      (insert "Results for " query "\n\n")
	      (setq googler-results-locations (render-all-entries results (+ (length query) 15)))
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


(defun googler-next ()
  "Move point to next result while on the Googler results page."
  (interactive)
  (goto-char
   (car (delq nil
	      (mapcar (lambda (x) (if (< (point) x) x))
		      googler-results-locations)))))


(defun googler-next ()
  "Move point to next result while on the Googler results page."
  (interactive)
  (goto-char
   (car (delq nil
	      (mapcar (lambda (x) (if (> (point) x) x))
		      googler-results-locations)))))


(defun googler-previous ()
  "Move point to next result while on the Googler results page."
  (interactive)
  (goto-char
   (car (last (delq nil
	      (mapcar (lambda (x) (if (> (point) x) x))
		      googler-results-locations))))))


;; (defun googler-move (&optional back)
;;   "Move forward or back in the Googler results page. If optional BACK is non-nil, moves back, else moves forward."
;;   (let ((valid-locations (delq nil (mapcar (lambda (x) (if (< (point) x) x)) googler-results-locations))))
;;     (goto-char
;;        (car valid-locations))))
;; ;	 (car (last valid-locations))
