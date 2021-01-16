;;; nnmaild.el --- Maildir spool access for Gnus

;; Author: Juan Jose Garcia-Ripoll <juanjose.garciaripoll@gmail.com>

;; This software is distributed without any warranty, under the
;; GNU General Public License. See <https://www.gnu.org/licenses/>.

;;; Code:

(require 'gnus)
(require 'nnheader)
(require 'nnmail)
(require 'nnoo)
(require 'nnml)

(nnoo-declare nnmaild)

(defvoo nnmaild-directory message-directory
  "Spool directory for the nnmaild mail backend.")

(defvoo nnmaild-flag-separator (if (eq system-type 'windows-nt) "!" ":")
  "Separator for maildir file names, to split the flags.")

(defvoo nnmaild-data-file ".nnmaild-data.el"
  "File where message ids are saves")

(defvoo nnmaild-get-new-mail t
  "If non-nil, nnmaild will check the incoming mail file and split the mail.")


(defconst nnmaild-version "nnmaild 0.1"
  "nnmaild version.")

(defvoo nnmaild-current-directory nil)
(defvoo nnmaild-current-group nil)
(defvoo nnmaild-status-string "")
(defvoo nnmaild-group-alist nil)
(defvoo nnmaild-active-timestamp nil)
(defvoo nnmaild-file-coding-system nnmail-file-coding-system)

(defvoo nnmaild--data nil
  "Variable to store data loaded from files.")


;;; Data structures

(cl-defstruct (nnmaild--data
               (:constructor nnmaild--make-data (min max hash path))
               ;; Makes it more easily printable
               (:type list))
  min max hash path)

(defsubst nnmaild--allocate ()
  (cl-incf (nnmaild--data-max nnmaild--data)))

(defsubst nnmaild--copy-data (data)
  (nnmaild--make-data
   (nnmaild--data-min data)
   (nnmaild--data-max data)
   (gnus-make-hashtable (hash-table-size (nnmaild--data-hash data)))
   (nnmaild--data-path data)))

(defsubst nnmaild--make-empty-data (path &optional l)
  (nnmaild--make-data
   1 0 (gnus-make-hashtable (* 2 (or l 10))) path))

(defsubst nnmaild--data-size (data)
  (/ (hash-table-count (nnmaild--data-hash data)) 2))

(defsubst nnmaild--min ()
  (nnmaild--data-min nnmaild--data))

(defsubst nnmaild--max ()
  (nnmaild--data-max nnmaild--data))

(defsubst nnmaild--hash ()
  (nnmaild--data-hash nnmaild--data))

(defsubst nnmaild--size ()
  (nnmaild--data-hash nnmaild--data))

(cl-defstruct (nnmaild--art
               (:constructor nnmaild--make-art (number suffix nov))
               (:type list))
  number suffix nov)

(defsubst nnmaild--prefix-to-art (prefix)
  (gethash prefix (nnmaild--hash) nil))


;;; Interface functions.

(nnoo-define-basics nnmaild)

(defun nnmaild-group-pathname (group &optional file server)
  "Return an absolute file name of FILE for GROUP on SERVER."
  (nnmail-group-pathname group nnmaild-directory file))

(deffoo nnmaild-retrieve-headers (sequence &optional group server fetch-old)
  "Return a list of headers in NOV format for the given group"
  (when (nnmaild-possibly-change-directory group server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
	  (if (stringp (car sequence))
	      'headers
        (let* ((nnmaild--data (nnmaild--scan-group-dir nnmaild-current-directory))
               (min (nnmaild--min))
               (max (nnmaild--max)))
	      (with-current-buffer nntp-server-buffer
            (erase-buffer)
	        (dolist (article sequence)
              (when (and (>= article min) (<= article max))
                (when-let ((nov (nnmaild--article-nov article)))
                  (insert nov)))))
          'nov)))))

(deffoo nnmaild-open-server (server &optional defs)
  "Open the SERVER and correct definitions."
  (nnoo-change-server 'nnmaild server defs)
  (when (not (file-exists-p nnmaild-directory))
    (ignore-errors (make-directory nnmaild-directory t)))
  (cond
   ((not (file-exists-p nnmaild-directory))
    (nnmaild-close-server)
    (nnheader-report 'nnmaild "Couldn't create directory: %s" nnmaild-directory))
   ((not (file-directory-p (file-truename nnmaild-directory)))
    (nnmaild-close-server)
    (nnheader-report 'nnmaild "Not a directory: %s" nnmaild-directory))
   (t
    (nnheader-report 'nnmaild "Opened server %s using directory %s"
		             server nnmaild-directory)
    t)))

(deffoo nnmaild-request-regenerate (server)
  "Regenerate server information."
  t)

(deffoo nnmaild-request-article (id &optional group server buffer)
  "Request the article denoted by ID from GROUP. ID can be an article number
or a string with a mail ID."
  (nnmaild-possibly-change-directory group server)
  (let* ((nntp-server-buffer (or buffer nntp-server-buffer))
	     (file-name-coding-system nnmail-pathname-coding-system)
         (nnmaild--data (nnmaild--scan-group-dir nnmaild-current-directory 'fast))
         (article (if (numberp id)
                      id
	                (or (nnmaild--data-find-id id)
                        (catch 'return
                          (dolist (pair nnmaild-group-alist nil)
                            (let* ((other-group (car pair))
                                   (group-path (nnmaild-group-pathname other-group nil server)))
                              (setq nnmaild--data (nnmaild--scan-group-dir group-path 'fast))
                              (when-let ((article (nnmaild--data-find-id id)))
                                (setq group other-group)
                                (throw 'return article))))))))
         (path (and article (nnmaild--data-article-to-file article))))
    (cond
     ((not path)
      (nnheader-report 'nnmaild "No such article: %s" id))
     ((not (file-exists-p path))
      (nnheader-report 'nnmaild "No such file: %s" path))
     ((file-directory-p path)
      (nnheader-report 'nnmaild "File is a directory: %s" path))
     ((not (save-excursion (let ((nnmail-file-coding-system
				                  nnmaild-file-coding-system))
			                 (nnmail-find-file path))))
      (nnheader-report 'nnmaild "Couldn't read file: %s" path))
     (t
      (nnheader-report 'nnmaild "Article %s retrieved" id)
      ;; We return the article number.
      (cons group article)))))

(deffoo nnmaild-request-group (group &optional server dont-check info)
  "Request GROUP from SERVER, outputting the number of messages, lowest message
number and highest message number."
  (let ((file-name-coding-system nnmail-pathname-coding-system))
    (cond
     ((not (nnmaild-possibly-change-directory group server))
      (nnheader-report 'nnmaild "Invalid group (no such directory)"))
     ((not (file-exists-p nnmaild-current-directory))
      (nnheader-report 'nnmaild "Directory %s does not exist"
		               nnmaild-current-directory))
     ((not (file-directory-p nnmaild-current-directory))
      (nnheader-report 'nnmaild "%s is not a directory" nnmaild-current-directory))
     (dont-check
      (nnheader-report 'nnmaild "Group %s selected" group)
      t)
     (t
      (nnheader-re-read-dir nnmaild-current-directory)
      (let ((nmaild--data (nnmaild--scan-group-dir nnmaild-current-directory)))
        (when info
          (nnmaild--load-and-update-info group info server))
	    (nnheader-report 'nnmaild "Selected group %s with %s messages"
                         group (nnmaild--size))
	    (nnheader-insert "211 %d %d %d %s\n" (nnmaild--size)
                         (nnmaild--min) (nnmaild--max)))))))

(deffoo nnmaild-request-group-scan (group &optional server info)
  (let ((nnmaild--data (nnmaild--load-and-update-info group info server)))
    (nnheader-report 'nnmaild
                     "Group scan: %s"
                     (format
	                  "211 %d %d %d %S\n" (nnmaild--size) (nnmaild--min) (nnmaild--max)
	                  group))
    (with-current-buffer nntp-server-buffer
	  (erase-buffer)
	  (insert
	   (format
	    "211 %d %d %d %S\n" (nnmaild--size) (nnmaild--min) (nnmaild--max)
	    group))
	  t)))

(deffoo nnmaild-request-scan (&optional group server)
  (nnmaild-possibly-change-directory group server)
  (cond
   (group
    (nnmail-get-new-mail 'nnmaild 'nnmaild-save-incremental-nov nnmaild-directory group))
   ((nnmail-get-new-mail-per-group)
    (nnmaild-request-list)
    (dolist (entry nnmaild-group-alist)
      (nnmaild-request-scan (car entry) server)))
   (t
    (nnmail-get-new-mail 'nnmaild 'nnmaild-save-incremental-nov nnmaild-directory nil))))

(deffoo nnmaild-request-update-info (group info &optional server)
  (nnmaild--load-and-update-info group info server)
  t)

(deffoo nnmaild-close-group (group &optional server)
  t)

(deffoo nnmaild-request-create-group (group &optional server args)
  nil)

(deffoo nnmaild-request-list (&optional server dir)
  (nnheader-insert "")
  (save-excursion
    (nnmaild--map-group-dirs 'nnmaild-request-list-1 server dir)
    (setq nnmaild-group-alist (nnmail-get-active))
    (message "Active list %S" nnmaild-group-alist)
    t))

(defun nnmaild--map-group-dirs (fn server dir)
  (nnmaild-possibly-change-directory nil server)
  (let ((file-name-coding-system nnmail-pathname-coding-system)
        (nnmaild-toplev
	     (file-truename (or dir (file-name-as-directory nnmaild-directory)))))
    (dolist (group-dir (cl-remove-if-not 'nnmaild--valid-maildir-p
                                         (nnheader-directory-files (expand-file-name nnmaild-toplev)
                                                                   t nil t)))
      (funcall fn group-dir (file-name-nondirectory group-dir) server))))


(defun nnmaild-request-list-1 (group-dir group server)
  (let* ((data (nnmaild--scan-group-dir group-dir)))
    (with-current-buffer nntp-server-buffer
      (goto-char (point-max))
      (nnheader-report 'nnmaild
                       "Group scan %s"
                       (format "%s %.0f %.0f y\n"
                               (file-name-nondirectory group-dir)
                               (nnmaild--data-max data)
                               (nnmaild--data-min data)))
      (insert (format "%s %.0f %.0f y\n"
                      (file-name-nondirectory group-dir)
                      (nnmaild--data-max data)
                      (nnmaild--data-min data))))))

(defun nnmaild--valid-maildir-p (dir)
  (cl-every (lambda (dir)
              (and (file-exists-p dir)
                   (file-directory-p dir)))
            (cons dir (mapcar (lambda (x) (expand-file-name x dir))
                              '("cur" "new" "tmp")))))

(deffoo nnmaild-request-newgroups (date &optional server)
  (nnmaild-request-list server))

(deffoo nnmaild-request-list-newsgroups (&optional server)
  nil)

(deffoo nnmaild-request-set-mark (group actions &optional server)
  (nnmaild-possibly-change-directory group server)
  (let* ((file-name-coding-system nnmail-pathname-coding-system)
         (group-dir (nnmaild-group-pathname group nil server))
         (nnmaild--data (nnmaild--scan-group-dir group-dir)))
    (maphash (lambda (prefix art)
               (when (stringp prefix)
                 (let ((article (nnmaild--art-number art))
                       (suffix (nnmaild--art-suffix art)))
                   (dolist (triplet actions)
                     (let ((range (car triplet))
                           (action (cadr triplet))
                           (marks (caddr triplet)))
                       (when (member article range)
                         (setq suffix (nnmaild--act-on-suffix suffix action marks)))))
                   (nnmaild--commit-new-suffix prefix suffix art))))
             (nnmaild--hash))))

(deffoo nnmaild-request-update-mark (group article mark)
  (nnheader-report 'nnmaild "Received mark %S for article %S" mark article)
  (let ((known-mark (cdr (assoc mark '((?R . (read))
                                       (?! . (tick)))))))
    (when known-mark
      (nnmaild-request-set-mark group `(((,article) add ,known-mark)))))
  mark)

(deffoo nnmaild-request-expire-articles (articles group &optional server force)
  nil)

(deffoo nnmaild-request-move-article
    (article group server accept-form &optional last move-is-internal)
  nil)

(deffoo nnmaild-request-accept-article (group &optional server last)
  nil)

(deffoo nnmaild-request-post (&optional server)
  (nnmail-do-request-post 'nnmaild-request-accept-article server))

(deffoo nnmaild-request-replace-article (article group buffer)
  nil)

(deffoo nnmaild-request-delete-group (group &optional force server)
  nil)

(deffoo nnmaild-request-rename-group (group new-name &optional server)
  (nnmaild-possibly-change-directory group server)
  nil)

(deffoo nnmaild-set-status (article name value &optional group server)
  (nnmaild-possibly-change-directory group server)
  (let ((file (nnmaild-article-to-file article)))
    (cond
     ((not (file-exists-p file))
      (nnheader-report 'nnmaild "File %s does not exist" file))
     (t
      (with-temp-file file
	    (nnheader-insert-file-contents file)
	    (nnmail-replace-status name value))
      t))))


;;; Internal functions.

(defun nnmaild-possibly-change-directory (group &optional server)
  (when (and server
	         (not (nnmaild-server-opened server)))
    (nnmaild-open-server server))
  (if (not group)
      t
    (let ((pathname (nnmaild-group-pathname group nil server))
	      (file-name-coding-system nnmail-pathname-coding-system))
      (when (not (equal pathname nnmaild-current-directory))
	    (setq nnmaild-current-directory pathname
	          nnmaild-current-group group
	          nnmaild-article-file-alist nil))
      (file-exists-p nnmaild-current-directory))))

(defsubst nnmaild--article-to-prefix (article)
  (gethash article (nnmaild--hash) nil))

(defun nnmaild--load-data (dir)
  (let ((data-file (expand-file-name nnmaild-data-file dir))
        (nnmaild--data nil))
    (if (file-exists-p data-file)
        (load data-file nil t t)
      (nnheader-report 'nnmaild "Cache file %s does not exist" data-file))
    nnmaild--data))

(defun nnmaild--save-data (dir data)
  (let ((data-file (expand-file-name nnmaild-data-file dir)))
    (with-temp-file data-file
      (insert ";; Gnus data file\n")
      (insert "(setq nnmaild--data '")
      (gnus-prin1 nnmaild--data)
      (insert ")\n")))
  nnmaild--data)

(defconst nnmaild-flag-mark-mapping
  '((?F . tick)
    (?P . forward)
    (?R . reply)
    (?S . read))
  "Alist mapping Maildir filename flags to Gnus marks.
Maildir filenames are of the form \"unique-id:2,FLAGS\",
where FLAGS are a string of characters in ASCII order.
Some of the FLAGS correspond to Gnus marks.")

(defsubst nnmaild--mark-to-flag (mark)
  "Find the Maildir flag that corresponds to MARK (an atom).
Return a character, or nil if not found.
See `nnmaildir-flag-mark-mapping'."
  (car (rassq mark nnmaild-flag-mark-mapping)))

(defsubst nnmaild--flag-to-mark (flag)
  "Find the Gnus mark that corresponds to FLAG (a character).
Return an atom, or nil if not found.
See `nnmaildir-flag-mark-mapping'."
  (cdr (assq flag nnmaild-flag-mark-mapping)))

(defun nnmaild--suffix-to-marks (suffix)
  (when (and (length suffix)
             (stringp suffix)
             (string-match "2,\\([A-Z]*\\)$" suffix))
    (let ((flags (match-string 1 suffix)))
      (cl-map 'list 'nnmaild--flag-to-mark flags))))

(defun nnmaild--marks-to-suffix (marks)
  (concat nnmaild-flag-separator "2,"
          (cl-map 'string 'nnmaild--mark-to-flag marks)))

(defun nnmaild--act-on-suffix (suffix action marks)
  (let* ((old-marks (nnmaild--suffix-to-marks suffix))
         (new-marks (cl-case action
                      (del (cl-set-difference old-marks marks))
                      (add (cl-union old-marks marks))
                      (set marks))))
    (print new-marks)
    (if (equal old-marks new-marks)
        suffix
      (nnmaild--marks-to-suffix new-marks))))

(defun nnmaild--commit-new-suffix (prefix new-suffix art)
  (let* ((suffix (nnmaild--art-suffix art)))
    (unless (string-equal suffix new-suffix)
      (let* ((path (nnmaild--data-path nnmaild--data))
             (old-file-name (expand-file-name (concat "cur/" prefix suffix) path))
             (new-file-name (expand-file-name (concat "cur/" prefix new-suffix) path)))
        (rename-file old-file-name new-file-name 'replace)
        (setf (nnmaild--art-suffix art) new-suffix)))))

(defun nnmaild--data-collect-marks (data)
  (let (read flagged replied forward)
    (maphash (lambda (key art)
               (when (stringp key)
                 (let* ((suffix (nnmaild--art-suffix art))
                        (marks (nnmaild--suffix-to-marks suffix))
                        (article (nnmaild--art-number art)))
                   (when (memq 'read marks)
                     (push article read))
                   (when (memq 'reply marks)
                     (push article replied))
                   (when (memq 'forward marks)
                     (push article forward))
                   (when (memq 'tick marks)
                     (push article flagged)))))
             (nnmaild--data-hash data))
    (when (or read flagged replied)
      `(,(gnus-compress-sequence (sort read '<))
        ,@(when flagged
            `((tick ,@(gnus-compress-sequence (sort flagged '<)))))
        ,@(when forward
            `((tick ,@(gnus-compress-sequence (sort forward '<)))))
        ,@(when replied
            `((reply ,@(gnus-compress-sequence (sort replied '<)))))))))

(defun nnmaild--load-and-update-info (group info server)
  (unless server
    (error "nnmaild-request-update-info got null server"))
  (let* ((group-dir (nnmaild-group-pathname group nil server))
         (nnmaild--data (nnmaild--scan-group-dir group-dir)))
    (when info
      (let ((marks (nnmaild--data-collect-marks nnmaild--data)))
        (nnmaild--update-info marks info)
        (nnmaild--store-info info (gnus-active (gnus-info-group info)))))
    nnmaild--data))

(defun nnmaild--update-info (marks info)
  (setf (gnus-info-read info) (car marks)
        (gnus-info-marks info) (cdr marks))
  info)

(defun nnmaild--store-info (info group)
  ;;; FIXME! Save this info for caching
  nil)

(defun nnmaild--load-article-nov (path number)
  (let ((nov-buffer (get-buffer-create " *nov*"))
        nov chars headers)
    (with-current-buffer nov-buffer
      (buffer-disable-undo)
      (erase-buffer)
      (nnheader-insert-file-contents path)
      (narrow-to-region
	   (goto-char (point-min))
	   (progn
	     (re-search-forward "\n\r?\n" nil t)
	     (setq chars (- (point-max) (point)))
	     (max (point-min) (1- (point)))))
      (unless (zerop (buffer-size))
	    (goto-char (point-min))
	    (setq headers (nnml-parse-head chars number)))
      (widen)
      (erase-buffer)
      (when headers
        (nnheader-insert-nov headers)
        (buffer-string)))))

(defun nnmaild--update-article-data (path prefix suffix old-record)
  (let (article-number)
    (if old-record
        (setf (nnmaild--art-suffix old-record) suffix
              article-number (nnmaild--art-number old-record))
      (setf article-number (nnmaild--allocate)
            old-record (nnmaild--make-art article-number suffix
                                          (nnmaild--load-article-nov path (nnmaild--max)))))
    (puthash prefix old-record (nnmaild--hash))
    (puthash article-number prefix (nnmaild--hash))))

(defun nnmaild--scan-group-dir (group-dir &optional fast)
  (let* ((files (nnheader-directory-files (expand-file-name "cur" group-dir)
                                          t "^[^.].+$" t))
         (old-data (nnmaild--load-data group-dir)))
    (if (and old-data fast)
        old-data ;; Do not scan files, just retrieve last database
      (let ((nnmaild--data
             (if old-data
                 (nnmaild--copy-data old-data)
                 (nnmaild--make-empty-data (expand-file-name group-dir) (length files))))
            (old-hash (and old-data (nnmaild--data-hash old-data)))
            (regex (format "\\`\\([^%s]*\\)\\(\\(%s.*\\)?\\)\\'"
                           nnmaild-flag-separator nnmaild-flag-separator)))
        (dolist (path files)
          (let ((f (file-name-nondirectory path)))
            (when (string-match regex f)
              (let* ((prefix (match-string 1 f))
                     (suffix (match-string 2 f))
                     (old-record (and old-hash (gethash prefix old-hash nil))))
                (nnmaild--update-article-data path prefix suffix old-record)))))
        (nnmaild--save-data group-dir nnmaild--data)))))

(defun nnmaild--article-prefix (article)
  (gethash article (nnmaild--hash)))

(defun nnmaild--article-nov (article)
  (let ((prefix (gethash article (nnmaild--hash))))
    (when prefix
      (nnmaild--art-nov (gethash prefix (nnmaild--hash))))))

(defun nnmaild--data-find-id (id)
  "Search nnmaild--hash for the message id and return the article number"
  (catch 'return
    (let ((regex (regexp-quote (concat "\t" id "\t"))))
      (maphash (lambda (prefix-or-article art)
                 (when (stringp prefix-or-article)
                   (let ((nov (nnmaild--art-nov art)))
                     (when (string-match regex nov)
                       (throw 'return (nnmaild--art-number art))))))
               (nnmaild--hash))
      nil)))

(defun nnmaild--data-article-to-file (number)
  (when-let ((hash (nnmaild--hash))
             (prefix (gethash number hash nil))
             (record (gethash prefix hash nil)))
    (expand-file-name (concat "cur/" prefix (nnmaild--art-suffix record))
                      (nnmaild--data-path nnmaild--data))))

(provide 'nnmaild)

;;; nnmaild.el ends here