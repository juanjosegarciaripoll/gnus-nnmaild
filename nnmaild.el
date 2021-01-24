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

(defvoo nnmaild-flag-separators (if (eq system-type 'windows-nt) ";!" ":")
  "Separator for maildir file names, to split the flags.")

(defvoo nnmaild-data-file ".nnmaild-data.el"
  "File where message ids are saves")

(defvoo nnmaild-get-new-mail t
  "If non-nil, nnmaild will check the incoming mail file and split the mail.")

(defvoo nnmaild-cache nil
  "Association list between group directory names and nnmaild--data
structures, where we cache information about the messages. Only active
when nnmaild-cache-strategy is not NIL.")

(defvoo nnmaild-recurse t
  "If T, look for Maildir directories recursively into all subfolders of
nnmaild-directory, not just on the folders immediately inside it. NIL means
that only the toplevel folders are treated as groups.")

(defvoo nnmaild-cache-strategy 'memory+file
  "Symbol determining how we cache information about the messages in Maildir.

NIL means that we do not do any caching at all and every
request implies loading the list of files and scanning the
individual messages. This is risky and not recommended because
the association between file names and article numbers is not
preserved.

FILE means that the cache is read from and saved to file with the
name given by the variable nnmaild-data-file.

MEMORY implies that the cache is preserved in memory for as long
as the server remains open.

MEMORY+FILE implies both FILE and MEMORY.")

(defvoo nnmaild-cache-expiration-strategy 'directory-mtime
  "Recipe to determine whether a previous cache of message
information has expired and needs to be recreated.

T means that we always consider that the cache has expired
and refill it will a fresh new list of files, preseving the
association between file names and message numbers.

DIRECTORY-MTIME looks at the directory's modification time as a
clue of whether messages have been created, deleted or
renamed. Only then it tries to rebuild the cache.
s
FILE-MTIME looks at a file with a name given by
nnmaild-cache-control-file. If the modification time of that file
is more recent than the cache's time stamp, the latter is
rebuilt.")

(defvoo nnmaild-cache-control-file ".mbsyncstate"
  "If nnmaild-cache-expiration-strategy is FILE-MTIME, this is
the name of the file whose modification time is used to check
whether the cache has expired. The file name can be relative to
the Maildir folder directory, or it can be an absolute file
name.")


(defconst nnmaild-version "nnmaild 0.4"
  "nnmaild version.")

(defvoo nnmaild-server-alist nil
  "Association list of server names to groups")
(defvoo nnmaild--move-file nil)
(defvoo nnmaild-files-to-delete nil)
(defvoo nnmaild-status-string "")
(defvoo nnmaild-group-alist nil)
(defvoo nnmaild-active-timestamp nil)
(defvoo nnmaild-file-coding-system nnmail-file-coding-system)
(defvoo nnmaild-delivery-count 0)

(defvoo nnmaild--data nil
  "Variable to store data loaded from files.")


;;; Data structures
(cl-defstruct (nnmaild--data
               (:constructor nnmaild--make-data (min max hash path mtime)))
  min max hash path mtime)

(defsubst nnmaild--allocate ()
  (cl-incf (nnmaild--data-max nnmaild--data)))

(defsubst nnmaild--copy-data (data)
  (nnmaild--make-data
   (nnmaild--data-min data)
   (nnmaild--data-max data)
   (gnus-make-hashtable (hash-table-size (nnmaild--data-hash data)))
   (nnmaild--data-path data)
   (nnmaild--data-mtime data)))

(defsubst nnmaild--make-empty-data (path &optional l)
  (nnmaild--make-data
   1 0 (gnus-make-hashtable (* 2 (or l 10))) path
   (current-time)))

(defsubst nnmaild--data-size (data)
  (/ (hash-table-count (nnmaild--data-hash data)) 2))

(defsubst nnmaild--min ()
  (nnmaild--data-min nnmaild--data))

(defsubst nnmaild--max ()
  (nnmaild--data-max nnmaild--data))

(defsubst nnmaild--hash ()
  (nnmaild--data-hash nnmaild--data))

(defsubst nnmaild--size ()
  (nnmaild--data-size nnmaild--data))

(cl-defstruct (nnmaild--art
               (:constructor nnmaild--make-art (number suffix nov))
               (:type list))
  number suffix nov)

(defsubst nnmaild--prefix-to-art (prefix)
  (gethash prefix (nnmaild--hash) nil))


;;; Interface functions.

(nnoo-define-basics nnmaild)

(defun nnmaild-current-server ()
  (nnoo-current-server 'nnmaild))

(defun nnmaild-group-pathname (group &optional server file ignore-error)
  "Return an absolute file name of FILE for GROUP on SERVER."
  (let* ((server (or server (nnmaild-current-server)))
         (record (assoc server nnmaild-server-alist))
         (reporter (if ignore-error 'nnheader-report 'nnmaild--report-error)))
    (cond ((null record)
           (funcall reporter 'nnmaild "Server %s has not yet been opened" server)
           nil)
          ((null (setq record (assoc group (cdr record))))
           (funcall reporter 'nnmaild "Group %s does not exist in server %s" group server)
           nil)
          (file
           (expand-file-name file (cdr record)))
          (t
           (cdr record)))))

(defun nnmaild--report-error (backed &rest args)
  (apply 'error args))

(deffoo nnmaild-retrieve-headers (sequence &optional group server fetch-old)
  "Return a list of headers in NOV format for the given group"
  (let ((group-dir (nnmaild-group-pathname group server)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
	  (if (stringp (car sequence))
	      'headers
        (let* ((nnmaild--data (nnmaild--scan-group-dir group-dir))
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
  ;; This registers the server with the current backend and sets
  ;; is as the (nnoo-current-server)
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
    (push (cons server (nnmaild--create-list-of-groups nnmaild-directory
                                                       nnmaild-recurse))
          nnmaild-server-alist)
    (nnheader-report 'nnmaild "Opened server %s using directory %s"
		             server nnmaild-directory)
    t)))

(deffoo nnmaild-close-server (&optional server defs)
  (nnmaild--delete-queued-files)
  (let ((server (or server (nnmaild-current-server))))
    (nnmaild--mapc-groups (lambda (group-dir group server)
                            (nnmaild--clear-cache group-dir))
                          server)
    (setq nnmaild-server-alist
          (assoc-delete-all server nnmaild-server-alist))))

(deffoo nnmaild-request-close ()
  (mapc (lambda (pair) (nnmaild-close-server (car pair)))
        nnmaild-server-alist)
  t)

(deffoo nnmaild-request-regenerate (server)
  "Regenerate server information."
  (let ((record (assoc server nnmaild-server-alist)))
    (when record
      (rplacd record (nnmaild--create-list-of-groups nnmaild-directory
                                                   nnmaild-recurse)))))

(deffoo nnmaild-request-article (id &optional group server buffer)
  "Request the article denoted by ID from GROUP. ID can be an article number
or a string with a mail ID."
  (let* ((nntp-server-buffer (or buffer nntp-server-buffer))
	     (file-name-coding-system nnmail-pathname-coding-system)
         (server (or server (nnmaild-current-server)))
         (group-dir (nnmaild-group-pathname group server))
         (nnmaild--data (nnmaild--scan-group-dir group-dir))
         (article (if (numberp id)
                      id
	                (or (nnmaild--data-find-id id)
                        (catch 'return
                          (nnmaild--mapc-groups
                           (lambda (group-dir other-group server)
                             (setq nnmaild--data (nnmaild--scan-group-dir group-dir))
                             (when-let ((article (nnmaild--data-find-id id)))
                               (setq group other-group)
                               (throw 'return article)))
                           server)
                          nil))))
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
  (let* ((file-name-coding-system nnmail-pathname-coding-system)
         (group-dir (nnmaild-group-pathname group server nil 'ignore-error)))
    (cond
     ((null group-dir)
      ;; nnheader-report has already been invoked from nnmaild-group-pathname
      nil)
     (dont-check
      (nnheader-report 'nnmaild "Group %s selected" group)
      t)
     (t
      (let ((nnmaild--data (nnmaild--load-and-update-info group info server)))
	    (nnheader-report 'nnmaild "Selected group %s with %s messages"
                         group (nnmaild--size))
	    (nnheader-insert "211 %d %d %d %s\n"
						 (nnmaild--size) (nnmaild--min) (nnmaild--max)
						 group))))))

(deffoo nnmaild-request-group-scan (group &optional server info)
  (let ((nnmaild--data (nnmaild--load-and-update-info group info server)))
    (with-current-buffer nntp-server-buffer
	  (erase-buffer)
	  (insert
	   (format
	    "211 %d %d %d %S\n" (nnmaild--size) (nnmaild--min) (nnmaild--max)
	    group))
	  t)))

(deffoo nnmaild-request-scan (&optional group server)
  t)

(deffoo nnmaild-request-update-info (group info &optional server)
  (nnmaild--load-and-update-info group info server)
  t)

(deffoo nnmaild-close-group (group &optional server)
  t)

(deffoo nnmaild-request-create-group (group &optional server args)
  "Create an empty GROUP in SERVER."
  ;;; FIXME! We can implement this!
  nil)

(deffoo nnmaild-request-list (&optional server)
  (nnheader-insert "")
  (save-excursion
    (nnmaild--mapc-groups 'nnmaild-request-list-1 server)
    t))

(defun nnmaild-request-list-1 (group-dir group-name server)
  (let* ((data (nnmaild--scan-group-dir group-dir)))
    (with-current-buffer nntp-server-buffer
      (goto-char (point-max))
      (insert (format "%s %.0f %.0f y\n"
                      group-name
                      (nnmaild--data-max data)
                      (nnmaild--data-min data))))))

(deffoo nnmaild-request-newgroups (date &optional server)
  (nnmaild-request-list server))

(deffoo nnmaild-request-list-newsgroups (&optional server)
  nil)

(deffoo nnmaild-request-set-mark (group actions &optional server)
  (let* ((file-name-coding-system nnmail-pathname-coding-system)
         (group-dir (nnmaild-group-pathname group server))
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
  (let ((known-mark (cdr (assoc mark '((?R . (read))
                                       (?! . (tick)))))))
    (when known-mark
      (nnmaild-request-set-mark group `(((,article) add ,known-mark)))))
  mark)

(deffoo nnmaild-request-expire-articles (articles group &optional server force)
  nil)

(deffoo nnmaild-request-move-article
    (article group server accept-form &optional last move-is-internal)
  (let* ((file-name-coding-system nnmail-pathname-coding-system)
         (group-dir (nnmaild-group-pathname group server))
         (nnmaild--data (nnmaild--scan-group-dir group-dir))
         (nnmaild--move-file (nnmaild--data-article-to-file article)))
    (with-temp-buffer
      (when (file-writable-p nnmaild--move-file)
        (nnheader-insert-file-contents nnmaild--move-file)
        (when-let ((group-article-pair (eval accept-form)))
          ;; If the file was moved in nnmaild-request-accept-article
          ;; nnmaild--move-file will be reset to nil
          (when nnmaild--move-file
            (push nnmaild--move-file nnmaild-files-to-delete))
          (nnmaild--data-delete-article article)
          (when last
            (nnmaild--delete-queued-files))
          (setf (nnmaild--data-mtime nnmaild--data)
                (current-time))
          group-article-pair)))))

(defun nnmaild--delete-queued-files ()
  (let ((file-name-coding-system nnmail-pathname-coding-system))
    (mapc 'nnmail-delete-file-function nnmaild-files-to-delete)
    (setq nnmaild-files-to-delete nil)))

(deffoo nnmaild-request-accept-article (group &optional server last)
  (let* ((coding-system-for-write nnheader-file-coding-system)
         (group-dir (nnmaild-group-pathname group server))
         (nnmaild--data (nnmaild--scan-group-dir group-dir))
         (extension (and nnmaild--move-file (file-name-extension nnmaild--move-file)))
         (output-file (nnmaild--tmp-file-name extension))
         (cur-dir (expand-file-name "cur" group-dir))
         (cur-file (expand-file-name output-file cur-dir))
         (tmp-dir (expand-file-name "new" group-dir))
         (tmp-file (expand-file-name output-file tmp-dir)))
    (cond ((file-exists-p cur-file)
           (nnheader-report 'nnmaild "Destination file %s exists. Should never happen!"
                            cur-file)
           nil)
		  ;; If we are moving the message from another nnmaild folder,
		  ;; we try to use rename-file, preserving the extension and
		  ;; marks of the original message. We could reuse the file name
		  ;; but some Maildir synchronization software stores information
		  ;; in the name (e.g. UUID), which could cause conflicts.
          ((and nnmaild--move-file
				(condition-case nil
					  (progn
						(rename-file nnmaild--move-file cur-file nil)
						t)
					(error
					 (nnheader-report 'nnmaild "Unable to move %S to %S"
									  nnmaild--move-file cur-file)
					 nil)))
           (nnheader-report 'nnmaild "Message file %S moved to %S"
                            nnmaild--move-file cur-file)
           ;; File has been simply renamed. We do not need to delete it.
           (setq nnmaild--move-file nil)
           (cons group (nnmaild--data-insert-file cur-file)))
          ((file-exists-p tmp-file)
           (nnheader-report 'nnmaild "Temporary file %s exists. Should never happen!"
                            tmp-file)
           nil)
          (t
           (condition-case nil
               (and (write-region (point-min) (point-max) tmp-file nil
                                  'no-message nil 'excl)
                    (rename-file tmp-file cur-file nil)
                    (nnheader-report 'nnmaild "Stored message into file %S" cur-file)
                    (cons group (nnmaild--data-insert-file cur-file)))
             (error
              (delete-file tmp-file)
              nil))))))

(defun nnmaild--tmp-file-name (&optional extension)
  "Return a hopefully unique and valid name for a message file."
  (let ((time (current-time)))
    (format "%sR%x%sP%dQ%d%s2,%s"
            (format-time-string "#%s" time) ;; A monotonously increasing number
            (random) ;; Not cryptographically random, but good enough
            (format-time-string "M%6N" time)
            (emacs-pid)
            (cl-incf nnmaild-delivery-count)
            (nnmaild-flag-separator)
			(or extension ""))))

(deffoo nnmaild-request-post (&optional server)
  nil)

(deffoo nnmaild-request-replace-article (article group buffer)
  nil)

(deffoo nnmaild-request-delete-group (group &optional force server)
  nil)

(deffoo nnmaild-request-rename-group (group new-name &optional server)
  nil)

(deffoo nnmaild-set-status (article name value &optional group server)
  (let* ((group-dir (nnmaild-group-pathname group server))
         (nnmaild--data (nnmaild--scan-group-dir group-dir))
         (file (nnmaild--data-article-to-file article)))
    (cond
     ((not (file-exists-p file))
      (nnheader-report 'nnmaild "File %s does not exist" file))
     (t
      (with-temp-file file
	    (nnheader-insert-file-contents file)
	    (nnmail-replace-status name value))
      t))))


;;; Internal functions.

(defun nnmaild--create-list-of-groups (directory recurse &optional root)
  "Return an alist of (GROUP-NAME . GROUP-PATH) for all valid
Maildir directories under DIRECTORY. If RECURSE is NIL, only the
first level of folders is scanned; otherwise, all valid
directories are searched for a Maildir structure. ROOT is an
absolute path that is removed from GROUP-PATH to create GROUP-NAME."
  (let ((file-name-coding-system nnmail-pathname-coding-system)
        (root (or root directory))
        output)
	(dolist (group-dir (cl-remove-if-not 'nnmaild--valid-maildir-p
                                         (nnheader-directory-files
                                          (expand-file-name directory)
                                          t "^[^.].*" t)))
      (nnheader-report 'nnmaild "Registering group dir %s" group-dir)
	  (push (cons (nnmaild--make-group-name group-dir root)
                  group-dir)
			output)
	  (when recurse
	    (setq output (nconc (nnmaild--create-list-of-groups group-dir recurse root)
                            output))))
    (nreverse output)))

(defun nnmaild--valid-maildir-p (dir)
  (and (file-directory-p dir)
       (cl-every (lambda (subdir)
                   (let ((dir (expand-file-name subdir dir)))
                     (and (file-exists-p dir)
                          (file-directory-p dir))))
                 '("cur" "new" "tmp"))))

(defun nnmaild--make-group-name (directory root)
  (string-trim (file-relative-name directory root)
               "[/\\\\]" "[/\\\\]"))

(defun nnmaild--mapc-groups (fn server)
  "Call FN once for each group in SERVER. The function should
take three arguments: the absolute path to the group's Maildir
folder, the group name and the server name. The output of the
function is discarded. There is no guarantee over the order in
which groups are scanned."
  (let ((file-name-coding-system nnmail-pathname-coding-system)
        (record (assoc server nnmaild-server-alist)))
    (when record
      (dolist (group-dir-pair (cdr record) t)
        (funcall fn
                 (cdr group-dir-pair) ;; group directory
                 (car group-dir-pair) ;; group name
                 server)))))

(defsubst nnmaild--article-to-prefix (article)
  (gethash article (nnmaild--hash) nil))

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
  (concat (nnmaild-flag-separator) "2,"
          (cl-map 'string 'nnmaild--mark-to-flag marks)))

(defun nnmaild--act-on-suffix (suffix action marks)
  (let* ((old-marks (nnmaild--suffix-to-marks suffix))
         (new-marks (cl-case action
                      (del (cl-set-difference old-marks marks))
                      (add (cl-union old-marks marks))
                      (set marks))))
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
  (let* ((group-dir (nnmaild-group-pathname group server))
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
            old-record (nnmaild--make-art article-number suffix nil)))
    (puthash prefix old-record (nnmaild--hash))
    (puthash article-number prefix (nnmaild--hash))
    article-number))

(defun nnmaild--data-expired-p (data)
  "Return true if the cache has expired and must be rebuilt. See
nnmaild-cache-expiration-strategy for how this is determined"
  (if (eq nnmaild-cache-expiration-strategy t)
      t
    (let ((path (nnmaild--data-path data)))
      (or (nnmaild--load-new-messages path)
          (let ((mtime (nnmaild--data-mtime data)))
            (nnmaild--file-newer-than
             (expand-file-name
              (if (eq nnmaild-cache-expiration-strategy
                      'directory-mtime)
                  "cur"
                nnmaild-cache-control-file)
              path)
             mtime))))))

(defun nnmaild--load-new-messages (group-dir)
  "Load any messages that are in the ./new folder into the ./cur folder,
relative to group-dir. Return true if any file was moved."
  (let* ((orig (expand-file-name "new" group-dir))
         (files (nnheader-directory-files orig t "^[^.].+$" t)))
    (when files
      (let ((dest (expand-file-name "cur" group-dir)))
        (dolist (f files)
          (rename-file orig (expand-file-name (file-name-nondirectory orig)
                                              dest)
                       t)))
      t)))

(defun nnmaild--file-newer-than (file mtime)
  "Return true if FILE has been modified past the time indicated
by MTIME (see current-time for allowed values)."
  (or (null mtime)
      (when-let ((attr (file-attributes file)))
        (time-less-p mtime (file-attribute-modification-time attr)))))

(defun nnmaild--clear-cache (server-dir)
  "Remove all records in the cache that are rooted inside the
directory selected by SERVER-DIR. If SERVER-DIR is NIL, the whole
cache is flushed."
  (when nnmaild-cache
    (let ((prefix (if server-dir
                      (expand-file-name server-dir)
                    "")))
      (setq nnmaild-cache
            (cl-remove-if (lambda (record)
                            (when (string-prefix-p prefix (car record))
                              ;; If the file was in memory and the strategy
                              ;; is memory+file, flush the cache to file
                              (when (eq nnmaild-cache-strategy 'memory+file)
                                (nnmaild--save-data-file (cdr record)))
                              t))
                          nnmaild-cache)))))

(defun nnmaild--data-from-cache (group-dir)
  "Retrieve the nnmaild--data structure, either from memory or
from file if the nnmaild-cache-strategy allows it."
  (when nnmaild-cache-strategy
    (cond ((eq nnmaild-cache-strategy 'memory+file)
           (or (cdr (assoc group-dir nnmaild-cache))
               (let ((data (nnmaild--load-data-file group-dir)))
                 (when data
                   (nnmaild--data-to-cache data)))))
          ((eq nnmaild-cache-strategy 'memory)
           (cdr (assoc group-dir nnmaild-cache)))
          ((eq nnmaild-cache-strategy 'file)
           (nnmaild--load-data-file group-dir))
          )))

(defun nnmaild--data-to-cache (data)
  "Save the nnmaild--data structure, either to memory or to file if the
nnmaild-cache-strategy allows it."
  (when nnmaild-cache-strategy
    (let ((group-dir (nnmaild--data-path data)))
      (if (or (eq nnmaild-cache-strategy 'memory+file)
              (eq nnmaild-cache-strategy 'memory))
          (let ((record (assoc group-dir nnmaild-cache)))
            (if record
                (rplacd record data)
              (push (cons group-dir data) nnmaild-cache)))
        (nnmaild--save-data-file data))))
  data)

(defun nnmaild--load-data-file (group-dir)
  "Load the nnmaild--data information from a file with name given
by nnmaild-data-file within the group directory. If the file does
not exist, or the data is corrupt, return false. Otherwise,
return an nnmaild--data structure."
  (let* ((nnmaild--data nil)
         (data-file (expand-file-name nnmaild-data-file group-dir))
         (attr (file-attributes data-file)))
    (if (null attr)
        (nnheader-report 'nnmaild "Data file %s does not exist" data-file)
      (load data-file nil t t)
      (setf (nnmaild--data-path nnmaild--data) group-dir
            (nnmaild--data-mtime nnmaild--data)
            (file-attribute-modification-time attr)))
    nnmaild--data))

(defun nnmaild--save-data-file (data)
  "Export our nnmaild--data information to a file with name given
by nnmaild-data-file within the group directory."
  (let* ((path (nnmaild--data-path data))
         (data-file (expand-file-name nnmaild-data-file path)))
    (with-temp-file data-file
      (insert ";; Gnus data file\n")
      (insert "(setq nnmaild--data '")
      (gnus-prin1 data)
      (insert ")\n")))
  data)

(defsubst nnmaild-flag-separator ()
  (substring nnmaild-flag-separators 0 1))

(defsubst nnmaild--split-prefix-regex ()
  (format "\\`\\([^%s]*\\)\\(\\([%s].*\\)?\\)\\'"
          nnmaild-flag-separators nnmaild-flag-separators))

(defun nnmaild--data-update (old-data group-dir)
  "Create a new nnmaild--data structure, updating the information from OLD-DATA
with information gathered from the directory GROUP-DIR. This includes removing
messages that have been deleted, updating the suffix of messages whose status
has changed, and preemptively loading NOV structures, if absent."
  ;; Load directory, if placed on a remote folder (e.g. tramp, ftp)
  (nnheader-re-read-dir group-dir)
  (let* ((files (nnheader-directory-files (expand-file-name "cur" group-dir)
                                          t "^[^.].+$" t))
         (nnmaild--data
          (if old-data
              (nnmaild--copy-data old-data)
            (nnmaild--make-empty-data group-dir (length files))))
         (old-hash (and old-data (nnmaild--data-hash old-data)))
         (regex (nnmaild--split-prefix-regex)))
    (dolist (path files)
      (nnmaild--data-insert-file path regex old-hash))
    nnmaild--data))

(defun nnmaild--data-insert-file (path &optional regex old-hash)
  (let ((f (file-name-nondirectory path)))
    (when (string-match (or regex (nnmaild--split-prefix-regex)) f)
      (let* ((prefix (match-string 1 f))
             (suffix (match-string 2 f))
             (old-record (and old-hash (gethash prefix old-hash nil))))
        (nnmaild--update-article-data path prefix suffix old-record)))))

(defun nnmaild--scan-group-dir (group-dir)
  "Return the nnmaild--data structure for the given group, either from the
cache (see nnmaild-cache-strategy and nnmaild-cache-expiration-strategy),
or by recreating it from scratch."
  (let* ((group-dir (string-trim (expand-file-name group-dir) nil "[/\\]"))
         (nnmaild--data (nnmaild--data-from-cache group-dir)))
    (if (or (null nnmaild--data)
            (nnmaild--data-expired-p nnmaild--data))
        (nnmaild--data-to-cache
         (nnmaild--data-update nnmaild--data group-dir))
      nnmaild--data)))

(defun nnmaild--article-prefix (article)
  (gethash article (nnmaild--hash)))

(defun nnmaild--article-nov (article)
  (when-let ((prefix (gethash article (nnmaild--hash)))
             (art (gethash prefix (nnmaild--hash))))
    (or (nnmaild--art-nov art)
        (setf (nnmaild--art-nov art)
              (nnmaild--load-article-nov
               (expand-file-name (concat prefix (nnmaild--art-suffix art))
                                 (expand-file-name "cur"
                                                   (nnmaild--data-path nnmaild--data)))
               article)))))

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

(defun nnmaild--data-delete-article (number)
  (let* ((hash (nnmaild--hash))
         (prefix (gethash number hash nil)))
    (when prefix
      (remhash number hash)
      (remhash prefix hash))))

(gnus-declare-backend "nnmaild" 'mail 'address)

(provide 'nnmaild)

;;; nnmaild.el ends here
