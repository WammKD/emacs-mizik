(require 'cl)
(require 'libmpdee)

;; commands to run upon startup
(defvar mizik-var~current-connection (mpd-conn-new "localhost" 6600))

(defconst mizik-var~load-path "~/.emacs.d/")
(defconst mizik-var~var-dir "mizik-vars/")
(defconst mizik-var~mpd-playlist-dir "~/.mpd/playlists/")
(defconst mizik-var~mpd-music-dir "/media/drive/jaft/music/")

(defun mizik-util~pause-before-shutdown ()
  (write-region
    (format "%S" mizik-var~sort-token)
    nil
    (concat mizik-var~load-path mizik-var~var-dir "sort-token"))
  (write-region
    (format "%S" mizik-var~current-playlist)
    nil
    (concat mizik-var~load-path mizik-var~var-dir "current-playlist"))
  (+ 1 2))

(add-hook 'kill-emacs-query-functions 'mizik-util~pause-before-shutdown)

(if (not (file-exists-p (concat mizik-var~load-path mizik-var~var-dir)))
    (make-directory (concat mizik-var~load-path mizik-var~var-dir)))

(defun get-hash-keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys
  )
)

(defalias '^ 'expt)

(defalias '++ '1+)

(defalias '-- '1-)

(defalias '&& 'and)

(defun !! (received-atom index)
  (cond
   ((listp   received-atom) (if (< index 0)
				(nth (+ (length received-atom) index) received-atom)
			      (nth index received-atom)))
   ((stringp received-atom) (string-to-char (substring
					      received-atom
					      index
					      (++ index))))
   ((vectorp received-atom) (car (append
				   (substring
				     received-atom
				     index
				     (++ index))
				   nil)))
   (t received-atom)))

(defun : (received-atom startIndex &optional endIndex)
  (let ((sI (if (< startIndex 0)
		(+ (length received-atom) startIndex)
	      startIndex))
	(eI (if (eq endIndex nil)
		(length received-atom)
	      (if (< endIndex 0)
		  (if (> (+ (length received-atom) endIndex) (length received-atom))
		      (length received-atom)
		    (+ (length received-atom) endIndex))
		(if (> endIndex (length received-atom))
		    (length received-atom)
		  endIndex)))))
    (cond
     ((listp received-atom) (flet ((newList (a n m l k)
					    (cond
					     ((equal a        '()) '())
					     ((<     k          n) (newList (cdr a) n m l (++ k)))
					     ((=     (length a) l) a)
					     (t                    (newList (reverse (cdr (reverse a))) n m l k)))))
			      (newList received-atom sI eI (- eI sI) 0)))
     ((vectorp received-atom) (vconcat (: (append received-atom nil) startIndex endIndex)))
     ((stringp received-atom) (if (> sI eI)
				  ""
				(substring received-atom sI (if (> eI (length received-atom))
								(length received-atom)
							      eI)))))))

(defun neq (x y)
  (not (eq x y)))

(defun neql (x y)
  (not (eql x y)))

(defun nequal (x y)
  (not (equal x y)))

(defun pairp (a)
  (&&
    (listp a)
    (not (listp (cdr a)))
    (neq (cdr a) nil)))

(defun rev (a)
  (cond
   ((stringp a) (mapconcat
		  'char-to-string
		  (reverse (string-to-list a))
		  ""))
   ((pairp   a) (cons (cdr a) (car a)))
   ((listp   a) (reverse a))
   ((vectorp a) (vconcat (reverse (append a nil))))))

(defun lst (&rest xs)
  (apply 'append (mapcar (lambda (x)
			   (if (listp x)
			       (if (listp (cdr x))
				   x
				 (list x))
			     (list x))) xs)))

(defun head (a)
  (!! a 0))

(defun cpr (l)
  (cond
   ((pairp l) (car l))
   ((listp l) (rev (cdr (rev l))))))

(defun ctr (l)
  (car (rev l)))

(defun init (a)
  (cond
   ((stringp a) (substring a 0 -1))
   ((listp   a) (cpr a))
   ((vectorp a) (vconcat (cpr (append a nil))))))

(defun tail (a)
  (cond
   ((stringp a) (substring a 1))
   ((listp   a) (cdr a))
   ((vectorp a) (vconcat (cdr (append a nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All buttons
(defun mizik-util~face-height (face-name)
  "Return the height of the font used for FACE-NAME, or nil.
If running without a window system, signal an error."
  (catch 'return
    (aref
      (or
        (font-info (or
		     (face-font face-name)
		     (throw 'return nil)))
	(throw 'return nil))
      3)))

(defun mizik-util~mode-line-icon-size ()
  "Return the size to use for mode line icons."
  11
  ;; (if (>= 
  ;;       (or
  ;; 	  (mizik-util~face-height 'mode-line)
  ;; 	  0)
  ;; 	18)
  ;;     18
  ;;   11)
  )

(defconst mizik-icon~pause-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *pause_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 18     18      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"..................\",
\"..................\",
\"..................\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"...####....####...\",
\"..................\",
\"..................\",
\"..................\"
};"))       
  "Mizik [Pause] button icon (18 pixels tall).")     
    
(defconst mizik-icon~pause-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *pause_11[] = {  
/* width  height  number of colors  number of characters per pixel */
\" 10     11      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"..........\",
\"..........\",
\"..##..##..\",
\"..##..##..\",
\"..##..##..\",
\"..##..##..\",
\"..##..##..\",
\"..##..##..\",
\"..##..##..\",
\"..........\",
\"..........\"};"))
  "Mizik [Pause] button icon (11 pixels tall).")

(defvar mizik-keymap~play/pause
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
	(lambda (e)
	  (interactive "e")
	  (mizik-func~pause/resume))))))

(defun mizik-util~pause-button ()
  "Return the string to use as [Pause] button."
  (let ((icon-size (mizik-util~mode-line-icon-size)))
    (concat
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1))))
      (propertize " "
		  'display
  		    (cond ((= icon-size 18)
			   (eval mizik-icon~pause-18))
			  ((= icon-size 11)
			   (eval mizik-icon~pause-11)))
		    'help-echo
		      "mouse-1: pause current song in current playlist"
		    'local-map
		      mizik-keymap~play/pause
		    'mouse-face
		      'highlight)
      (when (>= emacs-major-version 22)
         (propertize " " 'display '(space :width (1)))))))

(defconst mizik-icon~play-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *resume_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 18     18      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"..................\",
\"..................\",
\"......##..........\",
\"......###.........\",
\"......####........\",
\"......#####.......\",
\"......######......\",
\"......#######.....\",
\"......########....\",
\"......########....\",
\"......#######.....\",
\"......######......\",
\"......#####.......\",
\"......####........\",
\"......###.........\",
\"......##..........\",
\"..................\",
\"..................\"
};"))
  "Mizik [Play] button icon (18 pixels tall).")

(defconst mizik-icon~play-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *resume_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 10     11      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"..........\",
\"...#......\",
\"...##.....\",
\"...###....\",
\"...####...\",
\"...#####..\",
\"...####...\",
\"...###....\",
\"...##.....\",
\"...#......\",
\"..........\"
};"))
  "Mizik [Play] button icon (11 pixels tall).")

(defun mizik-util~play-button ()
  "Return the string to use as [Play] button."
  (let ((icon-size (mizik-util~mode-line-icon-size)))
    (concat
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1))))
      (propertize " "
		  'display
  		    (cond ((= icon-size 18)
			   (eval mizik-icon~play-18))
			  ((= icon-size 11)
			   (eval mizik-icon~play-11)))
		  'help-echo
		    "mouse-1: play current song in current playlist"
		  'local-map
		    mizik-keymap~play/pause
		  'mouse-face
		    'highlight)
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1)))))))

(defconst mizik-icon~previous-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *previous_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....###.......##....\",
\"....###......###....\",
\"....###.....####....\",
\"....###....#####....\",
\"....###...######....\",
\"....###...######....\",
\"....###....#####....\",
\"....###.....####....\",
\"....###......###....\",
\"....###.......##....\",
\"....................\",
\"....................\",
\"....................\",
\"....................\"
};"))
  "Mizik [Previous] button icon (18 pixels tall)")

(defconst mizik-icon~previous-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *previous_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\"..##....#..\",
\"..##...##..\",
\"..##..###..\",
\"..##.####..\",
\"..##..###..\",
\"..##...##..\",
\"..##....#..\",
\"...........\",
\"...........\"
};"))
  "Mizik [Previous] button icon (11 pixels tall).")

(defvar mizik-keymap~previous
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
	(lambda (e)
	  (interactive "e")
	  (mizik-func~previous))))))

(defun mizik-util~previous-button ()
  "Return the string to use as [Previous] button."
  (let ((icon-size (mizik-util~mode-line-icon-size)))
    (concat
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1))))
      (propertize " "
		  'display
  		    (cond ((= icon-size 18)
			   (eval mizik-icon~previous-18))
			  ((= icon-size 11)
			   (eval mizik-icon~previous-11)))
		  'help-echo
		    "mouse-1: skip to the previous track in current playlist"
		  'local-map
		    mizik-keymap~previous
		  'mouse-face
		    'highlight)
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1)))))))

(defconst mizik-icon~next-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *next_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....##.......###....\",
\"....###......###....\",
\"....####.....###....\",
\"....#####....###....\",
\"....######...###....\",
\"....######...###....\",
\"....#####....###....\",
\"....####.....###....\",
\"....###......###....\",
\"....##.......###....\",
\"....................\",
\"....................\",
\"....................\",
\"....................\"
};"))
  "Mizik [Next] button icon (18 pixels tall)")

(defconst mizik-icon~next-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *next_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\"..#....##..\",
\"..##...##..\",
\"..###..##..\",
\"..####.##..\",
\"..###..##..\",
\"..##...##..\",
\"..#....##..\",
\"...........\",
\"...........\"
};"))
  "Mizik [Next] button icon (11 pixels tall).")

(defvar mizik-keymap~next
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
	(lambda (e)
	  (interactive "e")
	  (mizik-func~next))))))

(defun mizik-util~next-button ()
  "Return the string to use as [Next] button."
  (let ((icon-size (mizik-util~mode-line-icon-size)))
    (concat
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1))))
      (propertize " "
		  'display
  		    (cond ((= icon-size 18)
			   (eval mizik-icon~next-18))
			  ((= icon-size 11)
			   (eval mizik-icon~next-11)))
		  'help-echo
		    "mouse-1: skip to the next track in current playlist"
		  'local-map
		    mizik-keymap~next
		  'mouse-face
		    'highlight)
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1)))))))

(defconst mizik-icon~backward-seek-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *backward_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"......##......##....\",
\".....###.....###....\",
\"....####....####....\",
\"...#####...#####....\",
\"...#####...#####....\",
\"....####....####....\",
\".....###.....###....\",
\"......##......##....\",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....................\"
};"))
  "Mizik [Rewind] button icon (18 pixels tall).")

(defconst mizik-icon~backward-seek-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *backward_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\"...........\",
\"....#...#..\",
\"...##..##..\",
\"..###.###..\",
\"...##..##..\",
\"....#...#..\",
\"...........\",
\"...........\",
\"...........\"
};"))
  "Mizik [Rewind] button icon (11 pixels tall).")

(defvar mizik-keymap~backward-seek
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
	(lambda (e)
	  (interactive "e")
	  (mpd-seek
	    mizik-var~current-connection
	    (mizik-util~get-current-song-ID)
	    (if (< (- (mizik-util~get-song-playback-position) 10) 0)
		0
	      (round (- (mizik-util~get-song-playback-position) 10)))
	    t))))))

(defun mizik-util~backward-seek-button ()
  "Return the string to use as [Rewind] button."
  (let ((icon-size (mizik-util~mode-line-icon-size)))
    (concat
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1))))
      (propertize " "
		  'display
  		    (cond ((= icon-size 18)
			   (eval mizik-icon~backward-seek-18))
			  ((= icon-size 11)
			   (eval mizik-icon~backward-seek-11)))
		  'help-echo
		    "mouse-1: rewind 10 seconds"
		  'local-map
		    mizik-keymap~backward-seek
		  'mouse-face
		    'highlight)
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1)))))))

(defconst mizik-icon~forward-seek-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *forward_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....##......##......\",
\"....###.....###.....\",
\"....####....####....\",
\"....#####...#####...\",
\"....#####...#####...\",
\"....####....####....\",
\"....###.....###.....\",
\"....##......##......\",
\"....................\",
\"....................\",
\"....................\",
\"....................\",
\"....................\"
};"))
  "Mizik [Fast-forward] button icon (18 pixels tall).")

(defconst mizik-icon~forward-seek-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *forward_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\"...........\",
\"..#...#....\",
\"..##..##...\",
\"..###.###..\",
\"..##..##...\",
\"..#...#....\",
\"...........\",
\"...........\",
\"...........\"
};"))
  "Mizik [Fast-forward] button icon (11 pixels tall).")

(defvar mizik-keymap~forward-seek
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mode-line mouse-1]
	(lambda (e)
	  (interactive "e")
	  (mpd-seek
	    mizik-var~current-connection
	    (mizik-util~get-current-song-ID)
	    (round (+ (mizik-util~get-song-playback-position) 10))
	    t))))))

(defun mizik-util~forward-seek-button ()
  "Return the string to use as [Fast-forward] button."
  (let ((icon-size (mizik-util~mode-line-icon-size)))
    (concat
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1))))
      (propertize " "
		  'display
  		    (cond ((= icon-size 18)
			   (eval mizik-icon~forward-seek-18))
			  ((= icon-size 11)
			   (eval mizik-icon~forward-seek-11)))
		  'help-echo
		    "mouse-1: fast-forward 10 seconds"
		  'local-map
		    mizik-keymap~forward-seek
		  'mouse-face
		    'highlight)
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1)))))))

(defconst mizik-icon~volume-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *volume_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"........#...........\",
\".......##....##.....\",
\"......###.....##....\",
\".....####...#..##...\",
\"..#######...##.##...\",
\"..#######.#..##.##..\",
\"..#######..#.##.##..\",
\"..#######..#.##.##..\",
\"..#######.#..##.##..\",
\"..#######...##.##...\",
\".....####...#..##...\",
\"......###.....##....\",
\".......##....##.....\",
\"........#...........\",
\"....................\",
\"....................\"
};"))
  "Mizik [Volume] button icon (18 pixels tall).")

(defconst mizik-icon~volume-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *volume_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\".....#.....\",
\"....##.#...\",
\"..####..#..\",
\"..#####.#..\",
\"..####..#..\",
\"....##.#...\",
\".....#.....\",
\"...........\",
\"...........\"
};"))
  "Mizik [Volume] button icon (11 pixels tall).")

(defun mizik-util~volume-button ()
  "Return the string to use as [Volume] button."
  (let ((icon-size (mizik-util~mode-line-icon-size)))
    (concat
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1))))
      (propertize " "
		  'display
  		    (cond ((= icon-size 18)
			   (eval mizik-icon~volume-18))
			  ((= icon-size 11)
			   (eval mizik-icon~volume-11)))
		  'help-echo
		    "mouse-1: mute the volume"
		  'local-map
		    mizik-keymap~forward-seek
		  'mouse-face
		    'highlight)
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1)))))))

(defconst mizik-icon~volume-mute-18
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *volume_18[] = {
/* width  height  number of colors  number of characters per pixel */
\" 20     18      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"....................\",
\"....................\",
\"........#...........\",
\".......##...........\",
\"......###...........\",
\".....####..#.....#..\",
\"..#######..##...##..\",
\"..#######...##.##...\",
\"..#######....###....\",
\"..#######....###....\",
\"..#######...##.##...\",
\"..#######..##...##..\",
\".....####..#.....#..\",
\"......###...........\",
\".......##...........\",
\"........#...........\",
\"....................\",
\"....................\"
};"))
  "Mizik [Volume-muted] button icon (18 pixels tall).")

(defconst mizik-icon~volume-mute-11
  '`(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *volume_11[] = {
/* width  height  number of colors  number of characters per pixel */
\" 11     11      2                 1\",
/* colors */
\"# c " (face-foreground 'mode-line nil 'default)  "\",
\". c None\",
/* pixels */
\"...........\",
\"...........\",
\".....#.....\",
\"....##.....\",
\"..#####.#..\",
\"..####.#...\",
\"..#####.#..\",
\"....##.....\",
\".....#.....\",
\"...........\",
\"...........\"
};"))
  "Mizik [Volume-muted] button icon (11 pixels tall).")

(defun mizik-util~volume-mute-button ()
  "Return the string to use as [Volume-muted] button."
  (let ((icon-size (mizik-util~mode-line-icon-size)))
    (concat
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1))))
      (propertize " "
		  'display
  		    (cond ((= icon-size 18)
			   (eval mizik-icon~volume-mute-18))
			  ((= icon-size 11)
			   (eval mizik-icon~volume-mute-11)))
		  'help-echo
		    "mouse-1: unmute the volume"
		  'local-map
		    mizik-keymap~forward-seek
		  'mouse-face
		    'highlight)
      (when (>= emacs-major-version 22)
	(propertize " " 'display '(space :width (1)))))))

(defvar mizik-var~mode-line-buttons~play
  (list (mizik-util~previous-button) (mizik-util~backward-seek-button) (mizik-util~play-button) (mizik-util~forward-seek-button) (mizik-util~next-button)))

(defvar mizik-var~mode-line-buttons~pause
  (list (mizik-util~previous-button) (mizik-util~backward-seek-button) (mizik-util~pause-button) (mizik-util~forward-seek-button) (mizik-util~next-button)))

(setq-default mode-line-format (lst (cpr mode-line-format) (list mizik-var~mode-line-buttons~play) (ctr mode-line-format)))

(defun mizik-util~change-play-mode-line-button-to-pause ()
  (setq-default mode-line-format
	(lst
	  (:
	    mode-line-format
	    0
	    (* (length (member
			 mizik-var~mode-line-buttons~play
			 mode-line-format)) -1))
	  (list mizik-var~mode-line-buttons~pause)
	  (cdr (member mizik-var~mode-line-buttons~play mode-line-format)))))

(defun mizik-util~change-pause-mode-line-button-to-play ()
  (setq-default mode-line-format
	(lst
	  (:
	    mode-line-format
	    0
	    (* (length (member
			 mizik-var~mode-line-buttons~pause
			 mode-line-format)) -1))
	  (list mizik-var~mode-line-buttons~play)
	  (cdr (member mizik-var~mode-line-buttons~pause mode-line-format)))))

;; All possible playlists
(defconst mizik-var~playlist-file-sorted "mizik~file-sorted"
  "*internal* Name of the playlist of all songs, sorted by file name.")

(defconst mizik-var~playlist-file-sorted~reversed "mizik~file-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by file name, reversed.")

(defconst mizik-var~playlist-added-sorted "mizik~added-sorted"
  "*internal* Name of the playlist of all songs, sorted by date-added.")

(defconst mizik-var~playlist-added-sorted~reversed "mizik~added-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by date-added, reversed.")

(defconst mizik-var~playlist-time-sorted "mizik~time-sorted"
  "*internal* Name of the playlist of all songs, sorted by song length.")

(defconst mizik-var~playlist-time-sorted~reversed "mizik~time-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by song length, reversed.")

(defconst mizik-var~playlist-artist-sorted "mizik~artist-sorted"
  "*internal* Name of the playlist of all songs, sorted by artist.")

(defconst mizik-var~playlist-artist-sorted~reversed "mizik~artist-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by artist, reversed.")

(defconst mizik-var~playlist-albumArtist-sorted "mizik~albumArtist-sorted"
  "*internal* Name of the playlist of all songs, sorted by the album-artist.")

(defconst mizik-var~playlist-albumArtist-sorted~reversed "mizik~albumArtist-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by the album-artist, reversed.")

(defconst mizik-var~playlist-title-sorted "mizik~title-sorted"
  "*internal* Name of the playlist of all songs, sorted by title.")

(defconst mizik-var~playlist-title-sorted~reversed "mizik~title-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by title, reversed.")

(defconst mizik-var~playlist-album-sorted "mizik~album-sorted"
  "*internal* Name of the playlist of all songs, sorted by album.")

(defconst mizik-var~playlist-album-sorted~reversed "mizik~album-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by album, reversed.")

(defconst mizik-var~playlist-track-sorted "mizik~track-sorted"
  "*internal* Name of the playlist of all songs, sorted by track number.")

(defconst mizik-var~playlist-track-sorted~reversed "mizik~track-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by track number, reversed.")

(defconst mizik-var~playlist-year-sorted "mizik~year-sorted"
  "*internal* Name of the playlist of all songs, sorted by year.")

(defconst mizik-var~playlist-year-sorted~reversed "mizik~year-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by year, reversed.")

(defconst mizik-var~playlist-genre-sorted "mizik~genre-sorted"
  "*internal* Name of the playlist of all songs, sorted by genre.")

(defconst mizik-var~playlist-genre-sorted~reversed "mizik~genre-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by genre, reversed.")

(defconst mizik-var~playlist-composer-sorted "mizik~composer-sorted"
  "*internal* Name of the playlist of all songs, sorted by composer.")

(defconst mizik-var~playlist-composer-sorted~reversed "mizik~composer-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by composer, reversed.")

(defconst mizik-var~playlist-disc-sorted "mizik~disc-sorted"
  "*internal* Name of the playlist of all songs, sorted by disc number.")

(defconst mizik-var~playlist-disc-sorted~reversed "mizik~disc-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by disc number, reversed.")

(defconst mizik-var~playlist-artistID-sorted "mizik~artistID-sorted"
  "*internal* Name of the playlist of all songs, sorted by Musicbrainz artist ID number.")

(defconst mizik-var~playlist-artistID-sorted~reversed "mizik~artistID-sorted~reversed"
  "*internal* Name of the playlist of all songs, sorted by Musicbrainz artist ID number, reversed.")

(defconst mizik-var~temp-playlist "mizik~temp-playlist"
  "*internal* Name of the session playlist.")
(mpd-simple-exec mizik-var~current-connection (concat "playlistclear " mizik-var~temp-playlist))

(defcustom mizik-var~field-info
  (list (cons (cons "Track  "         'Track)  7)
	(cons (cons "Artist  "       'Artist) -1) 
	(cons (cons "Title  "         'Title) -1)
	(cons (cons "Album  "         'Album) -1)
	(cons (cons "Time  "           'Time) 10)
	(cons (cons "Year  "           'Date)  6)
	(cons (cons "Added  " 'Last-Modified) 15))
  "A list of the header fields shown to organize and sort the music
library and their respective widths in characters. A width of -1
means 'unrestricted' and will evenly distribute the remaining space
amongst each of these fields. For the complete list of available
headers, see `mizik-header-info'."
  :type (list 'symbol)
  :group 'mizik-headers)

(defconst mizik-var~library-buffer-name " *Mizik Library*"
  "*internal* Name of the Mizik main view buffer.")

(defconst mizik-var~playlist-buffer-name " *Mizik Playlist*"
  "*internal* Name of the Mizik playlist buffer.")

(defvar mizik-var~library-buffer (get-buffer-create mizik-var~library-buffer-name)
  "*internal* The Mizik main view buffer.")
(kill-buffer mizik-var~library-buffer)

(defvar mizik-var~playlist-buffer (get-buffer-create mizik-var~playlist-buffer-name)
  "*internal* The Mizik playlist view buffer.")
(kill-buffer mizik-var~playlist-buffer)

(defvar mizik-var~last-view
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "last-view")))
    (if (not (file-exists-p file))
	'main
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~last-line-position~main 1)
(defvar mizik-var~last-line-position~playlist 1)

(defvar mizik-var~sort-token
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "sort-token")))
    (if (not (file-exists-p file))
	(cons 'Album 'Regular)
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-file
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-file")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-file
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-file")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-file~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-file~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-added
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-added")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-added
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-added")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-added~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-added~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-time
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-time")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-time
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-time")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-time~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-time~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-artist
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-artist")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-artist
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-artist")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-artist~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-artist~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-albumArtist
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-albumArtist")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-albumArtist
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-albumArtist")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-albumArtist~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-albumArtist~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-title
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-title")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-title
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-title")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-title~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-title~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-album
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-album")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-album
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-album")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-album~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-album~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-track
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-track")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-track
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-track")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-track~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-track~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-year
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-year")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-year
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-year")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-year~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-year~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-genre
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-genre")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-genre
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-genre")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-genre~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-genre~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-composer
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-composer")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-composer
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-composer")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-composer~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-composer~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-disc
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-disc")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-disc
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-disc")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-disc~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-disc~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~library-by-artistID
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "library-by-artistID")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-artistID
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-artistID")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
(defvar mizik-var~inserted-text-by-artistID~reversed
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-artistID~reversed")))
    (if (not (file-exists-p file))
	'()
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))

(defvar mizik-var~inserted-text-for-playlist
  '())

(defvar mizik-var~all-available-categories
  (list (cons 'file                 (cons
				     (cons mizik-var~inserted-text-by-file               mizik-var~inserted-text-by-file~reversed)
				     (cons mizik-var~playlist-file-sorted                 mizik-var~playlist-file-sorted~reversed)))
	(cons 'Last-Modified        (cons
				     (cons mizik-var~inserted-text-by-added             mizik-var~inserted-text-by-added~reversed)
				     (cons mizik-var~playlist-added-sorted               mizik-var~playlist-added-sorted~reversed)))
	(cons 'Time                 (cons
				     (cons mizik-var~inserted-text-by-time               mizik-var~inserted-text-by-time~reversed)
				     (cons mizik-var~playlist-time-sorted                 mizik-var~playlist-time-sorted~reversed)))
	(cons 'Artist               (cons
				     (cons mizik-var~inserted-text-by-artist           mizik-var~inserted-text-by-artist~reversed)
				     (cons mizik-var~playlist-artist-sorted             mizik-var~playlist-artist-sorted~reversed)))
	(cons 'AlbumArtist          (cons
				     (cons mizik-var~inserted-text-by-albumArtist mizik-var~inserted-text-by-albumArtist~reversed)
				     (cons mizik-var~playlist-albumArtist-sorted   mizik-var~playlist-albumArtist-sorted~reversed)))
	(cons 'Title                (cons
				     (cons mizik-var~inserted-text-by-title             mizik-var~inserted-text-by-title~reversed)
				     (cons mizik-var~playlist-title-sorted               mizik-var~playlist-title-sorted~reversed)))
	(cons 'Album                (cons
				     (cons mizik-var~inserted-text-by-album             mizik-var~inserted-text-by-album~reversed)
				     (cons mizik-var~playlist-album-sorted               mizik-var~playlist-album-sorted~reversed)))
	(cons 'Track                (cons
				     (cons mizik-var~inserted-text-by-track             mizik-var~inserted-text-by-track~reversed)
				     (cons mizik-var~playlist-track-sorted               mizik-var~playlist-track-sorted~reversed)))
	(cons 'Date                 (cons
				     (cons mizik-var~inserted-text-by-year               mizik-var~inserted-text-by-year~reversed)
				     (cons mizik-var~playlist-year-sorted                 mizik-var~playlist-year-sorted~reversed)))
	(cons 'Genre                (cons
				     (cons mizik-var~inserted-text-by-genre             mizik-var~inserted-text-by-genre~reversed)
				     (cons mizik-var~playlist-genre-sorted               mizik-var~playlist-genre-sorted~reversed)))
	(cons 'Composer             (cons
				     (cons mizik-var~inserted-text-by-composer       mizik-var~inserted-text-by-composer~reversed)
				     (cons mizik-var~playlist-composer-sorted         mizik-var~playlist-composer-sorted~reversed)))
	(cons 'Disc                 (cons
				     (cons mizik-var~inserted-text-by-disc               mizik-var~inserted-text-by-disc~reversed)
				     (cons mizik-var~playlist-disc-sorted                 mizik-var~playlist-disc-sorted~reversed)))
	(cons 'MUSICBRAINZ_ARTISTID (cons
				     (cons mizik-var~inserted-text-by-artistID       mizik-var~inserted-text-by-artistID~reversed)
				     (cons mizik-var~playlist-artistID-sorted         mizik-var~playlist-artistID-sorted~reversed)))))

(defun mizik-util~reset-aac ()
  (setq mizik-var~all-available-categories	
	(list (cons 'file                 (cons
					   (cons mizik-var~inserted-text-by-file               mizik-var~inserted-text-by-file~reversed)
					   (cons mizik-var~playlist-file-sorted                 mizik-var~playlist-file-sorted~reversed)))
	      (cons 'Last-Modified        (cons
					   (cons mizik-var~inserted-text-by-added             mizik-var~inserted-text-by-added~reversed)
					   (cons mizik-var~playlist-added-sorted               mizik-var~playlist-added-sorted~reversed)))
	      (cons 'Time                 (cons
					   (cons mizik-var~inserted-text-by-time               mizik-var~inserted-text-by-time~reversed)
					   (cons mizik-var~playlist-time-sorted                 mizik-var~playlist-time-sorted~reversed)))
	      (cons 'Artist               (cons
					   (cons mizik-var~inserted-text-by-artist           mizik-var~inserted-text-by-artist~reversed)
					   (cons mizik-var~playlist-artist-sorted             mizik-var~playlist-artist-sorted~reversed)))
	      (cons 'AlbumArtist          (cons
					   (cons mizik-var~inserted-text-by-albumArtist mizik-var~inserted-text-by-albumArtist~reversed)
					   (cons mizik-var~playlist-albumArtist-sorted   mizik-var~playlist-albumArtist-sorted~reversed)))
	      (cons 'Title                (cons
					   (cons mizik-var~inserted-text-by-title             mizik-var~inserted-text-by-title~reversed)
					   (cons mizik-var~playlist-title-sorted               mizik-var~playlist-title-sorted~reversed)))
	      (cons 'Album                (cons
					   (cons mizik-var~inserted-text-by-album             mizik-var~inserted-text-by-album~reversed)
					   (cons mizik-var~playlist-album-sorted               mizik-var~playlist-album-sorted~reversed)))
	      (cons 'Track                (cons
					   (cons mizik-var~inserted-text-by-track             mizik-var~inserted-text-by-track~reversed)
					   (cons mizik-var~playlist-track-sorted               mizik-var~playlist-track-sorted~reversed)))
	      (cons 'Date                 (cons
					   (cons mizik-var~inserted-text-by-year               mizik-var~inserted-text-by-year~reversed)
					   (cons mizik-var~playlist-year-sorted                 mizik-var~playlist-year-sorted~reversed)))
	      (cons 'Genre                (cons
					   (cons mizik-var~inserted-text-by-genre             mizik-var~inserted-text-by-genre~reversed)
					   (cons mizik-var~playlist-genre-sorted               mizik-var~playlist-genre-sorted~reversed)))
	      (cons 'Composer             (cons
					   (cons mizik-var~inserted-text-by-composer       mizik-var~inserted-text-by-composer~reversed)
					   (cons mizik-var~playlist-composer-sorted         mizik-var~playlist-composer-sorted~reversed)))
	      (cons 'Disc                 (cons
					   (cons mizik-var~inserted-text-by-disc               mizik-var~inserted-text-by-disc~reversed)
					   (cons mizik-var~playlist-disc-sorted                 mizik-var~playlist-disc-sorted~reversed)))
	      (cons 'MUSICBRAINZ_ARTISTID (cons
					   (cons mizik-var~inserted-text-by-artistID       mizik-var~inserted-text-by-artistID~reversed)
					   (cons mizik-var~playlist-artistID-sorted         mizik-var~playlist-artistID-sorted~reversed))))))

(defvar mizik-var~current-playlist
  (let ((file (concat mizik-var~load-path mizik-var~var-dir "current-playlist")))
    (if (not (file-exists-p file))
	(if (eq (cdr mizik-var~sort-token) 'Regular)
	    (car (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories)))
	  (cdr (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))
      (read
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string))))))
;; Make sure the proper playlist is loaded upon start up
(if (string= mizik-var~current-playlist mizik-var~temp-playlist)
    (progn
      (mpd-clear-playlist mizik-var~current-connection)
      (if (eq (cdr mizik-var~sort-token) 'Regular)
	  (progn
	    (mpd-load-playlist
	      mizik-var~current-connection
	      (car (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))
	    (setq mizik-var~current-playlist (car (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories)))))
	(progn
	  (mpd-load-playlist
	    mizik-var~current-connection
	    (cdr (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))
	  (setq mizik-var~current-playlist (cdr (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))))))
    
(defun mizik-util~get-youtube-stream (yt)
  (shell-command-to-string (concat "youtube-dl -f140 -g " yt)))

(defun mizik-func~add-song-to-playlist (choice)
  (interactive "sAre you adding a file (f), YouTube audio stream (yt), or other audio stream (o)? ")
  (let ((res ""))
    (cond
     ((equal (downcase choice)  "f") (setq res (expand-file-name (read-file-name "File: "))))
     ((equal (downcase choice) "yt") (setq res (mizik-util~get-youtube-stream (read-string "YouTube URI: "))))
     ((equal (downcase choice)  "o") (setq res (read-string "Audio stream: ")))
     (t (message "That was not one of the options given.")))
    (if (string= mizik-var~current-playlist mizik-var~temp-playlist) ;; Is the temp-playlist currently loaded?
	(if (or (string-match "http://" res) (string-match "https://" res))
	    (progn
	      (mpd-enqueue mizik-var~current-connection res)
	      (mpd-remove-playlist mizik-var~current-connection mizik-var~temp-playlist)
	      (mpd-save-playlist mizik-var~current-connection mizik-var~temp-playlist)
	      (setq res (concat "Audio Stream: " res)))
	  (mpd-simple-exec mizik-var~current-connection "playlistclear mizik~garbage")
	  (mpd-simple-exec mizik-var~current-connection (concat "playlistadd mizik~garbage " res))
	  (mpd-load-playlist mizik-var~current-connection "mizik~garbage"))
      (if (or (string-match "http://" res) (string-match "https://" res))
	  (progn
	    (mpd-simple-exec mizik-var~current-connection (concat "playlistadd " mizik-var~temp-playlist " " res))
	    (setq res (concat "Audio Stream: " res)))
	(write-region
	  (format "%s" (concat res "\n"))
	  nil
	  (concat mizik-var~mpd-playlist-dir mizik-var~temp-playlist ".m3u")
	  t)))
    (setq mizik-var~inserted-text-for-playlist
	  (lst
	    mizik-var~inserted-text-for-playlist
	    (if (> (length res) (window-width))
		(concat (substring res 0 (- (window-width) 3)) "…  ")
	      res)))))

(defun mizik-func~update-database ()
  (interactive)
  (mpd-update mizik-var~current-connection (if (neq mpd-db-root nil) mpd-db-root))
  ;; Album shit
    ;; writing library, sorted by album, to a file to use across Emacs sessions
  (write-region
   (format "%S" (mizik-util~generate-library-by-album))
   nil
   (concat mizik-var~load-path mizik-var~var-dir "library-by-album"))
    ;; re-setting the library-by-album variable to the newly calculated database
  (setq mizik-var~library-by-album
	(read (with-temp-buffer
		(insert-file-contents (concat mizik-var~load-path mizik-var~var-dir "library-by-album"))
		(buffer-string))))
    ;; writing the text used in the main buffer display, based off of which sorting
    ;; categories the user has chosen for the heading bar, to file
  (write-region
   (format "%S" (mizik-util~generate-by-album-insert-text))
   nil
   (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-album"))
    ;; re-setting the variable
  (setq mizik-var~inserted-text-by-album
	(read (with-temp-buffer
		(insert-file-contents (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-album"))
		(buffer-string))))
    ;; 
  (setq mizik-var~inserted-text-by-album~reversed
	(reverse mizik-var~inserted-text-by-album))
  (write-region
   (format "%S" mizik-var~inserted-text-by-album~reversed)
   nil
   (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-album~reversed"))
  ;; Artist shit
    ;; writing library, sorted by album, to a file to use across Emacs sessions
  (write-region
   (format "%S" (mizik-util~generate-library-by-artist))
   nil
   (concat mizik-var~load-path mizik-var~var-dir "library-by-artist"))
    ;; re-setting the library-by-artist variable to the newly calculated database
  (setq mizik-var~library-by-artist
	(read (with-temp-buffer
		(insert-file-contents (concat mizik-var~load-path mizik-var~var-dir "library-by-artist"))
		(buffer-string))))
    ;; writing the text used in the main buffer display, based off of which sorting
    ;; categories the user has chosen for the heading bar, to file
  (write-region
   (format "%S" (mizik-util~generate-by-artist-insert-text))
   nil
   (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-artist"))
    ;; re-setting the variable
  (setq mizik-var~inserted-text-by-artist
	(read (with-temp-buffer
		(insert-file-contents (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-artist"))
		(buffer-string))))
    ;; 
  (setq mizik-var~inserted-text-by-artist~reversed
	(reverse mizik-var~inserted-text-by-artist))
  (write-region
   (format "%S" mizik-var~inserted-text-by-artist~reversed)
   nil
   (concat mizik-var~load-path mizik-var~var-dir "inserted-text-by-artist~reversed"))
    ;; 
  (mizik-util~reset-aac))

(defun mizik-func~pause/resume ()
  (interactive)
  (if (string= (mizik-util~get-mpd-status) 'stop)
      (mpd-play mizik-var~current-connection)
    (mpd-pause mizik-var~current-connection))
  (let ((track-title (if (eq (car (cdr (member 'Title (mpd-get-current-song mizik-var~current-connection)))) nil)
			 "<No Name>"
		       (car (cdr (member 'Title (mpd-get-current-song mizik-var~current-connection))))))
	(track-artist (if (eq (car (cdr (member 'Artist (mpd-get-current-song mizik-var~current-connection)))) nil)
			  ""
			(car (cdr (member 'Artist (mpd-get-current-song mizik-var~current-connection)))))))
    (if (string= (mizik-util~get-mpd-status) 'play)
	(progn
	  (mizik-util~change-play-mode-line-button-to-pause)
	  (message (concat
		     "Now playing "
		     (propertize track-title 'face 'bold)
		     (if (not (string= track-artist ""))
			 (concat " by " track-artist)))))
      (progn
	(mizik-util~change-pause-mode-line-button-to-play)
	(message (concat
		   "Paused "
		   (propertize track-title 'face 'bold)
		   (if (not (string= track-artist ""))
		       (concat " by " track-artist))))))))

(defun mizik-func~previous ()
  (interactive)
  (mizik-util~change-play-mode-line-button-to-pause)
  (mpd-prev mizik-var~current-connection)
  (let ((track-title (if (eq (second (member 'Title (mpd-get-current-song mizik-var~current-connection))) nil)
			 "<No Name>"
		       (second (member 'Title (mpd-get-current-song mizik-var~current-connection)))))
	(track-artist (if (eq (second (member 'Artist (mpd-get-current-song mizik-var~current-connection))) nil)
			  ""
			(second (member 'Artist (mpd-get-current-song mizik-var~current-connection))))))
    (message (concat
	      "Skipped to the previous song; now playing "
	      (propertize track-title 'face 'bold)
	      (if (not (string= track-artist ""))
		  (concat " by " track-artist))))))

(defun mizik-func~next ()
  (interactive)
  (mizik-util~change-play-mode-line-button-to-pause)
  (mpd-next mizik-var~current-connection)
  (let ((track-title (if (eq (second (member 'Title (mpd-get-current-song mizik-var~current-connection))) nil)
			 "<No Name>"
		       (second (member 'Title (mpd-get-current-song mizik-var~current-connection)))))
	(track-artist (if (eq (second (member 'Artist (mpd-get-current-song mizik-var~current-connection))) nil)
			  ""
			(second (member 'Artist (mpd-get-current-song mizik-var~current-connection))))))
    (message (concat
	      "Skipped to the next song; now playing "
	      (propertize track-title 'face 'bold)
	      (if (not (string= track-artist ""))
		  (concat " by " track-artist))))))

(defun mizik-util~get-current-song-ID ()
  (second (member 'songid (mpd-get-status mizik-var~current-connection))))

(defun mizik-util~get-current-song-pos ()
  (second (member 'song (mpd-get-status mizik-var~current-connection))))

(defun mizik-util~get-mpd-status ()
  (second (member 'state (mpd-get-status mizik-var~current-connection))))

(defun mizik-util~get-song-playback-length ()
  (second (member 'time-total (mpd-get-status mizik-var~current-connection))))

(defun mizik-util~get-song-playback-position ()
  (string-to-number (second (member 'elapsed (mpd-get-status mizik-var~current-connection)))))

(defun mizik-util~chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string "\\(^[[:space:]\\n]*\\|[[:space:]\\n]*$\\)" "" str))

;; Library generation functions
(defun mizik-util~generate-library-by-album ()
  (let ((library (mpd-get-directory-songs mizik-var~current-connection))
	(library-by-album '()))
    (flet ((album (z)
		  (let ((cz (car z)))
		    (if (string= (if (numberp cz)
				     (number-to-string cz)
				   cz) "Album")
			(car (cdr z))
		      (if (eq (cdr (cdr z)) nil)
			  "Unknown Album"
			(album (cdr z))))))
	   (disc (z)
		 (let ((cz (car z)))
		   (if (string= (if (numberp cz)
				    (number-to-string cz)
				  cz) "Disc")
		       (car (cdr z))
		     (if (eq (cdr (cdr z)) nil)
			 ""
		       (disc (cdr z))))))
	   (track (z)
		  (let ((cz (car z)))
		    (if (string= (if (numberp cz)
				     (number-to-string cz)
				   cz) "Track")
			(car (cdr z))
		      (if (eq (cdr (cdr z)) nil)
			  ""
			(track (cdr z))))))
	   (songTable (info hash cat.s)
		      (let ((ci (car info))
			    (ca (car cat.s)))
			(if (or (string= ci "Album") (string= ci "Disc") (string= ci "Track"))
			    (songTable (cdr (cdr info)) hash (delete ci cat.s))
			  (if (eq cat.s nil)
			      hash
			    (if (member ca info)
				(progn
				  (puthash (car (member ca info)) (car (cdr (member ca info))) hash)
				  (songTable (append (reverse (cdr (member ca (reverse info)))) (cdr (cdr (member ca info)))) hash (delete ca cat.s)))
			      (progn
				(puthash ca "" hash)
				(songTable info hash (cdr cat.s))))))))
	   (stringc< (x y)
		     (string< (car x) (car y)))
	   (stringn< (x y)
		     (let ((cx (car x))
			   (cy (car y)))
		       (if (eq (and (string-match "/" cx) (string-match "/" cy)) nil) ;; One of them doesn't have a /
			   (if (neq (string-match "/" cx) nil) ;; X has a /
			       t
			     (if (string= cx "") ;; X is an empty string; you should probably sort alphabetically…
				 nil
			       (if (neq (string-match "/" cy) nil) ;; Y has a /
				   nil
				 (if (eq (length cx) (length cy)) ;; X and y are equal in length
				     (string< cx cy)
				   (if (< (length cx) (length cy)) ;; X is shorter than y
				       (let ((numb (-- (length cy))))
					 (while (> numb 0)
					   (setq cx (concat "0" cx))
					   (setq numb (-- numb)))
					 (string< cx cy))
				     (let ((numb (-- (length cx))))
				       (while (> numb 0)
					 (setq cy (concat "0" cy))
					 (setq numb (-- numb)))
				       (string< cx cy)))))))
			 (if (not (string=
				   (substring cx (string-match "/" cx))
				   (substring cy (string-match "/" cy)))) ;; Denominators are not equal
			     (let ((dcx (substring cx (string-match "/" cx)))
				   (dcy (substring cy (string-match "/" cy))))
			       (if (eq (length dcx) (length dcy)) ;; The denominators of x and y are equal in length
				   (string< dcx dcy)
				 (if (< (length dcx) (length dcy)) ;; The denominator of x is shorter than the denominator of y
				     (let ((numb (-- (length dcy))))
				       (while (> numb 0)
					 (setq dcx (concat "0" dcx))
					 (setq numb (-- numb)))
				       (string< dcx dcy))
				   (let ((numb (-- (length dcx))))
				     (while (> numb 0)
				       (setq dcy (concat "0" dcy))
				       (setq numb (-- numb)))
				     (string< dcx dcy)))))
			   (if (or
				(string= (substring cx 0 (string-match "/" cx)) "")
				(string= (substring cy 0 (string-match "/" cy)) "")) ;; Either x or y doesn't have a numerator
			       (if (string= (substring cx 0 (string-match "/" cx)) "") ;; X doesn't have a numerator
				   nil
				 t)
			     (let ((ncx (substring cx 0 (string-match "/" cx)))
				   (ncy (substring cy 0 (string-match "/" cy))))
			       (if (eq (length ncx) (length ncy)) ;; The numerators of x and y are equal in length
				   (string< ncx ncy)
				 (if (< (length ncx) (length ncy)) ;; The numerator of x is shorter than the numerator of y
				     (let ((numb (-- (length ncy))))
				       (while (> numb 0)
					 (setq ncx (concat "0" ncx))
					 (setq numb (-- numb)))
				       (string< ncx ncy))
				   (let ((numb (-- (length ncx))))
				     (while (> numb 0)
				       (setq ncy (concat "0" ncy))
				       (setq numb (-- numb)))
				     (string< ncx ncy)))))))))))
      (dolist (song library)
	(let ((songsAlbum (album song))
	      (songsDisc (disc song))
	      (songsTrack (track song))
	      (all-categories (mapcar 'car mizik-var~all-available-categories)))
	  (if (assoc songsAlbum library-by-album)
	      (if (assoc songsDisc (car (cdr (assoc songsAlbum library-by-album))))
		  (if (assoc songsTrack (car (cdr (assoc songsDisc (car (cdr (assoc songsAlbum library-by-album)))))))
		      (rplaca (cdr (assoc songsTrack (second (assoc songsDisc (second (assoc songsAlbum library-by-album)))))) (cons (songTable song (make-hash-table :test 'equal) all-categories) (second (assoc songsTrack (second (assoc songsDisc (second (assoc songsAlbum library-by-album))))))))
		    (rplaca (cdr (assoc songsDisc (second (assoc songsAlbum library-by-album)))) (sort (cons (cons songsTrack (list (list (songTable song (make-hash-table :test 'equal) all-categories)))) (second (assoc songsDisc (second (assoc songsAlbum library-by-album))))) 'stringn<)))
		(rplaca (cdr (assoc songsAlbum library-by-album)) (sort (cons (cons songsDisc (list (list (list songsTrack (list (songTable song (make-hash-table :test 'equal) all-categories)))))) (second (assoc songsAlbum library-by-album))) 'stringn<)))
	    (setq library-by-album (cons (cons songsAlbum (list (list (list songsDisc (list (list songsTrack (list (songTable song (make-hash-table :test 'equal) all-categories)))))))) library-by-album)))))
      (sort library-by-album 'stringc<))))

;; '((album0 . ((track0 . hash0) (track1 . hash1))) (album1 . ((track0 . hash0) (track1 . hash1) (track2 . hash2))))
			  
(defun mizik-util~generate-by-album-insert-text (&optional not-update)
  (let ((mizik-var~updated-field-spaces (mizik-util~update-field-spaces))
	(inserted-text-by-album '())
	(library-by-album mizik-var~library-by-album)
	(playlist mizik-var~playlist-album-sorted)
	(playlistReverse mizik-var~playlist-album-sorted~reversed)
	(playlistReverse-list '()))
    (flet ((mizik-util~generate-inserted-text (hash a-lbum d-isc t-rack)
					      (flet ((func (n f)
							   (if (> n 0)
							       (func (-- n) (concat f " "))
							     f)))
						(let ((tempText ""))
						  (dolist (item mizik-var~updated-field-spaces)
						    (let ((width (-- (cdr item)))
							  (category (if (eq (cdr (car item)) 'Track)
									t-rack
								      (if (eq (cdr (car item)) 'Album)
									  a-lbum
									(if (eq (cdr (car item)) 'Disc)
									    d-isc
									  (if (eq (cdr (car item)) 'Time) ;; Use a fucking recursive function…
									      (flet ((get-time (time result progress)
											       (let ((h-m-s (if (eq progress 1)
														(/ time 3600)
													      (if (eq progress 2)
														  (/ (% time 3600) 60)
														(% (% time 3600) 60)))))
												 (if (eq progress 4)
												     (substring result 0 (-- (length result)))
												   (get-time
												    time
												    (if (< h-m-s 10)
													(concat result "0" (int-to-string h-m-s) ":")
												      (concat result (int-to-string h-m-s) ":"))
												    (++ progress))))))
										(get-time (gethash (cdr (car item)) hash) "" 1))
									    (gethash (cdr (car item)) hash)))))))
						      (if (> (length category) width)
							  (setq tempText (concat tempText (concat (substring category 0 (- width 2)) "…  ")))
							(setq tempText (concat tempText (func (- (++ width) (length category)) category))))))
						  tempText))))
      (if (not not-update)
	  (mpd-clear-playlist mizik-var~current-connection))
      (dolist (album library-by-album)
	(let ((songsAlbum (car album))
	      (songList '())
	      (playlist-songList '()))
	  (dolist (disc (second album))
	    (let ((songsDisc (car disc)))
	      (dolist (track (second disc))
		(let ((songsTrack (car track)))
		  (dolist (song (second track))
		    (setq songList
			  (cons
			    (mizik-util~generate-inserted-text song songsAlbum songsDisc songsTrack)
			    songList))
		    (if (not not-update)
			(progn
			  (setq playlist-songList
				(cons
				  (gethash 'file song)
				  playlist-songList))
			  (mpd-enqueue mizik-var~current-connection (gethash 'file song)))))))))
	  (setq inserted-text-by-album
		(cons
		  (cons songsAlbum (list (reverse songList)))
		  inserted-text-by-album))
	  (if (not not-update)
	      (setq playlistReverse-list (cons
					  (cons songsAlbum (list (reverse playlist-songList)))
					  playlistReverse-list)))))
      (if (not not-update)
	  (progn
	    (mpd-remove-playlist mizik-var~current-connection playlist)
	    (mpd-save-playlist mizik-var~current-connection playlist)
	    (mpd-clear-playlist mizik-var~current-connection)
	    (dolist (song-list playlistReverse-list)
	      (dolist (file (second song-list))
		(mpd-enqueue mizik-var~current-connection file)))
	    (mpd-remove-playlist mizik-var~current-connection playlistReverse)
	    (mpd-save-playlist mizik-var~current-connection playlistReverse)
	    (mpd-clear-playlist mizik-var~current-connection)))
      (reverse inserted-text-by-album))))

;; By Artist functions
(defun mizik-util~generate-library-by-artist ()
  (let ((library (mpd-get-directory-songs mizik-var~current-connection))
	(library-by-artist '()))
    (flet ((album (z)
		  (if (member 'Album z)
		      (car (cdr (member 'Album z)))
		    ""))
	   (artist (z)
		   (if (member 'AlbumArtist z)
		       (car (cdr (member 'AlbumArtist z)))
		     (if (member 'Artist z)
			 (car (cdr (member 'Artist z)))
		       "")))
	   (disc (z)
		 (if (member 'Disc z)
		     (car (cdr (member 'Disc z)))
		   ""))
	   (track (z)
		  (if (member 'Track z)
		      (car (cdr (member 'Track z)))
		    ""))
	   (songTable (info hash cat.s)
		      (let ((ci (car info))
			    (ca (car cat.s)))
			(if (or (string= ci "Album") (string= ci "Disc") (string= ci "Track"))
			    (songTable (cdr (cdr info)) hash (delete ci cat.s))
			  (if (eq cat.s nil)
			      hash
			    (if (member ca info)
				(progn
				  (puthash (car (member ca info)) (car (cdr (member ca info))) hash)
				  (songTable (append (reverse (cdr (member ca (reverse info)))) (cdr (cdr (member ca info)))) hash (delete ca cat.s)))
			      (progn
				(puthash ca "" hash)
				(songTable info hash (cdr cat.s))))))))
	   (stringc< (x y)
		     (if (or (string= (car x) "") (string= (car y) ""))
			 (if (not (string= (car x) ""))
			     t
			   (if (not (string= (car y) ""))
			       nil
			     (string< (car x) (car y))))
		       (string< (car x) (car y))))
	   (stringn< (x y)
		     (let ((cx (car x))
			   (cy (car y)))
		       (if (eq (and (string-match "/" cx) (string-match "/" cy)) nil) ;; One of them doesn't have a /
			   (if (neq (string-match "/" cx) nil) ;; X has a /
			       t
			     (if (string= cx "") ;; X is an empty string; you should probably sort alphabetically…
				 nil
			       (if (neq (string-match "/" cy) nil) ;; Y has a /
				   nil
				 (if (eq (length cx) (length cy)) ;; X and y are equal in length
				     (string< cx cy)
				   (if (< (length cx) (length cy)) ;; X is shorter than y
				       (let ((numb (-- (length cy))))
					 (while (> numb 0)
					   (setq cx (concat "0" cx))
					   (setq numb (-- numb)))
					 (string< cx cy))
				     (let ((numb (-- (length cx))))
				       (while (> numb 0)
					 (setq cy (concat "0" cy))
					 (setq numb (-- numb)))
				       (string< cx cy)))))))
			 (if (not (string=
				   (substring cx (string-match "/" cx))
				   (substring cy (string-match "/" cy)))) ;; Denominators are not equal
			     (let ((dcx (substring cx (string-match "/" cx)))
				   (dcy (substring cy (string-match "/" cy))))
			       (if (eq (length dcx) (length dcy)) ;; The denominators of x and y are equal in length
				   (string< dcx dcy)
				 (if (< (length dcx) (length dcy)) ;; The denominator of x is shorter than the denominator of y
				     (let ((numb (-- (length dcy))))
				       (while (> numb 0)
					 (setq dcx (concat "0" dcx))
					 (setq numb (-- numb)))
				       (string< dcx dcy))
				   (let ((numb (-- (length dcx))))
				     (while (> numb 0)
				       (setq dcy (concat "0" dcy))
				       (setq numb (-- numb)))
				     (string< dcx dcy)))))
			   (if (or
				(string= (substring cx 0 (string-match "/" cx)) "")
				(string= (substring cy 0 (string-match "/" cy)) "")) ;; Either x or y doesn't have a numerator
			       (if (string= (substring cx 0 (string-match "/" cx)) "") ;; X doesn't have a numerator
				   nil
				 t)
			     (let ((ncx (substring cx 0 (string-match "/" cx)))
				   (ncy (substring cy 0 (string-match "/" cy))))
			       (if (eq (length ncx) (length ncy)) ;; The numerators of x and y are equal in length
				   (string< ncx ncy)
				 (if (< (length ncx) (length ncy)) ;; The numerator of x is shorter than the numerator of y
				     (let ((numb (-- (length ncy))))
				       (while (> numb 0)
					 (setq ncx (concat "0" ncx))
					 (setq numb (-- numb)))
				       (string< ncx ncy))
				   (let ((numb (-- (length ncx))))
				     (while (> numb 0)
				       (setq ncy (concat "0" ncy))
				       (setq numb (-- numb)))
				     (string< ncx ncy)))))))))))
      (dolist (song library)
	(let ((songsAlbum (album song))
	      (songsArtist (artist song))
	      (songsDisc (disc song))
	      (songsTrack (track song))
	      (all-categories (mapcar 'car mizik-var~all-available-categories)))
	  (if (assoc songsArtist library-by-artist)
	      (if (assoc songsAlbum (car (cdr (assoc songsArtist library-by-artist))))
		  (if (assoc songsDisc (car (cdr (assoc songsAlbum (car (cdr (assoc songsArtist library-by-artist)))))))
		      (if (assoc songsTrack (car (cdr (assoc songsDisc (car (cdr (assoc songsAlbum (car (cdr (assoc songsArtist library-by-artist))))))))))
			  (rplaca (cdr (assoc songsTrack (second (assoc songsDisc (second (assoc songsAlbum (second (assoc songsArtist library-by-artist)))))))) (cons (songTable song (make-hash-table :test 'equal) all-categories) (second (assoc songsTrack (second (assoc songsDisc (second (assoc songsAlbum (second (assoc songsArtist library-by-artist))))))))))
			(rplaca (cdr (assoc songsDisc (second (assoc songsAlbum (second (assoc songsArtist library-by-artist)))))) (sort (cons (cons songsTrack (list (list (songTable song (make-hash-table :test 'equal) all-categories)))) (second (assoc songsDisc (second (assoc songsAlbum (second (assoc songsArtist library-by-artist))))))) 'stringn<)))
		    (rplaca (cdr (assoc songsAlbum (second (assoc songsArtist library-by-artist)))) (sort (cons (cons songsDisc (list (list (list songsTrack (list (songTable song (make-hash-table :test 'equal) all-categories)))))) (second (assoc songsAlbum (second (assoc songsArtist library-by-artist))))) 'stringn<)))
		(rplaca (cdr (assoc songsArtist library-by-artist)) (sort (cons (cons songsAlbum (list (list (list songsDisc (list (list songsTrack (list (songTable song (make-hash-table :test 'equal) all-categories)))))))) (second (assoc songsArtist library-by-artist))) 'stringc<)))
	    (setq library-by-artist (cons (cons songsArtist (list (list (list songsAlbum (list (list songsDisc (list (list songsTrack (list (songTable song (make-hash-table :test 'equal) all-categories)))))))))) library-by-artist)))))
      (sort library-by-artist 'stringc<))))

(defun mizik-util~generate-by-artist-insert-text (&optional not-update)
  (let ((mizik-var~updated-field-spaces (mizik-util~update-field-spaces))
	(inserted-text-by-artist '())
	(library-by-artist mizik-var~library-by-artist)
	(playlist mizik-var~playlist-artist-sorted)
	(playlistReverse mizik-var~playlist-artist-sorted~reversed)
	(playlistReverse-list '()))
    (flet ((mizik-util~generate-inserted-text (hash a-lbum d-isc t-rack)
					      (flet ((func (n f)
							   (if (> n 0)
							       (func (-- n) (concat f " "))
							     f)))
						(let ((tempText ""))
						  (dolist (item mizik-var~updated-field-spaces)
						    (let ((width (-- (cdr item)))
							  (category (if (eq (cdr (car item)) 'Track)
									t-rack
								      (if (eq (cdr (car item)) 'Album)
									  a-lbum
									(if (eq (cdr (car item)) 'Disc)
									    d-isc
									  (if (eq (cdr (car item)) 'Time) ;; Use a fucking recursive function…
									      (flet ((get-time (time result progress)
											       (let ((h-m-s (if (eq progress 1)
														(/ time 3600)
													      (if (eq progress 2)
														  (/ (% time 3600) 60)
														(% (% time 3600) 60)))))
												 (if (eq progress 4)
												     (substring result 0 (-- (length result)))
												   (get-time
												    time
												    (if (< h-m-s 10)
													(concat result "0" (int-to-string h-m-s) ":")
												      (concat result (int-to-string h-m-s) ":"))
												    (++ progress))))))
										(get-time (gethash (cdr (car item)) hash) "" 1))
									    (gethash (cdr (car item)) hash)))))))
						      (if (> (length category) width)
							  (setq tempText (concat tempText (concat (substring category 0 (- width 2)) "…  ")))
							(setq tempText (concat tempText (func (- (++ width) (length category)) category))))))
						  tempText))))
      (if (not not-update)
	  (mpd-clear-playlist mizik-var~current-connection))
      (dolist (artist library-by-artist)
	(let ((songsArtist (car artist))
	      (songList '())
	      (playlist-songList '()))
	  (dolist (album (second artist))
	    (let ((songsAlbum (car album)))
	      (dolist (disc (second album))
		(let ((songsDisc (car disc)))
		  (dolist (track (second disc))
		    (let ((songsTrack (car track)))
		      (dolist (song (second track))
			(setq songList (cons
					(mizik-util~generate-inserted-text song songsAlbum songsDisc songsTrack)
					songList))
			(if (not not-update)
			    (progn
			      (setq playlist-songList (cons
						       (gethash 'file song)
						       playlist-songList))
			      (mpd-enqueue mizik-var~current-connection (gethash 'file song)))))))))))
	  (setq inserted-text-by-artist (cons
					 (cons songsArtist (list (reverse songList)))
					 inserted-text-by-artist))
	  (if (not not-update)
	      (setq playlistReverse-list (cons
					  (cons songsArtist (list (reverse playlist-songList)))
					  playlistReverse-list)))))
      (if (not not-update)
	  (progn
	    (mpd-remove-playlist mizik-var~current-connection playlist)
	    (mpd-save-playlist mizik-var~current-connection playlist)
	    (mpd-clear-playlist mizik-var~current-connection)
	    (dolist (song-list playlistReverse-list)
	      (dolist (file (second song-list))
		(mpd-enqueue mizik-var~current-connection file)))
	    (mpd-remove-playlist mizik-var~current-connection playlistReverse)
	    (mpd-save-playlist mizik-var~current-connection playlistReverse)
	    (mpd-clear-playlist mizik-var~current-connection)))
      (reverse inserted-text-by-artist))))

(defun mizik-util~update-field-spaces ()
  (flet ((adder (x n neg)
		(if (neq (car x) nil)
		    (if (/= (car x) -1)
			(adder (cdr x) (+ (car x) n) neg)
		      (adder (cdr x) n (++ neg)))
		  (cons n neg)))
	 (parse (p)
		(if (= (cdr p) 0)
		    0
		  (/ (- (window-width) (car p)) (cdr p))))
	 (fix (l)
	      (if (/= (cdr l) -1)
		  (if (< (length (car (car l))) (cdr l))
		      l
		    (cons (car l) (length (car (car l)))))
		(cons (car l) width))))
    (let ((numlist (mapcar 'cdr mizik-var~field-info)))
      (let ((width (parse (adder numlist 0 0))))
	(mapcar 'fix mizik-var~field-info)))))

(defun mizik-util~determine-spaces (pair)
  "Determine how many spaces ought to be added to the field title
to provide a particular width to the field column based upon the
dotted pair provided by `mizik-util~get-sort-token'."
  (let ((field (car (car pair)))
	(width (cdr pair)))
    (if (eq (cdr (car pair)) (car mizik-var~sort-token))
	(if (eq 'Regular (cdr mizik-var~sort-token))
	    (setq field (concat (substring field 0 (- (length field) 2)) "▼ "))
	  (setq field (concat (substring field 0 (- (length field) 2)) "▲ "))))
    (if (>= (length field) width)
	field
      (flet ((func (n f)
		   (if (> n 0)
		       (func (-- n) (concat f " "))
		     f)))
	(func (- width (length field)) field)))))

(defun mizik-func~main-view ()
  "Show the Mizik library."
  (interactive)
  (setq mizik-var~last-view 'main)
  (if (buffer-live-p mizik-var~library-buffer)
      (switch-to-buffer mizik-var~library-buffer)
    (setq mizik-var~library-buffer (get-buffer-create mizik-var~library-buffer-name))
    (with-current-buffer mizik-var~library-buffer
      (erase-buffer)
      ;; Play the currently selected song
      (local-set-key (kbd "RET") (lambda ()
				   (interactive)
				   ;; If no playlist is loaded…
				   (if (eq (mpd-get-playlist mizik-var~current-connection) nil)
				       ;; If current display isn't organized in reverse…
				       (if (eq (cdr mizik-var~sort-token) 'Regular)
					   (progn
					     (setq mizik-var~current-playlist
						   (car (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))
					     (mpd-load-playlist
					      mizik-var~current-connection
					      (car (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories)))))
					 (progn
					   (setq mizik-var~current-playlist
						 (cdr (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))
					   (mpd-load-playlist
					    mizik-var~current-connection
					    (cdr (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))))
				     (if (not (string=
					       mizik-var~current-playlist
					       (if (eq (cdr mizik-var~sort-token) 'Regular)
						   (car (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories)))
						 (cdr (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))))
					 (progn
					   (mpd-clear-playlist mizik-var~current-connection)
					   (if (eq (cdr mizik-var~sort-token) 'Regular)
					       (progn
						 (setq mizik-var~current-playlist
						       (car (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))
						 (mpd-load-playlist
						  mizik-var~current-connection
						  (car (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories)))))
					     (progn
					       (setq mizik-var~current-playlist
						     (cdr (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))
					       (mpd-load-playlist
						mizik-var~current-connection
						(cdr (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories)))))))))
				   (mpd-play
				    mizik-var~current-connection
				    (-- (line-number-at-pos)))
				   (mizik-util~change-play-mode-line-button-to-pause)
				   (let ((track-title (if (eq (car (cdr (member 'Title (mpd-get-current-song mizik-var~current-connection)))) nil)
							  "<No Name>"
							(car (cdr (member 'Title (mpd-get-current-song mizik-var~current-connection))))))
					 (track-artist (if (eq (car (cdr (member 'Artist (mpd-get-current-song mizik-var~current-connection)))) nil)
							   ""
							 (car (cdr (member 'Artist (mpd-get-current-song mizik-var~current-connection)))))))
				     (message (concat
					       "Now playing "
					       (propertize track-title 'face 'bold)
					       (if (not (string= track-artist ""))
						   (concat " by " track-artist)))))))
      ;; Sort by aLbum
      (local-set-key (kbd "l") (lambda ()
				 (interactive)
				 (setq mizik-var~sort-token (cons 'Album (cdr mizik-var~sort-token)))
				 (mizik-util~reset-aac)
				 (setq mizik-var~last-line-position~main (line-number-at-pos))
				 (kill-buffer mizik-var~library-buffer)
				 (mizik-func~main-view)))
      ;; Sort by aRtist
      (local-set-key (kbd "r") (lambda ()
				 (interactive)
				 (setq mizik-var~sort-token (cons 'Artist (cdr mizik-var~sort-token)))
				 (mizik-util~reset-aac)
				 (setq mizik-var~last-line-position~main (line-number-at-pos))
				 (kill-buffer mizik-var~library-buffer)
				 (mizik-func~main-view)))
      ;; Reverse (toggle) the order of the view
      (local-set-key (kbd "t") (lambda ()
				 (interactive)
				 (setq mizik-var~sort-token (cons
							      (car mizik-var~sort-token)
							      (if (eq (cdr mizik-var~sort-token) 'Regular)
								  'Reverse
								'Regular)))
				 (setq mizik-var~last-line-position~main (line-number-at-pos))
				 (kill-buffer mizik-var~library-buffer)
				 (mizik-func~main-view)))
      ;; Add song to Playlist
      (local-set-key (kbd "a") (lambda ()
				 (interactive)
				 (let ((res (expand-file-name (concat
							        mizik-var~mpd-music-dir
								(second (member 'file (car (mpd-get-playlist-entry
											     mizik-var~current-connection
											     (-- (line-number-at-pos))))))))))
				   (if (string= mizik-var~current-playlist mizik-var~temp-playlist) ;; Is the temp-playlist currently loaded?
				       (progn
					 (mpd-simple-exec mizik-var~current-connection "playlistclear mizik~garbage")
					 (mpd-simple-exec mizik-var~current-connection (concat "playlistadd mizik~garbage " res))
					 (mpd-load-playlist mizik-var~current-connection "mizik~garbage"))
				     (write-region
				       (format "%s" (concat res "\n"))
				       nil
				       (concat mizik-var~mpd-playlist-dir mizik-var~temp-playlist ".m3u")
				       t))
				   (setq mizik-var~inserted-text-for-playlist
					 (lst
					   mizik-var~inserted-text-for-playlist
					   (if (> (length res) (window-width))
					       (concat (substring res 0 (- (window-width) 3)) "…  ")
					     res))))))
      ;; Switch to Library View
      (local-set-key (kbd "h") (lambda ()
				 (interactive)
				 (mizik-func~playlist-view)))
      ;; Kill the window
      (local-set-key (kbd "C-x k") (lambda ()
				     (interactive)
				     (setq mizik-var~last-line-position~main (line-number-at-pos))
				     (kill-buffer)))
      (local-set-key (kbd "k") (lambda ()
				 (interactive)
				 (setq mizik-var~last-line-position~main (line-number-at-pos))
				 (kill-buffer)))
      ;; Go to the currently playing song
      (local-set-key (kbd "g") (lambda ()
				 (interactive)
				 (if (string= (mizik-util~get-mpd-status) 'stop)
				     (message "No song currently playing!")
				   (if (or
					 (string= mizik-var~current-playlist mizik-var~temp-playlist)
					 (string= mizik-var~current-playlist (if (eq (cdr mizik-var~sort-token) 'Regular)
										 (car (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories)))
									       (cdr (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))))
				       (goto-line (++ (mizik-util~get-current-song-pos)))
				     (message "Wrong playlist view!")))))
      ;; Pause/play the currently playing song
      (local-set-key (kbd "SPC") (lambda ()
				   (interactive)
				   (mizik-func~pause/resume)))
      ;; Set the header-line format based on which sorting categories the user has chosen
      (setq header-line-format
	    (cons (propertize " " 'display '((space :align-to 0)))
		  (mapcar 'mizik-util~determine-spaces (mizik-util~update-field-spaces))))
      (let ((inserted-text (if (eq (cdr mizik-var~sort-token) 'Regular)
			       (car (second (assoc (car mizik-var~sort-token) mizik-var~all-available-categories)))
			     (cdr (second (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))))
	(dolist (songList inserted-text)
	  (dolist (song (second songList))
	    (insert (propertize (concat song "\n") 'face 'bold)))))
      (read-only-mode)
      (goto-line mizik-var~last-line-position~main)
      (hl-line-mode t)
      (switch-to-buffer mizik-var~library-buffer-name))))

(defun mizik-func~playlist-view ()
  "Show the Mizik temperary playlist."
  (interactive)
  (setq mizik-var~last-view 'playlist)
  (if (buffer-live-p mizik-var~playlist-buffer)
      (kill-buffer mizik-var~playlist-buffer))
  (setq mizik-var~playlist-buffer (get-buffer-create mizik-var~playlist-buffer-name))
  (with-current-buffer mizik-var~playlist-buffer
    (erase-buffer)
    ;; Play the currently selected song
    (local-set-key (kbd "RET") (lambda ()
				 (interactive)
				 (if (not (string= (buffer-string) ""))
				     (progn
				       ;; If no playlist is loaded…
				       (if (eq (mpd-get-playlist mizik-var~current-connection) nil)
					   (progn
					     (setq mizik-var~current-playlist mizik-var~temp-playlist)
					     (mpd-load-playlist mizik-var~current-connection mizik-var~temp-playlist))
					 (if (not (string= mizik-var~current-playlist mizik-var~temp-playlist))
					     (progn
					       (mpd-clear-playlist mizik-var~current-connection)
					       (setq mizik-var~current-playlist mizik-var~temp-playlist)
					       (mpd-load-playlist mizik-var~current-connection mizik-var~temp-playlist))))
				       (mpd-play mizik-var~current-connection (-- (line-number-at-pos)))
				       (mizik-util~change-play-mode-line-button-to-pause)
				       (message "Now playing.")))))
    ;; Delete song from playlist
    (local-set-key (kbd "d") (lambda ()
			       (interactive)
			       (if (string= mizik-var~current-playlist mizik-var~temp-playlist) ;; Is the temp-playlist currently loaded?
				   (progn
				     (mpd-delete mizik-var~current-connection (-- (line-number-at-pos)))
				     (mpd-remove-playlist mizik-var~current-connection mizik-var~temp-playlist)
				     (mpd-save-playlist mizik-var~current-connection mizik-var~temp-playlist)
				     (setq mizik-var~inserted-text-for-playlist
					   (lst
					     (: mizik-var~inserted-text-for-playlist 0 (-- (line-number-at-pos)))
					     (: mizik-var~inserted-text-for-playlist (line-number-at-pos))))
				     (setq mizik-var~last-line-position~playlist (line-number-at-pos))
				     (kill-buffer)
				     (mizik-func~playlist-view))
				 (let ((ln (line-number-at-pos)))
				   (progn
				     (write-region
				      (with-temp-buffer
					(insert-file-contents (concat mizik-var~mpd-playlist-dir mizik-var~temp-playlist ".m3u"))
					(goto-line ln)
					(kill-line)
					(kill-line)
					(buffer-string))
				      nil
				      (concat mizik-var~mpd-playlist-dir mizik-var~temp-playlist ".m3u"))
				     (setq mizik-var~inserted-text-for-playlist
					   (lst
					     (: mizik-var~inserted-text-for-playlist 0 (-- (line-number-at-pos)))
					     (: mizik-var~inserted-text-for-playlist (line-number-at-pos))))
				     (setq mizik-var~last-line-position~playlist (line-number-at-pos))
				     (kill-buffer)
				     (mizik-func~playlist-view))))))
    ;; Move selected song forward in position of playlist
    (local-set-key (kbd "n") (lambda ()
			       (interactive)
			       (let ((ln (line-number-at-pos)))
				  (if (string= mizik-var~current-playlist mizik-var~temp-playlist) ;; Is the temp-playlist currently loaded?
				      (progn
					 (mpd-swap mizik-var~current-connection (-- ln) ln)
					 (mpd-remove-playlist mizik-var~current-connection mizik-var~temp-playlist)
					 (mpd-save-playlist mizik-var~current-connection mizik-var~temp-playlist))
				    (write-region
				      (with-temp-buffer
					 (insert-file-contents (concat mizik-var~mpd-playlist-dir mizik-var~temp-playlist ".m3u"))
					 (goto-line (++ ln))
					 (transpose-lines 1)
					 (buffer-string))
				      nil
				      (concat mizik-var~mpd-playlist-dir mizik-var~temp-playlist ".m3u")))
				  (setq mizik-var~inserted-text-for-playlist
					(lst
					  (: mizik-var~inserted-text-for-playlist 0 (-- ln))
					  (!! mizik-var~inserted-text-for-playlist ln)
					  (!! mizik-var~inserted-text-for-playlist (-- ln))
					  (: mizik-var~inserted-text-for-playlist (++ ln))))
				  (setq mizik-var~last-line-position~playlist (++ ln))
				  (kill-buffer)
				  (mizik-func~playlist-view))))
    ;; Move selected song backwards in position of playlist
    (local-set-key (kbd "p") (lambda ()
			       (interactive)
			       (let ((ln (line-number-at-pos)))
				  (if (string= mizik-var~current-playlist mizik-var~temp-playlist) ;; Is the temp-playlist currently loaded?
				      (progn
					 (mpd-swap mizik-var~current-connection (- ln 2) (-- ln))
					 (mpd-remove-playlist mizik-var~current-connection mizik-var~temp-playlist)
					 (mpd-save-playlist mizik-var~current-connection mizik-var~temp-playlist))
				    (write-region
				      (with-temp-buffer
					 (insert-file-contents (concat mizik-var~mpd-playlist-dir mizik-var~temp-playlist ".m3u"))
					 (goto-line ln)
					 (transpose-lines 1)
					 (buffer-string))
				      nil
				      (concat mizik-var~mpd-playlist-dir mizik-var~temp-playlist ".m3u")))
				  (setq mizik-var~inserted-text-for-playlist
					(lst
					  (: mizik-var~inserted-text-for-playlist 0 (- ln 2))
					  (!! mizik-var~inserted-text-for-playlist (-- ln))
					  (!! mizik-var~inserted-text-for-playlist (- ln 2))
					  (: mizik-var~inserted-text-for-playlist ln)))
				  (setq mizik-var~last-line-position~playlist (-- ln))
				  (kill-buffer)
				  (mizik-func~playlist-view))))
    ;; Go to the currently playing song
    (local-set-key (kbd "g") (lambda ()
			       (interactive)
			       (if (string= (mizik-util~get-mpd-status) 'stop)
				   (message "No song currently playing!")
				 (if (or
					 (string= mizik-var~current-playlist mizik-var~temp-playlist)
					 (string= mizik-var~current-playlist (if (eq (cdr mizik-var~sort-token) 'Regular)
										(car (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories)))
									      (cdr (last (assoc (car mizik-var~sort-token) mizik-var~all-available-categories))))))
				     (goto-line (++ (mizik-util~get-current-song-pos)))
				   (message "Wrong playlist view!")))))
    ;; Switch to Library View
    (local-set-key (kbd "h") (lambda ()
			       (interactive)
			       (mizik-func~main-view)))
    ;; Kill the window
    (local-set-key (kbd "C-x k") (lambda ()
				   (interactive)
				   (setq mizik-var~last-line-position~playlist (line-number-at-pos))
				   (kill-buffer)))
    (local-set-key (kbd "k") (lambda ()
			       (interactive)
			       (setq mizik-var~last-line-position~playlist (line-number-at-pos))
			       (kill-buffer)))
    ;; Pause/play the currently playing song
    (local-set-key (kbd "SPC") (lambda ()
				 (interactive)
				 (mizik-func~pause/resume)))
    ;; Pause/play the currently playing song
    (local-set-key (kbd ";") (lambda ()
				 (interactive)
				 (mizik-func~next)))
    ;; Pause/play the currently playing song
    (local-set-key (kbd "j") (lambda ()
				 (interactive)
				 (mizik-func~previous)))
    ;; Pause/play the currently playing song
    (local-set-key (kbd "a") (lambda ()
				 (interactive)
				 (mizik-func~add-song-to-playlist)))
    ;; Set the header-line format based on which sorting categories the user has chosen
    (setq header-line-format
	  (cons (propertize " " 'display '((space :align-to 0)))
		(mapcar 'mizik-util~determine-spaces (mizik-util~update-field-spaces))))
    (dolist (item mizik-var~inserted-text-for-playlist)
      (insert (concat (propertize item 'face 'bold) "\n")))
    (read-only-mode)
    (goto-line mizik-var~last-line-position~playlist)
    (hl-line-mode t)
    (switch-to-buffer mizik-var~playlist-buffer-name)))

(defun mizik ()
  (interactive)
  (unless (mpd-assert-mpd-conn mizik-var~current-connection)
      (shell-command "mpd"))
  (if (string= mizik-var~last-view 'main)
      (mizik-func~main-view)
    (mizik-func~playlist-view)))

(provide 'mizik)




