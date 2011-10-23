(in-package :trivial-gray-streams)

(defclass trivial-gray-stream-mixin () ())

(defgeneric stream-read-sequence
    (stream sequence start end &key &allow-other-keys))
(defgeneric stream-write-sequence
    (stream sequence start end &key &allow-other-keys))

(defgeneric stream-file-position (stream))
(defgeneric (setf stream-file-position) (newval stream))

(defmethod stream-write-string
    ((stream trivial-gray-stream-mixin) seq &optional start end)
  (stream-write-sequence stream seq (or start 0) (or end (length seq))))

;; Implementations should provide this default method, I believe, but
;; at least sbcl and allegro don't.
(defmethod stream-terpri ((stream trivial-gray-stream-mixin))
  (write-char #\newline stream))

(defmethod stream-file-position ((stream trivial-gray-stream-mixin))
  nil)

(defmethod (setf stream-file-position)
    (newval (stream trivial-gray-stream-mixin))
  (declare (ignore newval))
  nil)

#+allegro
(progn
  (defmethod excl:stream-read-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-read-sequence s seq (or start 0) (or end (length seq))))
  (defmethod stream:stream-write-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-write-sequence s seq (or start 0) (or end (length seq)))))

#+cmu
(progn
  (defmethod ext:stream-read-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-read-sequence s seq (or start 0) (or end (length seq))))
  (defmethod ext:stream-write-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-write-sequence s seq (or start 0) (or end (length seq)))))

#+lispworks
(progn
  (defmethod stream:stream-read-sequence
      ((s trivial-gray-stream-mixin) seq start end)
    (stream-read-sequence s seq start end))
  (defmethod stream:stream-write-sequence
      ((s trivial-gray-stream-mixin) seq start end)
    (stream-write-sequence s seq start end))

  (defmethod stream:stream-file-position ((stream trivial-gray-stream-mixin))
    (stream-file-position stream))
  (defmethod (setf stream:stream-file-position)
      (newval (stream trivial-gray-stream-mixin))
    (setf (stream-file-position stream) newval)))

#+openmcl
(progn
  (defmethod ccl:stream-read-vector
      ((s trivial-gray-stream-mixin) seq start end)
    (stream-read-sequence s seq start end))
  (defmethod ccl:stream-write-vector
      ((s trivial-gray-stream-mixin) seq start end)
    (stream-write-sequence s seq start end)))

;; up to version 2.43 there were no
;; stream-read-sequence, stream-write-sequence
;; functions in CLISP
#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "STREAM-READ-SEQUENCE" "GRAY")
    (pushnew :clisp-has-stream-read/write-sequence *features*)))

#+clisp
(progn

  #+clisp-has-stream-read/write-sequence
  (defmethod gray:stream-read-sequence
      (seq (s trivial-gray-stream-mixin) &key start end)
    (stream-read-sequence s seq (or start 0) (or end (length seq))))

  #+clisp-has-stream-read/write-sequence
  (defmethod gray:stream-write-sequence
      (seq (s trivial-gray-stream-mixin) &key start end)
    (stream-write-sequence s seq (or start 0) (or end (length seq))))
  
  ;; Even despite the stream-read/write-sequence are present in newer 
  ;; CLISP, it's better to provide stream-(read/write)-(byte/char)-sequence
  ;; methods too.
  ;; Example: if fundamental-binary-input-stream comes in the
  ;; class precedence list of your user-defined stream before
  ;; the trivial-gray-steam-mixin, the default CLISP's implementation
  ;; of the gray:stream-read-sequence will be used; and this default 
  ;; implementation calls the gray:stream-read-byte-sequence.
  ;; Therefore we override gray:stream-read-byte-sequence and call
  ;; our stream-read-sequence.

  (defmethod gray:stream-read-byte-sequence
      ((s trivial-gray-stream-mixin)
       seq
       &optional start end no-hang interactive)
    (when no-hang
      (error "this stream does not support the NO-HANG argument"))
    (when interactive
      (error "this stream does not support the INTERACTIVE argument"))
    (stream-read-sequence s seq start end))

  (defmethod gray:stream-write-byte-sequence
      ((s trivial-gray-stream-mixin)
       seq
       &optional start end no-hang interactive)
    (when no-hang
      (error "this stream does not support the NO-HANG argument"))
    (when interactive
      (error "this stream does not support the INTERACTIVE argument"))
    (stream-write-sequence s seq start end))

  (defmethod gray:stream-read-char-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-read-sequence s seq start end))

  (defmethod gray:stream-write-char-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-write-sequence s seq start end))

  (defmethod gray:stream-position ((stream trivial-gray-stream-mixin) position)
    (if position
	(setf (stream-file-position stream) position)
        (stream-file-position stream))))

#+sbcl
(progn
  (defmethod sb-gray:stream-read-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-read-sequence s seq (or start 0) (or end (length seq))))
  (defmethod sb-gray:stream-write-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-write-sequence s seq (or start 0) (or end (length seq))))
  ;; SBCL extension:
  (defmethod sb-gray:stream-line-length ((stream trivial-gray-stream-mixin))
    80))

#+ecl
(progn
  (defmethod gray:stream-read-sequence
    ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-read-sequence s seq (or start 0) (or end (length seq))))
  (defmethod gray:stream-write-sequence
    ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-write-sequence s seq (or start 0) (or end (length seq)))))
