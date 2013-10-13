(defclass subject ()
  ((code :accessor code :initarg :code)
   (name :accessor name :initarg :name)
   (creds :accessor creds :initarg :creds)
   (reqs :accessor reqs :initarg :reqs)))

(defclass grade ()
  ((code :accessor code :initarg :code)
   (grade :accessor grade :initarg :grade)))

(defun subject (grade)
  "Get the subject related to a passing grade"
  (gethash (code grade) *subjects*))

(defun load-db ()
  "Populate tables from filesystem"
  (defparameter *subjects* (make-hash-table))
  (with-open-file (in "subjects")
    (dolist (line (read in))
      (destructuring-bind (code name &optional creds reqs) line
        (setf (gethash code *subjects*)
              (make-instance 'subject
                             :code code
                             :name name
                             :creds creds
                             :reqs reqs)))))

  (defparameter *grades* (make-hash-table))
  (with-open-file (in "grades")
    (dolist (line (read in))
      (destructuring-bind (code &optional grade) line
        (setf (gethash code *grades*)
              (make-instance 'grade
                             :code code
                             :grade grade))))))

(defun store-db ()
  "Persist changes into filesystem"
  (with-open-file (out "grades" :direction :output :if-exists :supersede)
    (print (loop for grade being the hash-values in *grades*
                 collect (list (code grade) (grade grade)))
           out)))

(defun print-subject (subject)
  "Print subject code and name"
  (format t "~&~a ~a~&" (code subject) (name subject)))

(defun get-grade (code)
  (let ((grade (gethash code *grades*)))
    (when grade (print-grade (gethash code *grades*)))
    grade))

(defun set-grade (code grade)
  (when (gethash code *subjects*)
    (setf (gethash code *grades*)
          (make-instance 'grade
                         :code code
                         :grade grade))))

(defun unset-grade (code)
  (remhash code *grades*))

(defun print-grade (grade)
  "Print system code, grade given, and name"
  (let ((code (code grade)))
    (format t "~&~a ~a ~a~&"
            code
            (grade grade)
            (name (gethash code *subjects*)))))

(defun subject-passed-p (subject)
  "Returns whether the subject has been passed"
  (not (not (gethash (code subject) *grades*))))

(defun subject-available-p (subject)
  "Returns whether the subject is available"
  (not (or (subject-passed-p subject)
           (let ((reqs (reqs subject)))
             (case (first reqs)
               ('creds (> (second reqs) (total-creds)))
               (otherwise (notevery (lambda (req) (gethash req *grades*))
                                    (reqs subject))))))))

(defun total-creds ()
  "Returns sum of credits of passed subjects"
  (loop for grade being the hash-values in *grades*
        when (subject grade)
        summing (creds (subject grade))))

(defun passed ()
  "Print passed subjects with their grades"
  (loop for grade being the hash-values in *grades* do
             (when (gethash (code grade) *subjects*)
               (print-grade grade))))

(defun missing ()
  "Prints missing subjects"
  (loop for subject being the hash-values in *subjects* do
             (unless (subject-passed-p subject)
               (print-subject subject))))

(defun available ()
  "Prints missing subjects having all dependencies met"
  (loop for subject being the hash-values in *subjects* do
        (when (subject-available-p subject)
          (print-subject subject))))

