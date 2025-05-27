(defparameter *node* '((living-room (you are asleep in the living room and a wizard is snoring on the couch))
                       (garden      (you are in a beautiful garden. there is a well in front of you))
                       (attic       (you are in the attic. there is a giant welding torch in the corner))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges* '((living-room
                         (garden west door)
                         (attic upstairs ladder))
                        (garden
                         (living-room east door))
                        (attic
                         (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *node*)
          (describe-paths *location* *edges*)
          (describ-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that go way))))

(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-location)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-location)) loc)))
           (remove-if-not #'at-loc-p objs)))

(defun describ-objects (loc objs obj-location)
  (labels ((describe-obj (obj)
                        `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-location)))))
