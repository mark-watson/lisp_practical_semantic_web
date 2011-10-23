(require :agraph)

;; Copyright Mark Watson 2001-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

(in-package :db.agraph.user)
(enable-!-reader)
(register-namespace "g" "http://knowledgebooks.com/geo#")
(create-triple-store "/tmp/geospatial-test")


;; define some locations in Verde Valley, Arizona:
(defvar *locs*
  '(("Verde_Valley_Ranger_Station" 34.7666667 -112.1416667)
    ("Verde_Valley_School" 34.8047596 -111.8060388)
    ("Sedona" 34.8697222 -111.7602778)
    ("Cottonwood" 34.739 -112.009)
    ("Jerome" 34.75 -112.11)
    ("Flagstaff" 35.20 -111.63)
    ("Clarkdale" 34.76 -112.05)
    ("Mount_Wilson" 35.996 -114.611)
    ("Tuzigoot" 34.56 -111.84)))

(defvar *min-lat* (reduce #'min (mapcar #'cadr *locs*)))
(defvar *max-lat* (reduce #'max (mapcar #'cadr *locs*)))
(defvar *min-lon* (reduce #'min (mapcar #'caddr *locs*)))
(defvar *max-lon* (reduce #'max (mapcar #'caddr *locs*)))

;; create a type:
(setf offset 5.0)
(flet ((fixup (num direction)
              (if (eq direction :min)
                  (- num offset)
                (+ num offset))))
  (setf *verde-valley-arizona*
        (db.agraph:register-latitude-striping-in-miles
         3
         :lat-min (fixup *min-lat* :min)
         :lat-max (fixup *max-lat* :max)
         :lon-min (fixup *min-lon* :min)
         :lon-max (fixup *max-lon* :max))))

(add-geospatial-subtype-to-db *verde-valley-arizona*)

(dolist (loc *locs*)
  (let ((name (intern-resource (format nil "http://knowledgebooks.com/geo#~a" (car loc)))))
    (print name)
    (add-triple name !g:isAt3 (longitude-latitude->upi *verde-valley-arizona* (caddr loc) (cadr loc)))))

(index-all-triples :wait t)


(print
 (count-cursor
  (get-triples-haversine-miles *verde-valley-arizona* !g:isAt3 -112.009 34.739 30.0)))

(dolist (distance '(50.0 30.0 10.0 5.0))
  (format t "~%~%Checking with distance = ~A~%" distance)
  (let ((cursor (get-triples-haversine-miles *verde-valley-arizona* !g:isAt3 -112.009 34.739 distance)))
    (while (cursor-next-p cursor)
      (print (cursor-next-row cursor)))))

