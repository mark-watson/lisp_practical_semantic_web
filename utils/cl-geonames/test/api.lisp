;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.lisp
;;;; Purpose:       Unit Tests for the Common Lisp Geonames API.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-geonames, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-geonames are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cl-geonames-test)




(lift:addtest (cl-geonames-test)
  unable-host-test 
  (:documentation "Test condition when the Geonames server is unable.")
  (lift:ensure-condition 'usocket:unknown-error
    (let  ((cl-geonames::*geonames-server*
            (format nil "http://www.~A.org" (gensym))))
      (cl-geonames:geo-country-code "44.8391224692125" "-0.585060840409772"))))


(lift:addtest (cl-geonames-test)
  country-info-test
  (:documentation "Test retrieve countries informations.")
  (let ((data (cl-geonames:geo-country-info :country '("FR" "GB"))))
    (lift:ensure (= 2 (length (cdr data))))
    (destructuring-bind (fr-country gb-country) (cdr data)
      (loop for keyword in '(:|countryCode|  :|countryName|
                             :|isoNumeric| :|isoAlpha3| :|fipsCode|
                             :|continent| :|capital| :|areaInSqKm|
                             :|population| :|currencyCode|
                             :|languages| :|geonameId|
                             :|bBoxWest| :|bBoxNorth| :|bBoxEast| :|bBoxSouth|)
         do (lift:ensure (find keyword (cdr fr-country) :test #'equal :key #'car))
            (lift:ensure (find keyword (cdr gb-country) :test #'equal :key #'car)))
      (lift:ensure (string-equal "FR" (cadr (second fr-country))))
      (lift:ensure (string-equal "GB" (cadr (second gb-country))))
      (lift:ensure (string-equal "France" (cadr (third fr-country))))
      (lift:ensure (string-equal "United Kingdom" (cadr (third gb-country))))
      (lift:ensure (string-equal "250" (cadr (fourth fr-country))))
      (lift:ensure (string-equal "826" (cadr (fourth gb-country))))
      (lift:ensure (string-equal "FRA" (cadr (fifth fr-country))))
      (lift:ensure (string-equal "GBR" (cadr (fifth gb-country)))))))


(lift:addtest (cl-geonames-test)
  country-code-test
  (:documentation "Test retrieve the iso country code for the given LATITUDE/LONGITUDE.")
  (let ((data (cl-geonames:geo-country-code "44.8391224692125" "-0.585060840409772")))
    (let ((infos (find :|countryCode| (cdadr data) :test #'equal :key #'car)))
      (lift:ensure (string-equal "FR" (cadr infos))))
    (let ((infos (find :|countryName| (cdadr data) :test #'equal :key #'car)))
      (lift:ensure (string-equal "France" (cadr infos))))))


(lift:addtest (cl-geonames-test)
  country-subdivision-test
  (:documentation "Test retrieve the country and the administrative subdivison
(state, province,...) for the given LATITUDE/LONGITUDE")
  (let ((data (cl-geonames:geo-country-subdivision "44.8391224692125" "-0.585060840409772")))
    (let ((infos (find :|countryCode| (cdadr data) :test #'equal :key #'car)))
      (lift:ensure (string-equal "FR" (cadr infos))))
    (let ((infos (find :|countryName| (cdadr data) :test #'equal :key #'car)))
      (lift:ensure (string-equal "France" (cadr infos))))
    (let ((infos (find :|adminName1| (cdadr data) :test #'equal :key #'car)))
      (lift:ensure (string-equal "Aquitaine" (cadr infos))))))


(lift:addtest (cl-geonames-test)
  srtm3-test
  (:documentation "Test getting the elevation in meters according to srtm3 for
the given LATITUDE/LONGITUDE.")
  (let ((data (cl-geonames:geo-elevation-srtm3 "44.8391224692125" "-0.585060840409772")))
    (lift:ensure (not (null data)))))


(lift:addtest (cl-geonames-test)
  gtopo30-test
  (:documentation "Get the elevation in meters according to gtopo30 for the given LATITUDE/LONGITUDE.")
  (let ((data (cl-geonames:geo-elevation-gtopo30 "44.8391224692125" "-0.585060840409772")))
    (lift:ensure (not (null data)))))


(lift:addtest (cl-geonames-test)
  find-nearby-lat-lng-test
  (:documentation "Retrieve a list of place for the LATITUDE / LONGITUDE query.")
  (let ((data (cl-geonames:geo-find-nearby-place-name "44.8391224692125"
                                                      "-0.585060840409772"
                                                      :radius 10)))
    (lift:ensure (= 10 (length (cdr data))))))


(lift:addtest (cl-geonames-test)
  find-nearby-postal-code-test
  (:documentation "Retrieve a list of place for the postal code query.")
  (let ((data (cl-geonames:geo-find-nearby-postal-code "33560"
                                                       :max-rows 2 :country '("fr"))))
    (lift:ensure (= 2 (length (cdr data))))
    (destructuring-bind (cb ste) (cdr data)
      (let ((infos (find :|name| (cdr cb) :test #'equal :key #'car)))
        (lift:ensure (string-equal "Carbon Blanc" (cadr infos))))
      (let ((infos (find :|name| (cdr ste) :test #'equal :key #'car)))
        (lift:ensure (string-equal "Ste Eulalie" (cadr infos))))
      (let ((infos (find :|adminName1| (cdr ste) :test #'equal :key #'car)))
        (lift:ensure (string-equal "Aquitaine" (cadr infos))))
      (let ((infos (find :|adminName2| (cdr ste) :test #'equal :key #'car)))
        (lift:ensure (string-equal "Gironde" (cadr infos)))))))


(lift:addtest (cl-geonames-test)
  find-nearby-postal-code-geocoding-test
  (:documentation "Retrieve a list of places near latitude and longitude.")
  (let ((data (cl-geonames:geo-find-nearby-postal-code-geocoding "44.8833333" "-0.5"
                                                                 :max-rows 10)))
    (lift:ensure (= 10 (length (cdr data))))
    (loop for place in (cdr data)
          as x = (find :|adminName1| (cdr place) :test #'equal :key #'car)
          as y = (find :|adminName2| (cdr place) :test #'equal :key #'car)
          do
       (lift:ensure (string-equal (cadr x) "Aquitaine"))
       (lift:ensure (string-equal (cadr y) "Gironde")))))


(lift:addtest (cl-geonames-test)
  lookup-places-test
  (:documentation "Test research of places using postal code.")
  (let ((data (cl-geonames:geo-placename-lookup "33560" :country '("FR"))))
    (lift:ensure (= 2 (length (cdar data))))
    (destructuring-bind (cb ste) (cdar data)
      (let ((infos (find :POSTALCODE cb :test #'equal :key #'car)))
        (lift:ensure (string-equal (cdr infos) "33560")))
      (let ((infos (find :PLACE-NAME cb :test #'equal :key #'car)))
        (lift:ensure (string-equal (cdr infos) "Carbon Blanc")))
      (let ((infos (find :PLACE-NAME ste :test #'equal :key #'car)))
        (lift:ensure (string-equal (cdr infos) "Ste Eulalie")))
      (let ((infos (find :POSTALCODE ste :test #'equal :key #'car)))
        (lift:ensure (string-equal (cdr infos) "33560")))
      (let ((infos (find :COUNTRY-CODE cb :test #'equal :key #'car)))
        (lift:ensure (string-equal (cdr infos) "FR")))
      (let ((infos (find :COUNTRY-CODE ste :test #'equal :key #'car)))
        (lift:ensure (string-equal (cdr infos) "FR")))
      (let ((infos (find :LNG cb :test #'equal :key #'car)))
        (lift:ensure (= (cdr infos) -0.5)))
      (let ((infos (find :LNG ste :test #'equal :key #'car)))
        (lift:ensure (= (cdr infos) -0.5)))
      (let ((infos (find :LAT cb :test #'equal :key #'car)))
        (lift:ensure (= (cdr infos) 44.883335)))
      (let ((infos (find :LAT ste :test #'equal :key #'car)))
        (lift:ensure (= (cdr infos) 44.883335))))))


(lift:addtest (cl-geonames-test)
  postal-code-country-info-test
  (:documentation "Test to retrieve countries list for which postal codes are available.")
  (let ((data (cl-geonames:geo-postal-code-country-info)))
    (lift:ensure (< 0 (length (cdr data))))))


(lift:addtest (cl-geonames-test)
  timezone-test
  (:documentation "Test to retrieve timezone for at latitude/longitude place.")
  (let ((data (cl-geonames:geo-timezone "44.8391224692125" "-0.585060840409772")))
    (lift:ensure (= 1 (length (cdr data))))
    (let ((infos (find :|timezoneId| (cdadr data) :test #'equal :key #'car)))
      (lift:ensure (string-equal "Europe/Paris" (cadr infos))))))


(lift:addtest (cl-geonames-test)
  geo-search-test
  (:documentation "Test for the geo-searching query.")
  (let ((data (cl-geonames:geo-search "Bordeaux" "Bordeaux" "Bordeaux" 
                                      :country '("FR") :max-rows 5)))
    (lift:ensure (= 5 (length (cddr data))))))


(lift:addtest (cl-geonames-test)
  postal-code-search-test
  (:documentation "Test for the postal code searching feature.")
  (let ((data (cl-geonames:geo-postal-code-search "33560" "" :country '("FR"))))
    (lift:ensure (= 2 (length (cddr data))))
    (destructuring-bind (cb ste) (cddr data)
      (let ((infos (find :|name| (cdr cb) :test #'equal :key #'car)))
        (lift:ensure (string-equal "Carbon Blanc" (cadr infos))))
      (let ((infos (find :|name| (cdr ste) :test #'equal :key #'car)))
        (lift:ensure (string-equal "Ste Eulalie" (cadr infos)))))))


(lift:addtest (cl-geonames-test)
  output-formats-test
  (:documentation "check available output formats.")
  (lift:ensure-condition 'cl-geonames::geonames-output-error
    (cl-geonames:geo-postal-code-search "33560" "" :country '("FR") :type :foo)))
