(push "../utils/cl-json_0.4.0/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cl-json)

(push "../utils/s-xml/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :s-xml)

(push "../utils/split-sequence/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :split-sequence)

(push "../utils/usocket-0.4.1/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :usocket)

(push "../utils/trivial-gray-streams-2008-11-02/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :trivial-gray-streams)

(push "../utils/flexi-streams-1.0.7/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :flexi-streams)

(push "../utils/chunga-1.1.1/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :chunga)

(push "../utils/cl-base64-3.3.3/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cl-base64)

(push "../utils/puri-1.5.5/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :puri)

(push "../utils/drakma-1.2.3/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :drakma)

(push "../utils/cl-geonames/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cl-geonames)

(cl-geonames:geo-country-info :country '("FR" "GB" "ES"))
