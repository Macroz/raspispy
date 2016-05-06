(ns raspispy.core
  (:require [cljs.nodejs :as nodejs]))

(defonce noble (nodejs/require "noble"))
(defonce mqtt (nodejs/require "mqtt"))

(nodejs/enable-util-print!)



(def app-state (atom {}))




;; utility functions

(defn dispatch [f] (fn [& args] (apply f args)))

(def hex-chars "0123456789abcdef")

(defn byte-to-hex [b]
  (let [low (mod b 16)
        high (/ (- b low) 16)]
    [(nth hex-chars high)
     (nth hex-chars low)]))



;; iBeacon functions

(defn get-state []
  (keyword (.-state noble)))

(defn ibeacon? [data]
  (= (.-length data) 25))

(defn parse-uuid [buffer]
  (let [chars (map #(aget buffer %) (range 4 20))
        uuid (vec (mapcat byte-to-hex chars))
        parts [(subvec uuid 0 8)
               (subvec uuid 8 12)
               (subvec uuid 12 16)
               (subvec uuid 16 20)
               (subvec uuid 20 32)]]
    (->> parts
         (interpose ["-"])
         (apply concat)
         (apply str))))



;; event handlers

(declare publish!)

(defn on-discover [peripheral]
  (let [advertisement (.-advertisement peripheral)
        manufacturer-data (.-manufacturerData advertisement)
        rssi (.-rssi peripheral)]
    (when (and manufacturer-data
               (ibeacon? manufacturer-data))
      (let [uuid (parse-uuid manufacturer-data)
            tx-power (.readInt8 manufacturer-data 24)]
        ;;(println "Discovered uuid" uuid "tx-power" tx-power "rssi" rssi)
        (when (:connected? @app-state)
          (publish! {:uuid uuid :tx-power tx-power :rssi rssi}))))))

(defn start-discovery! []
  (println "Starting discovery!")
  (.on noble "discover" (dispatch #'on-discover)))

(defn on-scan-start []
  (start-discovery!))

(defn start-scanning! []
  (println "Starting scanning!")
  (.on noble "scanStart" (dispatch #'on-scan-start))
  (.startScanning noble (clj->js []) true))

(defn stop-scanning! []
  (println "Stopping scanning!")
  (.stopScanning noble))

(defn on-state-change []
  (if (= (get-state) :poweredOn)
    (start-scanning!)
    (stop-scanning!)))

(defn spy []
  (println "Starting spying...")
  (.on noble "stateChange" (dispatch #'on-state-change)))



;; delivery

(defn publish! [msg]
  (let [client (:client @app-state)]
    (.publish client "beacon" (pr-str msg))))

(defn on-message [topic msg]
  (println "Message" (.toString msg)))

(defn handle-connect! []
  (swap! app-state assoc :connected? true)
  (let [client (:client @app-state)]
    (.subscribe client "beacon")
    ))

(defn handle-disconnect! []
  (swap! app-state dissoc :connected?))

(defn on-connect []
  (println "Connected to MQTT")
  (handle-connect!))

(defn on-reconnect []
  (handle-disconnect!)
  (println "Reconnected to MQTT")
  (handle-connect!))

(defn on-disconnect []
  (println "Disconnected from MQTT")
  (handle-disconnect!))

(defn on-error [& args]
  (println "Error in MQTT" args))

(defn deliver []
  (let [client (.connect mqtt "ws://locutus.rontu.net:8080/mqtt")]
    (swap! app-state assoc :client client)
    (.on client "message" (dispatch #'on-message))
    (.on client "connect" (dispatch #'on-connect))
    (.on client "reconnect" (dispatch #'on-reconnect))
    (.on client "close" (dispatch #'on-disconnect))
    (.on client "error" (dispatch #'on-error))))



;; main

(defn -main []
  (deliver)
  (spy))

(set! *main-cli-fn* -main)

