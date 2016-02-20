(ns raspispy.core
  (:require [cljs.nodejs :as nodejs]))

(defonce noble (nodejs/require "noble"))

(nodejs/enable-util-print!)

(defn dispatch [f]
  (fn [& args] (apply f args)))

(def known-beacons
  {"ec00fb52-740a-40f6-aa96-2d4d8120c567" :hsl})

(defn get-state []
  (keyword (.-state noble)))

(defn ibeacon? [data]
  (= (.-length data) 25))


(def hex-chars "0123456789abcdef")

(defn byte-to-hex [b]
  (let [low (mod b 16)
        high (/ (- b low) 16)]
    [(nth hex-chars high)
     (nth hex-chars low)]))

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


(defn on-discover [peripheral]
  (let [advertisement (js->clj (.-advertisement peripheral) :keywordize-keys true)
        tx-power-level (:txPowerLevel advertisement)
        service-uuids (:serviceUuids advertisement)
        manufacturer-data (:manufacturerData advertisement)
        rssi (.-rssi peripheral)]
    (when (and manufacturer-data
               (ibeacon? manufacturer-data))
      (let [uuid (parse-uuid manufacturer-data)
            tx-power (.readInt8 manufacturer-data 24)]
        (println "Discovered uuid" uuid "tx-power" tx-power "rssi" rssi (name (get known-beacons uuid :unknown)))))))

(defn start-discovery! []
  (println "Starting discovery!")
  (.on noble "discover" (dispatch #'on-discover)))

(defn on-scan-start []
  (start-discovery!))

(defn start-scanning! []
  (println "Starting scanning!")
  (.on noble "scanStart" on-scan-start)
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
  (.on noble "stateChange" on-state-change))

(defn -main []
  (spy))

(set! *main-cli-fn* -main)

