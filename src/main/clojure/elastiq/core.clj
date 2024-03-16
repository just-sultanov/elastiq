(ns elastiq.core
  (:require
    [clojure.java.io :as io]
    [jsonista.core :as json]
    [malli.core :as m]
    [malli.error :as me]
    [malli.transform :as mt]))


(def transformer
  (mt/transformer
    {:name :query-builder}))


(defn read-mappings
  [filename]
  (some-> filename
          (io/resource)
          (slurp)
          (json/read-value json/keyword-keys-object-mapper)))


(defn mappings->properties
  [index-name mappings]
  (get-in mappings [index-name :mappings :doc :properties]))


(defn searchable?
  [{:keys [index enabled]
    :or {index true
         enabled true}}]
  (and index enabled))


(defn as-search-field
  [parent-field field]
  (if parent-field
    (keyword (format "%s.%s" (name parent-field) (name field)))
    field))


;;
;; Queries
;;


(defn term-query
  [{:search/keys [field]} v]
  {:term {field v}})


(defn match-query
  [{:search/keys [field metadata]} v]
  (let [{:keys [analyzer]} metadata]
    (cond-> {:match {field {:query v}}}
      analyzer (assoc-in [:match field :analyzer] analyzer))))


(defn range-query
  [{:search/keys [field]} {:keys [from to]}]
  {:range {field {:gte from, :lte to}}})


;;
;; Query builders
;;


(def boolean-query-builder
  {:compile
   (fn boolean-query-builder
     [schema _]
     (let [props (m/properties schema)]
       {:enter
        (fn [x]
          (cond
            (boolean? x) (term-query props x)
            (sequential? x) (mapv (partial term-query props) x)
            :else (throw
                    (ex-info
                      "Unsupported type"
                      {:type (type x)
                       :value x
                       :expected ["boolean" "collection"]
                       :problems (me/humanize (m/explain schema x))}))))}))})


(def keyword-query-builder
  {:compile
   (fn keyword-query-builder
     [schema _]
     (let [props (m/properties schema)]
       {:enter
        (fn [x]
          (cond
            (string? x) (match-query props x)
            (sequential? x) (mapv (partial match-query props) x)
            :else
            (throw
              (ex-info
                "Unsupported type"
                {:type (type x)
                 :value x
                 :expected ["string" "collection"]
                 :problems (me/humanize (m/explain schema x))}))))}))})


(def date-query-builder
  {:compile
   (fn date-query-builder
     [schema _]
     (let [props (m/properties schema)]
       {:enter
        (fn [x]
          (cond
            (map? x) (range-query props x)
            (sequential? x) (mapv (partial range-query props) x)
            :else (throw
                    (ex-info
                      "Unsupported type"
                      {:type (type x)
                       :value x
                       :expected ["map" "collection"]
                       :problems (me/humanize (m/explain schema x))}))))}))})


(def map-query-builder
  {:compile
   (fn map-query-builder
     [schema _]
     {:leave
      (fn [x]
        (if (map? x)
          (let [queries (reduce-kv
                          (fn [acc _field query]
                            (cond
                              (map? query) (conj acc query)
                              (sequential? query) (into acc query)
                              :else acc))
                          [] x)]
            (when (seq queries)
              {:bool {:must queries}}))
          (throw
            (ex-info
              "Unsupported type"
              {:type (type x)
               :value x
               :expected ["map"]
               :problems (me/humanize (m/explain schema x))}))))})})


;;
;; Schema builders
;;

(defmulti entry
  (fn [{:search/keys [metadata]}]
    (case (:type metadata)
      ("keyword" "text") :keyword
      "boolean" :boolean
      "date" :date
      :unsupported)))


(defmethod entry :unsupported
  [{:search/keys [metadata]}]
  (throw
    (ex-info
      "Unsupported type"
      {:type (:type metadata)
       :expected ["keyword" "text" "boolean" "date"]})))


(defmethod entry :boolean
  [{:search/keys [field metadata]}]
  [field
   {:search/field field
    :search/metadata metadata
    :decode/query-builder boolean-query-builder}
   [:or :boolean [:sequential {:min 1} :boolean]]])


(defmethod entry :keyword
  [{:search/keys [field metadata]}]
  (let [schema [:string {:min 1}]]
    [field
     {:search/field field
      :search/metadata metadata
      :decode/query-builder keyword-query-builder}
     [:or schema [:sequential {:min 1} schema]]]))


(defmethod entry :date
  [{:search/keys [field metadata]}]
  (let [schema [:map
                [:from [:string {:min 1}]]
                [:to [:string {:min 1}]]]]
    [field
     {:search/field field
      :search/metadata metadata
      :decode/query-builder date-query-builder}
     [:or schema [:sequential {:min 1} schema]]]))


(defn schema
  [properties]
  (let [entries (reduce-kv
                  (fn [acc field metadata]
                    (if-not (searchable? metadata)
                      acc
                      (conj acc (entry {:search/field field, :search/metadata metadata}))))
                  [] properties)]
    (when (seq entries)
      (into [:map {:decode/query-builder map-query-builder}]
            entries))))
