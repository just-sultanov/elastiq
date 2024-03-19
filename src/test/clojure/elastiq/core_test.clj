(ns elastiq.core-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [elastiq.core :as sut]
    [malli.core :as m]))


;; {:from 0
;;  :size 100
;;  :query {;; keyword
;;          :keywordWithAnalyzer "foo"
;;          ;; :keywordWithAnalyzer ["foo" "bar"]
;;          ;; :keywordWithAnalyzer "*"
;;
;;          :dateObject {:from "now-365d" :to "now"}
;;          ;; :dateObject [{:from "2023-01-01" :to "2023-01-31"}
;;          ;;              {:from "2024-01-01" :to "2024-01-31"}]
;;
;;          :nestedObject {:fieldOne "foo"}
;;          ;; :nestedObject [{:fieldOne "foo"}
;;          ;;                {:fieldOne "bar"}]
;;          }}

(defn transform
  [schema value]
  (m/decode schema value sut/transformer))


(defn valid?
  [schema value]
  (m/validate schema value))


(deftest ^:unit builder-test
  (let [properties (->> (sut/read-mappings "mappings.json")
                        (sut/mappings->properties :index-name))]
    (testing "exists query"
      (let [schema (sut/schema (select-keys properties [:textWithAnalyzer]))]

        (testing "invalid value"
          (doseq [x [42 [42 42]]]
            (is (not (valid? schema {:textWithAnalyzer x})))))

        (testing "wildcard value"
          (is (valid? schema {:textWithAnalyzer "*"}))
          (is (= {:bool {:must [{:exists {:field :textWithAnalyzer}}]}}
                 (transform schema {:textWithAnalyzer "*"}))))))


    (testing "term query"
      (testing "boolean type"
        (let [schema (sut/schema (select-keys properties [:booleanObject]))]
          (testing "invalid value"
            (doseq [x [42 ["foo" 42]]]
              (is (not (valid? schema {:booleanObject x})))))

          (testing "boolean value"
            (is (valid? schema {:booleanObject true}))
            (is (= {:bool {:must [{:term {:booleanObject true}}]}}
                   (transform schema {:booleanObject true}))))

          (testing "collection of booleans"
            (is (valid? schema {:booleanObject [true false]}))
            (is (= {:bool {:must [{:bool {:minimum_should_match 1
                                          :should [{:term {:booleanObject true}}
                                                   {:term {:booleanObject false}}]}}]}}
                   (transform schema {:booleanObject [true false]})))))))


    (testing "match query"
      (testing "keyword type"
        (let [schema (sut/schema (select-keys properties [:keywordWithAnalyzer]))]
          (testing "invalid value"
            (doseq [x [42 ["foo" 42]]]
              (is (not (valid? schema {:keywordWithAnalyzer x})))))

          (testing "string value"
            (is (valid? schema {:keywordWithAnalyzer "foo"}))
            (is (= {:bool {:must [{:match {:keywordWithAnalyzer {:query "foo"
                                                                 :analyzer "some_keyword_analyzer"}}}]}}
                   (transform schema {:keywordWithAnalyzer "foo"}))))

          (testing "collection of strings"
            (is (valid? schema {:keywordWithAnalyzer ["foo" "bar"]}))
            (is (= {:bool {:must [{:bool {:minimum_should_match 1
                                          :should [{:match {:keywordWithAnalyzer {:query "foo" :analyzer "some_keyword_analyzer"}}}
                                                   {:match {:keywordWithAnalyzer {:query "bar" :analyzer "some_keyword_analyzer"}}}]}}]}}
                   (transform schema {:keywordWithAnalyzer ["foo" "bar"]})))))))


    (testing "date query"
      (let [schema (sut/schema (select-keys properties [:dateObject]))]
        (testing "invalid value"
          (doseq [x [42
                     []
                     ["foo" 42]
                     {}
                     {:from "now-365d"}
                     {:to "now-365d"}
                     {:from "" :to ""}
                     {:from 42 :to 42}]]
            (is (not (valid? schema {:dateObject x})))))

        (testing "map value"
          (is (valid? schema {:dateObject {:from "now-365d", :to "now"}}))
          (is (= {:bool {:must [{:range {:dateObject {:gte "now-365d", :lte "now"}}}]}}
                 (transform schema {:dateObject {:from "now-365d", :to "now"}}))))

        (testing "collection of maps"
          (is (valid? schema {:dateObject [{:from "2023-01-01", :to "2023-01-31"}
                                           {:from "2024-01-01", :to "2024-01-31"}]}))
          (is (= {:bool {:must [{:bool {:minimum_should_match 1
                                        :should [{:range {:dateObject {:gte "2023-01-01", :lte "2023-01-31"}}}
                                                 {:range {:dateObject {:gte "2024-01-01", :lte "2024-01-31"}}}]}}]}}
                 (transform schema {:dateObject [{:from "2023-01-01", :to "2023-01-31"}
                                                 {:from "2024-01-01", :to "2024-01-31"}]}))))))

    (testing "nested query"
      (let [schema (sut/schema (select-keys properties [:nestedObject]))]
        (testing "invalid value"
          (doseq [x [42
                     []
                     ["foo" 42]
                     {}
                     {:fieldOne 42}]]
            (is (not (valid? schema {:nestedObject x})))))

        (testing "map value"
          (doseq [x ["*"
                     [{:fieldOne "foo"} {:fieldOne "bar"}]
                     {:fieldOne "*"}
                     {:fieldOne ["foo" "bar"]}]]
            (is (valid? schema {:nestedObject x})))

          (testing "exists"
            (is (= {:bool {:must [{:exists {:field :nestedObject}}]}}
                   (transform schema {:nestedObject "*"})))
            (is (= {:bool {:must [{:nested {:path :nestedObject
                                            :query {:bool {:must [{:exists {:field :nestedObject.fieldOne}}]}}}}]}}
                   (transform schema {:nestedObject {:fieldOne "*"}}))))

          (testing "keyword"
            (is (= {:bool {:must [{:nested {:path :nestedObject
                                            :query {:bool {:must [{:match {:nestedObject.fieldOne {:query "foo"
                                                                                                   :analyzer "some_text_analyzer"}}}]}}}}]}}
                   (transform schema {:nestedObject {:fieldOne "foo"}}))))

          (testing "collection of keywords"
            (is (= {:bool {:must [{:nested {:path :nestedObject
                                            :query {:bool {:must [{:bool {:minimum_should_match 1
                                                                          :should [{:match {:nestedObject.fieldOne {:query "foo" :analyzer "some_text_analyzer"}}}
                                                                                   {:match {:nestedObject.fieldOne {:query "bar" :analyzer "some_text_analyzer"}}}]}}]}}}}]}}
                   (transform schema {:nestedObject {:fieldOne ["foo" "bar"]}})))))

        (testing "collection of maps"
          (is (valid? schema {:nestedObject [{:fieldOne "foo"} {:fieldOne "bar"}]}))
          #_(is (= {:bool {:must [{:nested {:path :nestedObject
                                          :query {:bool {:must [{:bool {:minimum_should_match 1
                                                                        :should [[{:match {:nestedObject.fieldOne {:query "foo" :analyzer "some_text_analyzer"}}}
                                                                                  {:match {:nestedObject.fieldOne {:query "bar" :analyzer "some_text_analyzer"}}}]]}}]}}}}]}}
                 (transform schema {:nestedObject [{:fieldOne "foo"}
                                                   {:fieldOne "bar"}]}))))))))


(comment
  (def properties
    (->> (sut/read-mappings "mappings.json")
         (sut/mappings->properties :index-name)))

 (def schema
   (sut/schema (select-keys properties [:nestedObject])))

 (try
   (transform schema {:nestedObject [{:fieldOne "foo"}]})
   (catch Exception e
     {:message (ex-message e)
      :data (ex-data e)})))
