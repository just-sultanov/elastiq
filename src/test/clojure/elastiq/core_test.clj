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
            (is (= {:bool {:must [{:term {:booleanObject true}}
                                  {:term {:booleanObject false}}]}}
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
            (is (= {:bool {:must [{:match {:keywordWithAnalyzer {:query "foo"
                                                                 :analyzer "some_keyword_analyzer"}}}
                                  {:match {:keywordWithAnalyzer {:query "bar"
                                                                 :analyzer "some_keyword_analyzer"}}}]}}
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
          (is (= {:bool {:must [{:range {:dateObject {:gte "2023-01-01", :lte "2023-01-31"}}}
                                {:range {:dateObject {:gte "2024-01-01", :lte "2024-01-31"}}}]}}
                 (transform schema {:dateObject [{:from "2023-01-01", :to "2023-01-31"}
                                                 {:from "2024-01-01", :to "2024-01-31"}]}))))))))
