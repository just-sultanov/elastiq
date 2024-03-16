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


(deftest ^:unit builder-test
  (let [properties (->> (sut/read-mappings "mappings.json")
                        (sut/mappings->properties :index-name))]
    (testing "term query"
      (testing "boolean type"
        (let [schema (sut/schema (select-keys properties [:booleanObject]))]
          (testing "invalid value"
            (is (false? (m/validate schema {:booleanObject 42})))
            (is (false? (m/validate schema {:booleanObject ["opened" 42]}))))

          (testing "boolean value"
            (is (true? (m/validate schema {:booleanObject true})))
            (is (= {:bool {:must [{:term {:booleanObject true}}]}}
                   (m/decode
                     schema
                     {:booleanObject true}
                     sut/transformer))))

          (testing "collection of booleans"
            (is (true? (m/validate schema {:booleanObject [true false]})))
            (is (= {:bool {:must [{:term {:booleanObject true}}
                                  {:term {:booleanObject false}}]}}
                   (m/decode
                     schema
                     {:booleanObject [true false]}
                     sut/transformer)))))))


    (testing "match query"
      (testing "keyword type"
        (let [schema (sut/schema (select-keys properties [:keywordWithAnalyzer]))]
          (testing "invalid value"
            (doseq [x [42
                       ["opened" 42]]]
              (is (false? (m/validate schema {:keywordWithAnalyzer x})))))

          (testing "string value"
            (is (true? (m/validate schema {:keywordWithAnalyzer "opened"})))
            (is (= {:bool {:must [{:match {:keywordWithAnalyzer {:query "opened"
                                                                 :analyzer "some_keyword_analyzer"}}}]}}
                   (m/decode
                     schema
                     {:keywordWithAnalyzer "opened"}
                     sut/transformer))))

          (testing "collection of strings"
            (is (true? (m/validate schema {:keywordWithAnalyzer ["opened" "closed"]})))
            (is (= {:bool {:must [{:match {:keywordWithAnalyzer {:query "opened"
                                                                 :analyzer "some_keyword_analyzer"}}}
                                  {:match {:keywordWithAnalyzer {:query "closed"
                                                                 :analyzer "some_keyword_analyzer"}}}]}}
                   (m/decode
                     schema
                     {:keywordWithAnalyzer ["opened" "closed"]}
                     sut/transformer)))))))


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
            (is (false? (m/validate schema {:dateObject x})))))

        (testing "map value"
          (is (true? (m/validate schema {:dateObject {:from "now-365d", :to "now"}})))
          (is (= {:bool {:must [{:range {:dateObject {:gte "now-365d", :lte "now"}}}]}}
                 (m/decode
                   schema
                   {:dateObject {:from "now-365d", :to "now"}}
                   sut/transformer))))

        (testing "collection of maps"
          (is (true? (m/validate schema {:dateObject [{:from "2023-01-01", :to "2023-01-31"}
                                                      {:from "2024-01-01", :to "2024-01-31"}]})))
          (is (= {:bool {:must [{:range {:dateObject {:gte "2023-01-01", :lte "2023-01-31"}}}
                                {:range {:dateObject {:gte "2024-01-01", :lte "2024-01-31"}}}]}}
                 (m/decode
                   schema
                   {:dateObject [{:from "2023-01-01", :to "2023-01-31"}
                                 {:from "2024-01-01", :to "2024-01-31"}]}
                   sut/transformer))))))))
