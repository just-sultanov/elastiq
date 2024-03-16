# elastiq

An experimental library for building queries to the Elastic (OpenSearch) Search
API.

## How it works

- Provide mappings of your `index`
- Automatically create a `malli` schema using mappings of your `index`
- Validate a user request
- Transform a user request into a request to the Elastic (OpenSearch) Search API
  using the `malli` scheme created above

**User request**

```jsonc
{
  "from": 0,
  "size": 100,
  "query": {
    "article": { "content": "clojure" }, // `content` contains "clojure"
    "tags": ["clojure", "clojurescript"], // `tag` is equal to "clojure" or "clojurescript"
    "author": "*", // `author` is not `empty` or `null`
    "public": true, // only `public` articles
    "published_at": { "from": "2024-01-01", "to": "now" } // articles for the specified period
  }
}
```

**Generated query to the Elastic (OpenSearch) Search API**

```clojure
{:from 0,
 :size 100,
 :query {:bool 
          {:must [{:nested {:path :article
                            :query {:bool {:must [{:match {:article.content "clojure"}}]}}}}
                  {:bool {:minimum_should_match 1
                          :should [{:match {:tags {:query "clojure"}}}
                                   {:match {:tags {:query "clojurescript"}}}]}}
                  {:exists {:field :author}}
                  {:term {:public true}}
                  {:range {:published_at {:gte "2024-01-01", :lte "now"}}}]}}}
```

**Mappings**

```json
{
  "posts": {
    "mappings": {
      "doc": {
        "_meta": {
          "applicationVersion": "14.15"
        },
        "properties": {
          "article": {
            "type": "nested",
            "properties": {
              "title": {
                "type": "keyword"
              },
              "content": {
                "type": "text",
                "analyzer": "some_text_analyzer"
              }
            }
          },
          "tags": {
            "type": "keyword"
          },
          "author": {
            "type": "keyword"
          },
          "public": {
            "type": "boolean"
          },
          "published_at": {
            "type": "date"
          }
        }
      }
    }
  }
}
```

## License

[Copyright © 2024 Ilshat Sultanov](./license)
