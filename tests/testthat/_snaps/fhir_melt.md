# fhir_melt produces correct output

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["resource_identifier", "address.city", "address.country", "address.type", "address.use"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 2, 3, 3, 3]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1]Amsterdam", "[1]Rome", "[1]Stockholm", "[1]Berlin", null, "[1]London"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1]Netherlands", "[1]Italy", "[1]Sweden", null, "[1]France", "[1]England"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1]physical", "[1]physical", "[1]postal", null, "[1]postal", "[1]postal"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1]home", "[1]home", "[1]work", "[1]home", null, "[1]work"]
        }
      ]
    }

