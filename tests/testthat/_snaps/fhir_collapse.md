# fhir_collapse works on address.line

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["address.city", "address.line", "address.use", "id"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]London|[2.1]London", "[1.1]Paris|[2.1]Paris"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]Example Street 1 c/o Baker [2.1]Some firm Some Department Some Lane 1", "[1.1]Rue example 3 [2.1]La fabrique Avenue 33"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]home|[2.1]work", "[1.1]home|[2.1]work"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1]id1", "[1]id2"]
        }
      ]
    }

# fhir_collapse works on name.given

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "name.family", "name.given", "name.use"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["[1]id1", "[1]id2"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]Smith|[2.1]Baker", "[1.1]Brown|[2.1]Jones"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]Marie Luise [2.1]Lea Sophie Anna", "[1.1]Max [2.1]Anton John"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]official|[2.1]nickname", "[1.1]official|[2.1]nickname"]
        }
      ]
    }

