# fhir_crack compact with filtered values and automatic column names produces correct output

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["address[use[@value='home']].city", "address[use[@value='work']].city", "address.use", "address.country"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
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
          "value": ["Amsterdam", "Rome", "Berlin"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "Stockholm", "London"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["home", "home:::work", "home:::work"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Netherlands", "Italy:::Sweden", "France:::England"]
        }
      ]
    }

# fhir_crack compact with filtered values and given column names produces correct output

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["home_city", "work_city", "use", "country"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
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
          "value": ["Amsterdam", "Rome", "Berlin"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "Stockholm", "London"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["home", "home:::work", "home:::work"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Netherlands", "Italy:::Sweden", "France:::England"]
        }
      ]
    }

# fhir_crack compact with filtered values and brackets produces correct output

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["address[use[@value='home']].city", "address[use[@value='work']].city", "address.use", "address.country"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
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
          "value": ["[1.1]Amsterdam", "[1.1]Rome", "[1.1]Berlin"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "[2.1]Stockholm", "[3.1]London"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]home", "[1.1]home:::[2.1]work", "[1.1]home:::[3.1]work"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]Netherlands", "[1.1]Italy:::[2.1]Sweden", "[2.1]France:::[3.1]England"]
        }
      ]
    }

# fhir_crack wide with filtered values and brackets produces correct output

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]address[use[@value='home']].city", "[2.1]address[use[@value='work']].city", "[3.1]address[use[@value='work']].city", "[1.1]address.use", "[2.1]address.use", "[3.1]address.use", "[1.1]address.country", "[2.1]address.country", "[3.1]address.country"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["Amsterdam", "Rome", "Berlin"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "Stockholm", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "London"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["home", "home", "home"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "work", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "work"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Netherlands", "Italy", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "Sweden", "France"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "England"]
        }
      ]
    }

# fhir_crack compact with filtered values and keep_attr produces correct output

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["address.country@value", "address.use@value", "address[use[@value='home']].city@value", "address[use[@value='work']].city@value"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
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
          "value": ["[1.1]Netherlands", "[1.1]Italy:::[2.1]Sweden", "[2.1]France:::[3.1]England"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]home", "[1.1]home:::[2.1]work", "[1.1]home:::[3.1]work"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]Amsterdam", "[1.1]Rome", "[1.1]Berlin"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "[2.1]Stockholm", "[3.1]London"]
        }
      ]
    }

# fhir_crack compact with filtered values produces correct output

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["address[use[@value='home']].city", "address[use[@value='work']].city", "address.use", "address.country"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
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
          "value": ["Amsterdam", "Rome", "Berlin"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "Stockholm", "London"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["home", "home:::work", "home:::work"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Netherlands", "Italy:::Sweden", "France:::England"]
        }
      ]
    }

# fhir_crack compact all columns produces correct output

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["address.city", "address.country", "address.type", "address.use", "id", "name.given"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
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
          "value": ["Amsterdam", "Rome:::Stockholm", "Berlin:::London"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Netherlands", "Italy:::Sweden", "France:::England"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["physical", "physical:::postal", "postal:::postal"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["home", "home:::work", "home:::work"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["id1", "id2", "id3"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Marie", "Susie", "Frank:::Max"]
        }
      ]
    }

# fhir_crack compact given columns produces correct output

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "name.given", "address.city"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
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
          "value": ["id1", "id2", "id3"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Marie", "Susie", "Frank:::Max"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Amsterdam", "Rome:::Stockholm", "Berlin:::London"]
        }
      ]
    }

# fhir_crack wide given columns produces correct output

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["[1]id", "[1.1]name.given", "[2.1]name.given", "[1.1]address.city", "[2.1]address.city", "[3.1]address.city"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["id1", "id2", "id3"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Marie", "Susie", "Frank"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "Max"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Amsterdam", "Rome", "Berlin"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "Stockholm", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "London"]
        }
      ]
    }

# fhir_crack wide all columns produces correct output

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["[1.1]address.city", "[2.1]address.city", "[3.1]address.city", "[1.1]address.country", "[2.1]address.country", "[3.1]address.country", "[1.1]address.type", "[2.1]address.type", "[3.1]address.type", "[1.1]address.use", "[2.1]address.use", "[3.1]address.use", "[1]id", "[1.1]name.given", "[2.1]name.given"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["Amsterdam", "Rome", "Berlin"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "Stockholm", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "London"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Netherlands", "Italy", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "Sweden", "France"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "England"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["physical", "physical", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "postal", "postal"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "postal"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["home", "home", "home"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "work", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "work"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["id1", "id2", "id3"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Marie", "Susie", "Frank"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "Max"]
        }
      ]
    }

# fhir_crack produces correct output with two tables

    {
      "type": "S4",
      "attributes": {
        ".Data": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "list",
              "attributes": {
                "row.names": {
                  "type": "integer",
                  "attributes": {},
                  "value": [1, 2, 3]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["data.frame"]
                },
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["[1.1]address.city", "[2.1]address.city", "[3.1]address.city", "[1.1]address.country", "[2.1]address.country", "[3.1]address.country", "[1.1]address.type", "[2.1]address.type", "[3.1]address.type", "[1.1]address.use", "[2.1]address.use", "[3.1]address.use", "[1]id", "[1.1]name.given", "[2.1]name.given"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Amsterdam", "Rome", "Berlin"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [null, "Stockholm", null]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [null, null, "London"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Netherlands", "Italy", null]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [null, "Sweden", "France"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [null, null, "England"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["physical", "physical", null]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [null, "postal", "postal"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [null, null, "postal"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["home", "home", "home"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [null, "work", null]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [null, null, "work"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["id1", "id2", "id3"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Marie", "Susie", "Frank"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": [null, null, "Max"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["code.coding.code", "code.coding.display", "code.coding.system", "id", "subject.reference"]
                },
                "row.names": {
                  "type": "integer",
                  "attributes": {},
                  "value": [1]
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
                  "value": ["29463-7:::27113001"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Body Weight:::Body weight"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["http://loinc.org:::http://snomed.info/sct"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["obs1"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Patient/id2"]
                }
              ]
            }
          ]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["Patients", "Observations"]
        },
        "design": {
          "type": "S4",
          "attributes": {
            ".Data": {
              "type": "list",
              "attributes": {},
              "value": [
                {
                  "type": "S4",
                  "attributes": {
                    "resource": {
                      "type": "S4",
                      "attributes": {
                        ".Data": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Patient"]
                        }
                      },
                      "value": {
                        "class": "fhir_resource_type",
                        "package": "fhircrackr"
                      }
                    },
                    "cols": {
                      "type": "S4",
                      "attributes": {
                        ".Data": {
                          "type": "character",
                          "attributes": {},
                          "value": []
                        },
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": []
                        }
                      },
                      "value": {
                        "class": "fhir_columns",
                        "package": "fhircrackr"
                      }
                    },
                    "sep": {
                      "type": "character",
                      "attributes": {},
                      "value": [":::"]
                    },
                    "brackets": {
                      "type": "character",
                      "attributes": {},
                      "value": ["[", "]"]
                    },
                    "rm_empty_cols": {
                      "type": "logical",
                      "attributes": {},
                      "value": [false]
                    },
                    "format": {
                      "type": "character",
                      "attributes": {},
                      "value": ["wide"]
                    },
                    "keep_attr": {
                      "type": "logical",
                      "attributes": {},
                      "value": [false]
                    }
                  },
                  "value": {
                    "class": "fhir_table_description",
                    "package": "fhircrackr"
                  }
                },
                {
                  "type": "S4",
                  "attributes": {
                    "resource": {
                      "type": "S4",
                      "attributes": {
                        ".Data": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Observation"]
                        }
                      },
                      "value": {
                        "class": "fhir_resource_type",
                        "package": "fhircrackr"
                      }
                    },
                    "cols": {
                      "type": "S4",
                      "attributes": {
                        ".Data": {
                          "type": "character",
                          "attributes": {},
                          "value": []
                        },
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": []
                        }
                      },
                      "value": {
                        "class": "fhir_columns",
                        "package": "fhircrackr"
                      }
                    },
                    "sep": {
                      "type": "character",
                      "attributes": {},
                      "value": [":::"]
                    },
                    "brackets": {
                      "type": "character",
                      "attributes": {},
                      "value": []
                    },
                    "rm_empty_cols": {
                      "type": "logical",
                      "attributes": {},
                      "value": [false]
                    },
                    "format": {
                      "type": "character",
                      "attributes": {},
                      "value": ["compact"]
                    },
                    "keep_attr": {
                      "type": "logical",
                      "attributes": {},
                      "value": [false]
                    }
                  },
                  "value": {
                    "class": "fhir_table_description",
                    "package": "fhircrackr"
                  }
                }
              ]
            },
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["Patients", "Observations"]
            }
          },
          "value": {
            "class": "fhir_design",
            "package": "fhircrackr"
          }
        }
      },
      "value": {
        "class": "fhir_df_list",
        "package": "fhircrackr"
      }
    }

