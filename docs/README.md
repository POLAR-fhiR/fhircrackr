# fhiR
fhiR is a package for convenient downloading fhir resources in xml format and converting to R data frames.

## Set of commands:

### download a single fhir bundle
- ```download.bundle( fhir.search.request, max.attempts = 5 )```  

  Actually one doesn't use this one.  
  Use download.bundles for downloading all bundles at once!  

### download all fhir bundle at once
- ```download.bundles( fhir.search.request, max.attempts = 5 )```  

  e.g.
  ```
  bundles <- download.bundles(
    "https://hapi.fhir.org/baseR4/MedicationStatement?
    _include=MedicationStatement:context&
    _include=MedicationStatement:subject&
    _format=xml" )
  ```  

### write all bundles as xml files to directory
- ```write.bundles( bundles, directory )```  

  e.g. ```write.bundles( bundles, "bundle-medication-statement" )```

### read bundles as xml files from directory
- ```read.bundles( directory )```  

  e.g. ``` bundles.bak <- read.bundles( "bundle-medication-statement" )```

### extract a single bundle to data frames
- ```bundle.to.dataframes( bundle, design )```  

  Actually one doesn't use this one.  
  Use bundles.to.dataframes( bundles, design ) for extracting a all bundles of a fhir search request to data frames!  

### extract all bundles to data frames at once
- ```bundles.to.dataframes( bundles, design )```

  e.g. create 3 data frames with a set of items of interest  
  ```
  design <- list(
    Medication = list(
        ".//MedicationStatement",
        list(
            AID                   = "id/@value",
            STATUS                = "status/@value",
            MEDICATION.SYSTEM     = "medicationCodeableConcept/coding/system/@value",
            MEDICATION.CODE       = "medicationCodeableConcept/coding/code/@value",
            MEDICATION.DIPLAY     = "medicationCodeableConcept/coding/display/@value",
            PATIENT               = "subject/reference/@value",
            ENCOUNTER             = "context/reference/@value",
            START                 = "effectivePeriod/start/@value",
            END                   = "effectivePeriod/end/@value",
            DATE                  = "dateAsserted/@value"
        )
	),
	Encounters = list(
		".//Encounter",
		list(
			EID            = "id/@value",
			PATIENTS.ID    = "subject/reference/@value",
			PARTICIPANT.ID = "participant/individual/reference/@value",
			START          = "period/start/@value",
			END            = "period/end/@value",
			SYSTEM         = "class/system/@value",
			CODE           = "class/code/@value",
			DISPLAY        = "class/display/@value"
		)
	),
	Patients = list(
		".//Patient",
		list(
			PID         = "id/@value",
			NAME.USE    = "name/use/@value",
			NAME.GIVEN  = "name/given/@value",
			NAME.FAMILY = "name/family/@value",
			SEX         = "gender/@value",
			BIRTHDATE   = "birthDate/@value"
        )
     )
  )

  dfs <- bundles.to.dataframes( bundles, design )
  ```


### extract an attribute from bundle tags
- ```tag.attr( bundle, xpath )```

  e.g. how many medication statements are available?
  ```
  tag.attr(
    download.bundle( "https://hapi.fhir.org/baseR4/MedicationStatement/?_summary=count&_format=xml" ),
    "total/@value" )

  # or

  tag.attr(
    download.bundles( "https://hapi.fhir.org/baseR4/MedicationStatement/?_summary=count&_format=xml" )[[ 1 ]],
    "total/@value" )
  ```

  tag.attr gives a vector of all available values. missings aren't in the results.
  so never use it in this way!

  ```
  b <- fhiR::download.bundle( "https://vonk.fire.ly/R4/Patient?_format=xml" )

  data.frame(
    vname = fhiR::tag.attr( b, ".//name/given/@value" ),
    nname = fhiR::tag.attr( b, ".//name/family/@value" )
  )
  ```
