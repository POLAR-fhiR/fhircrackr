# fhiR
fhiR is a package for convenient downloading fhir resources in xml format and converting to R data frames.

## Set of commands:

### download a single fhir bundle page
- ```download.page( fhir.search.request, max.attempts = 5 )```  

  Actually one doesn't use this one.  
  Use download.bundle for downloading a complete bundle!  

### download a complete fhir bundle
- ```download.bundle( fhir.search.request, max.attempts = 5 )```  

  e.g.
  ```
  bundle <- download.bundle(
    "https://hapi.fhir.org/baseR4/MedicationStatement?
    _include=MedicationStatement:context&
    _include=MedicationStatement:subject&
    _format=xml&" )
  ```  

### write bundle pages as xml files to directory
- ```write.bundle( bundle, directory )```  

  e.g. ```write.bundle( bundle, "bundle-medication-statement" )```

### read bundle pages as xml files from directory
- ```read.bundle( directory )```  

  e.g. ``` bundle.bak <- read.bundle( "bundle-medication-statement" )```

### extract a single bundle page to data frames
- ```page.to.dataframes( page, design )```  

  Actually one doesn't use this one.  
  Use bundle.to.dataframes( bundle, design ) for extracting a complete bundle to data frames!  

### extract a complete bundle to data frames
- ```bundle.to.dataframes( bundle, design )```

  e.g.  
  ```
  design <- list(
    Medication = list(
        ".//MedicationStatement",
        list(
            AID                   = "id/@value",
            STATUS                = "status/@value",
            STATUS.REASON.SYSTEM  = "statusReason/coding/system/@value",
            STATUS.REASON.CODE    = "statusReason/coding/code/@value",
            STATUS.REASON.DISPLAY = "statusReason/coding/display/@value",
            REASON.CODE.SYSTEM    = "reasonCode/coding/system/@value",
            REASON.CODE.VALUE     = "reasonCode/coding/code/@value",
            REASON.CODE.DISPLAY   = "reasonCode/coding/display/@value",
            MEDICATION.SYSTEM     = "medicationCodeableConcept/coding/system/@value",
            MEDICATION.CODE       = "medicationCodeableConcept/coding/code/@value",
            MEDICATION.DIPLAY     = "medicationCodeableConcept/coding/display/@value",
            PATIENT               = "subject/reference/@value",
            ENCOUNTER             = "context/reference/@value",
            BEGIN                 = "effectivePeriod/start/@value",
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
			BEGIN          = "period/start/@value",
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

  dfs <- bundle.to.dataframes( bundle, design )
  ```


### extract an attribute from bundle tags
- ```tag.attr( bundle, xpath )```

  e.g. how many medication statements are available?
  ```
  tag.attr(
    download.page( "https://hapi.fhir.org/baseR4/MedicationStatement/?_summary=count&_format=xml" ),
    "total/@value" )

  # or

  tag.attr(
    download.bundle( "https://hapi.fhir.org/baseR4/MedicationStatement/?_summary=count&_format=xml" )[[ 1 ]],
    "total/@value" )
  ```
