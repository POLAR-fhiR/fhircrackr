# fhiR
fhiR is a package for convenient downloading fhir resources in xml format and converting to R data frames.

## What can you do?

### download a single fhir bundle page
- ```download.page( fhir.search.request, max.attempts = 5 )```  

### download a complete fhir bundle
- ```download.bundle( fhir.search.request, max.attempts = 5 )```  

### write bundle pages as xml files to directory
- ```write.bundle( bundle, directory )```  

### read bundle pages as xml files from directory
- ```read.bundle( directory )```

### extract a single bundle page to data frames
- ```page.to.dataframes( page, design )```

### extract a complete bundle to data frames
- ```bundle.to.dataframes( bundle, design )```
