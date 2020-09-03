# fhircrackr: Handling HL7&reg; FHIR&reg; resources in R 
fhircrackr is a package that conveniently downloads FHIR<sup>[1](#hl7stuff)</sup> resources in xml format and converts them to R data frames. It uses FHIR search to download bundles from a FHIR server, provides functions to save and read xml-files containing such bundles and allows flattening the bundles to data frames using XPath expressions.

You can download the development version using `devtools::install_github("POLAR-fhiR/fhircrackr")`.

This readme gives a only short overview over the most important functions in `fhircrackr`. For a more comprehensive introduction see the package vignette.

## Prerequisites
For the moment, this package focuses mostly on downloading and flattening resources from a FHIR server. This requires some prerequisites:

- The endpoint of the FHIR server you want to access. If you don't have your own FHIR server, you can use one of the publicly available servers, such as https&#58;//hapi.fhir.org/baseR4 or http&#58;//fhir.hl7.de:8080/baseDstu3. The endpoint of a FHIR server is often referred to as [base].

- To download resources from the server, you should be familiar with [FHIR search requests](https://www.hl7.org/fhir/search.html). FHIR search allows you to download sets of resources that match very specific requirements. As this package mainly takes care of the downloading and flattening part, we will use very simple examples of FHIR search requests of the form `[base]/[type]?parameter(s)`, where `[type]` refers to the type of resource you are looking for and `parameter(s)` characterize specific properties those resources should have.
`https://hapi.fhir.org/baseR4/Patient?gender=female` for example downloads all Patient resources from the FHIR server at `https://hapi.fhir.org/baseR4/` that represent female patients.

- To specify which attributes of the FHIR resources you want in your data frame, you should have at least some familiarity with XPath expressions, because this package downloads the resources in xml-format. A good tutorial for XPath can be found [here](https://www.w3schools.com/xml/xpath_intro.asp).

## Download FHIR resources from a server

```r
bundles <- fhir_search("https://hapi.fhir.org/baseR4/MedicationStatement?_include=MedicationStatement:subject", max.bundles=5)
```
To download resources from a FHIR server into R, you use the function `fhir_search()`. This function requires you to state a full FHIR search request such as `https://hapi.fhir.org/baseR4/MedicationStatement?_include=MedicationStatement:subject` in the argument `request`. This example request downloads all MedicationStatement resources and the Patient resources they link to.

If you want to connect to a FHIR server that uses basic authentication, you can supply the arguments `username` and `password`. Because endpoints can sometimes be hard to reach, `fhir_search()` will start five attempts to connect to the endpoint before it gives up. With the arguments `max_attempts` and `delay_between_attempts` you can control this number as well the time interval between attempts.

In general, a FHIR search request returns a *bundle* of the resources you requested. If there are a lot of resources matching your request, the search result isn't returned in one big bundle but distributed over several of them. If the argument `max_bundles` is set to its default `Inf`, `fhir_search()` will return all available bundles, meaning all resources matching your request. If you set it to another number, the download will stop once it has reached the specified number of bundles. Note that in this case, the result *may not contain all* the resources from the server matching your request!

`fhir_search()` returns a list of xml objects where each list element represents one bundle of resources.

## Flatten FHIR resources into data frames
If you want to do statistical analyses, the xml format the resources come in is not very useful. Instead, we need the data in some matrix like form, preferably as a data frame. Most of the times it makes sense to create one data frame per type of resource (e.g. Patient, MedicationStatement).

We can achieve this using the `fhir_crack()` function.

```r
#define design

design <- list(

  MedicationStatements = list(
    resource = "//MedicationStatement",
    cols = list(
      STATUS             = "status",
      MEDICATION.SYSTEM  = "medicationCodeableConcept/coding/system",
      MEDICATION.CODE    = "medicationCodeableConcept/coding/code",
      MEDICATION.DISPLAY = "medicationCodeableConcept/coding/display",
      PATIENT            = "subject/reference",
      START              = "effectivePeriod/start",
      END                = "effectivePeriod/end"
    ),
    style = list(
    sep = " ",
    brackets = c("[", "]"),
    rm_empty_cols = FALSE
    )
  ),

  Patients = list(
    resource = "//Patient",
    cols = "./*"
  )
)

#Convert resources
dfs <- fhir_crack(bundles = bundles, design = design)

#Inspect results
View(df$Patients)
```

`fhir_crack()` takes a list of bundles as returned by `fhir_search()` and a list `design` which contains one or more data.frame descriptions (two called `Patients` and `MedicationStatements` in the above example) defining the data to be extracted from the resources. `fhir_crack()` returns a list of data frames, one for each data.frame description.

The `design` has to follow a specific structure. It must be a named list of data.frame descriptions, where each data.frame description is a list with the following elements:

1. *resource*
A string containing an XPath expression to the resource you want to extract, e.g. `"//Patient"`. If your bundles are the result of a regular FHIR search request, the correct XPath expression will always be `"//<resource name>"`.

2. *cols*
Can be NULL, a string or a list describing the columns your data frame is going to have.

- If *cols* is NULL, all attributes available in the resources will be extracted and put in one column each, the column names will be chosen automatically and reflect the position of the attribute in the resource.

- If *cols* is a string with an XPath expression indicating a certain level in the bundle, all attributes on this specific level will be extracted. `"./*"` e.g. will extract all attributes that are located (exactly) one level below the root level given by `"//Patient"`.

- If *cols* is a named list of XPath expressions, each element is taken to be the description for one column. `PATIENT = "subject/reference"` for example creates a column named PATIENT which contains the values for the attribute indicated by the XPath expression `"subject/reference"`.

3. *style*
Can be NULL or a list of length three with the following named elements:

- *sep*: A string defining the seperator used when multiple entries to the same attribute are pasted together, e.g. `" "`. 

- *brackets* Either NULL or a character vector of length two. If NULL, multiple entries will be pasted together without indices. If character, the two strings provided here are used as brackets for automatically generated indices to sort out multiple entries. `brackets = c("[", "]")` e.g. will lead to indices like `[1.1]`.  

- *rm_empty_cols*: Logical. If `TRUE`, columns containing only `NA` values will be removed, if `FALSE`, these columns will be kept.

All three elements of style can also be controlled directly by the `fhir_crack()` arguments `sep`, `brackets` and `remove_empty_columns`. If the function arguments are `NULL` (their default), the values provided in *style* are used, if they are not NULL, they will overwrite any values in *style*. If both the function arguments and the *style* component of the data.frame description are NULL, default values(`sep=" "`, `brackets = NULL`, `rm_empty_cols=TRUE`) will be assumed. 


For detailed examples of the different variants of the `design` please see the package vignette.


## Multiple entries
When there are multiple entries to one attribute, e.g. multiple addresses for the same Patient resource, `fhir_crack()` will paste these entries together using the string provided in the argument `sep`. If you set `brackets=c('[', ']')`, the entries will be assigned to indices to allow you to distinguish between entries. The indices are surrounded by the given brackets:

```r
#create example bundle with multiple entries

bundle<-xml2::read_xml(
	"<Bundle>
		
		<Patient>
			<id value='id1'/>
			<address>
				<use value='home'/>
				<city value='Amsterdam'/>
				<type value='physical'/>
				<country value='Netherlands'/>
			</address>
			<birthDate value='1992-02-06'/>
		</Patient>
		
		<Patient>
			<id value='id2'/>
			<address>
				<use value='home'/>
				<city value='Rome'/>
				<type value='physical'/>
				<country value='Italy'/>
			</address>
			<address>
				<use value='work'/>
				<city value='Stockholm'/>
				<type value='postal'/>
				<country value='Sweden'/>
			</address>			
			<birthDate value='1980-05-23'/>
		</Patient>
		
		<Patient>
			<id value='id3'/> 
			<address>
				<use value='home'/>
				<city value='Berlin'/>
			</address>
			<address>
				<type value='postal'/>
				<country value='France'/>
			</address>
			<address>
				<use value='work'/>
				<city value='London'/>
				<type value='postal'/>
				<country value='England'/>
			</address>
			<birthDate value='1974-12-25'/>
		</Patient>		
		
	</Bundle>"
)

bundle_list<-list(bundle)

#define design
design <- list(
	Patients = list("//Patient")
)

#extract data frame
dfs <- fhir_crack(bundle_list, design, sep = " ", brackets= c("[","]"))

dfs$Patients
```
For more information on how to deal with these indexed data frames, e.g. how to melt rows with multiple entries into several distinct rows or how to get rid of the indices again, please see the respective paragraph in the package vignette. 


## Save and load downloaded bundles
Since `fhir_crack()` discards of all the data not specified in `design` it makes sense to store the original search result for reproducibility and in case you realize later on that you need elements from the resources that you haven't extracted at first.

There are two ways of saving the FHIR bundles you downloaded: Either you save them as R objects, or you write them to an xml file.

### Save and load bundles as R objects
If you want to save the list of downloaded bundles as an `.rda` or `.RData` file, you cannot just R's `save()`or `save_image()` on it, because this will break the external pointers in the xml objects representing your bundles. Instead, you have to serialize the bundles before saving and unserialize them after loading. For single xml objects the package `xml2` proved serialization functions. For convenience, however, `fhircrackr` provides the functions `fhir_serialize()` and `fhir_unserialize()` that you can use directly on the list of bundles returned by `fhir_search()`:

```r
#serialize bundles
serialized_bundles <- fhir_serialize(patient_bundles)

#have a look at them
head(serialized_bundles[[1]])

#save
save(serialized_bundles, file="bundles.rda")
```

If you load this bundle again, you have to unserialize it, before you can work with it:

```r
#load bundles
load("bundles.rda")

#unserialize
bundles <- fhir_unserialize(serialized_bundles)

#have a look
head(bundles[[1]])
```

After unserialization, the pointers are restored and you can continue to work with the bundles. Note that the example bundle `medication_bundles` that is provided with the `fhircrackr` package is also provided in its serialized form and has to be unserialized as described on its help page.

### Save and load bundles as xml files
If you want to store the bundles in xml files instead of R objects, you can use the functions `fhir_save()` and `fhir_load()`.
`fhir_save()` takes a list of bundles in form of xml objects (as returned by `fhir_search()`) and writes them into the directory specified in the argument `directory`. Each bundle is saved as a separate xml-file. If the folder defined in `directory` doesn't exist, it is created in the current working directory.

```r
#save bundles as xml files
fhir_save(patient_bundles, directory="MyDirectory")
```

To read bundles saved with `fhir_save()` back into R, you can use `fhir_load()`:

```r
bundles <- fhir_load("MyDirectory")
```  

`fhir_load()` takes the name of the directory (or path to it) as its only argument. All xml-files in this directory will be read into R and returned as a list of bundles in xml format just as returned by `fhir_search()`.

## Save and read designs
If you want to save a design for later or to share with others, you can do so using the `fhir_save_design()`. This function takes a design and saves it as an xml file:

```r
fhir_save_design(design1, file = paste0(temp_dir,"\\design.xml"))
```

To read the design back into R, you can use `fhir_load_design()`:

```r
fhir_load_design(paste0(temp_dir,"\\design.xml"))
```




## Acknowledgements
This work was carried out by the SMITH consortium and the cross-consortium use case POLAR_MI; both are part of the German Initiative for Medical Informatics and funded by the German Federal Ministry of Education and Research (BMBF), grant no. 01ZZ1803A , 01ZZ1803C and 01ZZ1910A.



------

<a name="hl7stuff">1</a>: FHIR is the registered trademark of HL7 and is used with the permission of HL7. Use of the FHIR trademark does not constitute endorsement of this product by HL7.