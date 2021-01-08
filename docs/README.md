# fhircrackr: Handling HL7&reg; FHIR&reg; resources in R 
fhircrackr is a package that conveniently downloads FHIR<sup>[1](#hl7stuff)</sup> resources in xml format and converts them to R data frames. It uses FHIR search to download bundles from a FHIR server, provides functions to save and read xml-files containing such bundles and allows flattening the bundles to data frames using XPath expressions.

You can download the development version using `devtools::install_github("POLAR-fhiR/fhircrackr")`.

This readme gives a only short overview over the most important functions in `fhircrackr`. For a more comprehensive introduction we strongly recommend to read the package vignette which you can find in the folder vignettes in this GitHub repository.

## Prerequisites
For the moment, this package focuses mostly on downloading and flattening resources from a FHIR server. This requires some prerequisites:

- The endpoint of the FHIR server you want to access. If you don't have your own FHIR server, you can use one of the publicly available servers, such as https&#58;//hapi.fhir.org/baseR4 or http&#58;//fhir.hl7.de:8080/baseDstu3. The endpoint of a FHIR server is often referred to as [base].

- To download resources from the server, you should be familiar with [FHIR search requests](https://www.hl7.org/fhir/search.html). FHIR search allows you to download sets of resources that match very specific requirements. The `fhircrackr` package offers some help building FHIR search requests, for this please see the paragraph on FHIR search requests.

- To specify which attributes of the FHIR resources you want in your data frame, you should have at least some familiarity with XPath expressions, because this package downloads the resources in xml-format. A good tutorial for XPath can be found [here](https://www.w3schools.com/xml/xpath_intro.asp).

##specify FHIR search requests
This paragraph introduces the basics of FHIR search and some functions to build valid FHIR search requests with `fhircrackr`. If you are already familiar and comfortable with FHIR search, you can skip this paragraph.

A FHIR search request will mostly have the form `[base]/[type]?parameter(s)`, where `[base]` is a URL to the FHIR endpoint you are trying to access, `[type]` refers to the type of resource you are looking for and `parameter(s)` characterize specific properties those resources should have. The function `fhir_build_request()` offers a solution to bring those three components together correctly, taking care of proper formatting for you. You use this function in conjunction with three sub-functions: `fhir_base()`, `fhir_resource()` and `fhir_key_value()`:

```r
fhir_build_request(
	fhir_base(" http://hapi.fhir.org/baseR4/"),
	fhir_resource("patient"),
	fhir_key_value(key = "birthdate", value = "lt2000-01-01", url_enc = TRUE),
	fhir_key_value(key = "_count", value = "10")
)
```
You have to provide exactly one base and one resource and can provide none or as many key value pairs as you want. 

### Accessing the current request
Whenever you call `fhir_build_request()` or `fhir_search()` (see below), the corresponding FHIR search request will be saved implicitly and can be accessed like this:

```{r}
fhir_current_request()
```

You can update it with new search parameters using `fhir_update_request()`. If you set the argument `append=FALSE`, the key value pairs in the current request are overwritten:

```r
fhir_update_request(fhir_key_value(key = "gender", value = "male"),
					append = FALSE,
					return_request = TRUE)
```

If you set `append=TRUE`, the new pairs are appended to the current ones:

```r
fhir_current_request()
fhir_update_request(fhir_key_value(key = "birthdate", value = "lt2000-01-01"),
					append = TRUE,
					return_request = FALSE)
fhir_current_request()
```

You can save the requests you build explicitly in an object and provide this object to the `request` argument of `fhir_search()`. If you call `fhir_search()` without providing an explicit request however, the function will automatically call `fhir_current_request()`.


## Download FHIR Resources from a server

```r
bundles <- fhir_search("https://hapi.fhir.org/baseR4/MedicationStatement?_include=MedicationStatement:subject", max.bundles=5)
```
To download resources from a FHIR server into R, you use the function `fhir_search()`. This function requires you to state a full FHIR search request such as `https://hapi.fhir.org/baseR4/MedicationStatement?_include=MedicationStatement:subject` in the argument `request`. This example request downloads all MedicationStatement resources and the Patient resources they link to.

If you want to connect to a FHIR server that uses basic authentication, you can supply the arguments `username` and `password`. Because endpoints can sometimes be hard to reach, `fhir_search()` will start five attempts to connect to the endpoint before it gives up. With the arguments `max_attempts` and `delay_between_attempts` you can control this number as well the time interval between attempts.

In general, a FHIR search request returns a *bundle* of the resources you requested. If there are a lot of resources matching your request, the search result isn't returned in one big bundle but distributed over several of them. If the argument `max_bundles` is set to its default `Inf`, `fhir_search()` will return all available bundles, meaning all resources matching your request. If you set it to another number, the download will stop once it has reached the specified number of bundles. Note that in this case, the result *may not contain all* the resources from the server matching your request!

`fhir_search()` returns a list of xml objects where each list element represents one bundle of resources.

## Flatten FHIR Resources into data frames
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
View(dfs$Patients)
```

`fhir_crack()` takes a list of bundles as returned by `fhir_search()` and a list `design` which contains one or more data.frame descriptions (in the above example there are two called `Patients` and `MedicationStatements` ) defining the data to be extracted from the resources. `fhir_crack()` returns a list of data frames, one for each data.frame description.

The `design` has to follow a specific structure. It must be a named list of data.frame descriptions, where each data.frame description is a list with the following elements:

1. *resource*

A string containing an XPath expression to the resource you want to extract, e.g. `"//Patient"`. If your bundles are the result of a regular FHIR search request, the correct XPath expression will always be `"//<resource name>"`.

2. *cols*

Can be NULL, a string or a list describing the columns your data frame is going to have.

- If *cols* is NULL, all attributes available in the resources will be extracted and put in one column each, the column names will be chosen automatically and reflect the position of the attribute in the resource.

- If *cols* is a string with an XPath expression indicating a certain level in the bundle, all attributes on this specific level will be extracted. `"./*"` e.g. will extract all attributes that are located (exactly) one level below the root level given by `"//Patient"`.

- If *cols* is a named list of XPath expressions, each element is taken to be the description for one column. `PATIENT = "subject/reference"` for example creates a column named PATIENT which contains the values for the attribute indicated by the XPath expression `"subject/reference"`.

3. *style*

Can be NULL or a list of length 3 with the following named elements:

- *sep*: A string defining the seperator used for pasting multiple entries to the same attribute together, e.g. `" "`. 

- *brackets* Either NULL or a character vector of length two. If NULL, multiple entries will be pasted together without indices. If character, the two strings provided here are used as brackets for automatically generated indices to sort out multiple entries. `brackets = c("[", "]")` e.g. will lead to indices like `[1.1]`.  

- *rm_empty_cols*: Logical. If `TRUE`, columns containing only `NA` values will be removed, if `FALSE`, these columns will be kept.

All three elements of style can also be controlled directly by the `fhir_crack()` arguments `sep`, `brackets` and `remove_empty_columns`. If the function arguments are `NULL` (their default), the values provided in *style* are used, if they are not NULL, they will overwrite any values in *style*. If both the function arguments and the *style* component of the data.frame description are NULL, default values(`sep=" "`, `brackets = NULL`, `rm_empty_cols=TRUE`) will be assumed. 


For detailed examples of the different variants of the `design` please see the package vignette.


## Multiple Entries
When there are multiple entries to one attribute, e.g. multiple addresses for the same Patient resource, `fhir_crack()` will paste these entries together using the string provided in the argument `sep`. If for example you set `brackets=c("[", "]")`, the indices surrounded by [ ] will be assigned to the entries to allow you to distinguish between them:

```r
#create example bundle with multiple entries

bundle <- xml2::read_xml(
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

bundle_list <- list(bundle)

#define design
design <- list(
	Patients = list(resource = "//Patient")
)

#extract data frame
dfs <- fhir_crack(bundles = bundle_list, design = design, sep = " ", brackets= c("[","]"))

dfs$Patients
```
For more information on how to deal with these indexed data frames, e.g. how to melt rows with multiple entries into several distinct rows or how to get rid of the indices again, please see the respective paragraph in the package vignette. 


## Save and load downloaded bundles
Since `fhir_crack()` discards of all the data not specified in `design` it makes sense to store the original search result for reproducibility and in case you realize later on that you need elements from the resources that you haven't extracted at first.

There are two ways of saving the FHIR bundles you downloaded: Either you save them as R objects, or you write them to an xml file.

### Save and load bundles as R objects
If you want to save the list of downloaded bundles as an `.rda` or `.RData` file, you cannot just R's `save()`or `save_image()` on it, because this will break the external pointers in the xml objects representing your bundles. Instead, you have to serialize the bundles before saving and unserialize them after loading. For single xml objects the package `xml2` provides serialization functions. For convenience, however, `fhircrackr` provides the functions `fhir_serialize()` and `fhir_unserialize()` that you can use directly on the list of bundles returned by `fhir_search()`:

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

After unserialization, the pointers are restored and you can continue to work with the bundles. Note that the example bundles `patient_bundles` and `medication_bundles` that are provided with the `fhircrackr` package are also provided in their serialized form and have to be unserialized as described on theit help pages.

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

## Performance
When you want to download a lot of data from the server, you might run into problems with time and memory. To speed up the download of a large number of resources, you can increase the number of resources per bundle by setting the `_count` parameter in your FHIR search request e.g. `https://hapi.fhir.org/baseR4/Patient?_count=500`. This will reduce the number of (potentially time consuming) queries to the server.

When the number of downloaded bundles might overburden your computers working memory, you can either set the `save_to_disc=TRUE` argument of `fhir_search()` to save bundles consecutively to a specified directory instead of loading them into the R session or you can load and process the bundles in several smaller batches by using `fhir_next_bundle_url()`. For examples of both approaches, please see the package vignette.


## Acknowledgements
This work was carried out by the SMITH consortium and the cross-consortium use case POLAR_MI; both are part of the German Initiative for Medical Informatics and funded by the German Federal Ministry of Education and Research (BMBF), grant no. 01ZZ1803A , 01ZZ1803C and 01ZZ1910A.



------

<a name="hl7stuff">1</a>: FHIR is the registered trademark of HL7 and is used with the permission of HL7. Use of the FHIR trademark does not constitute endorsement of this product by HL7.