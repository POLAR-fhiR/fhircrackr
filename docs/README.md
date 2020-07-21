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

## Download fhir resources from a server

```r
bundles <- fhir_search("https://hapi.fhir.org/baseR4/MedicationStatement?_include=MedicationStatement:subject", max.bundles=5)
```
To download resources from a FHIR server into R, you use the function `fhir_search()`. This function requires you to state a full FHIR search request such as `https://hapi.fhir.org/baseR4/MedicationStatement?_include=MedicationStatement:subject` in the argument `request`. This example request downloads all MedicationStatement resources and the Patient resources they link to.

If you want to connect to a FHIR server that uses basic authentication, you can supply the arguments `username` and `password`. Because endpoints can sometimes be hard to reach, `fhir_search()` will start five attempts to connect to the endpoint before it gives up. With the arguments `max_attempts` and `delay_between_attempts` you can control this number as well the time interval between attempts.

In general, a FHIR search request returns a *bundle* of the resources you requested. If there are a lot of resources matching your request, the search result isn't returned in one big bundle but distributed over several of them. If the argument `max_bundles` is set to its default `Inf`, `fhir_search()` will return all available bundles, meaning all resources matching your request. If you set it to another number, the download will stop once it has reached the specified number of bundles. Note that in this case, the result *may not contain all* the resources from the server matching your request!

`fhir_search()` returns a list of xml objects where each list element represents one bundle of resources.

## Turn fhir resources into data frames
If you want to do statistical analyses, the xml format the resources come in is not very useful. Instead, we need the data in some matrix like form, preferably as a data frame.

We can achieve this using the `fhir_crack()` function.

```r
#define which elements of the resources are of interest
design <- list(

  MedicationStatements = list(
    "//MedicationStatement",
    list(
      STATUS             = "status",
      MEDICATION.SYSTEM  = "medicationCodeableConcept/coding/system",
      MEDICATION.CODE    = "medicationCodeableConcept/coding/code",
      MEDICATION.DISPLAY = "medicationCodeableConcept/coding/display",
      PATIENT            = "subject/reference",
      START              = "effectivePeriod/start",
      END                = "effectivePeriod/end"
    )
  ),

  Patients = list(
    "//Patient",
    list(
      NAME.GIVEN  = "name/given",
      NAME.FAMILY = "name/family",
      SEX         = "gender",
      BIRTHDATE   = "birthDate"
    )
  )
)

#Convert resources
dfs <- fhir_crack(bundles, design)

#Inspect results
View(df$Patients)
```

`fhir_crack()` takes a list of bundles as returned by `fhir_search()` and a list `design` defining the data to be extracted from the resources and returns a list of data frames.

`design` should be a named list, where each element of `design` corresponds to one data frame that will be created. The element names of `design` are going to be the names of the data.frames in the result of the function.

It makes sense to create one data frame per type of resource (MedicationStatement and Patient in this case). Lets have a look at the element `Medication` from the above example of `design` to understand how it works:

`Medication` is a list of length 2, where the first element is an XPath expression selecting the nodes (i.e. resources) matching a MedicationStatement, so this element is used to define the type of resource in this data frame.

The second element is again a list, this time a named list. Each element corresponds to one variable (i.e. column) in the resulting data frame. The name (e.g. `Status`) will be the column name, the column values will be taken from the attribute defined by the following XPath expression (e.g. `"status"`).

The abstract form `design` therefore has is:

```r
list(

  <Name of first data frame> = list(
    <XPath to resource type>,
    list(
      <column name 1> = <XPath to attribute>,
      <column name 2> = <XPath to attribute>
      ...
    )
  ),

  <Name of second data frame> = list(
    <XPath to resource type>,
    list(
      <column name 1> = <XPath to attribute>,
      <column name 2> = <XPath to attribute>
      ...
    )
  ),
  ...
)
```
There are other forms `design` can take, for example if you want to extract all attributes or only attributes from a certain level of the resource. To get to know these options, please see the package vignette.


## Multiple entries
When there are multiple entries to one attribute, e.g. multiple addresses for the same Patient resource, `fhir_crack()` will paste these entries together using the string provided in the argument `sep`. If you set `add_indices=TRUE`, the entries will be assigned indices to allow you to distinguish between entries:

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

```{r}
bundles <- fhir_load("MyDirectory")
```  

`fhir_load()` takes the name of the directory (or path to it) as its only argument. All xml-files in this directory will be read into R and returned as a list of bundles in xml format just as returned by `fhir_search()`.


## Acknowledgements
This work was carried out by the SMITH consortium and the cross-consortium use case POLAR_MI; both are part of the German Initiative for Medical Informatics and funded by the German Federal Ministry of Education and Research (BMBF), grant no. 01ZZ1803A , 01ZZ1803C and 01ZZ1910A.



------

<a name="hl7stuff">1</a>: FHIR is the registered trademark of HL7 and is used with the permission of HL7. Use of the FHIR trademark does not constitute endorsement of this product by HL7.