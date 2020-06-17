# fhiR
fhiR is a package that conveniently downloads fhir resources in xml format and converts them to R data frames. It uses fhir-search to download bundles from a fhir server, provides functions to save and read xml-files containing such bundles and allows flattening the bundles to data.frames using XPath expressions.

## Prerequisites
For the moment, this package focuses mostly on downloading and flattening resources from a fhir server. This requires some prerequisites:

- The endpoint of the fhir server you want to access. If you don't have you own fhir server, you can use one of the publicly available servers, such as http://hapi.fhir.org/baseR4 or http://fhir.hl7.de:8080/. In the following the endpoint of you fhir server will generally referred to as [base].

- To download ressources from the server, you should be familiar with [fhir search requests](https://www.hl7.org/fhir/search.html). Fhir search allows you to download sets of resources that match very specific requirements. As this package mainly takes care of the downloading and flattening part, we will mostly use very simple examples of fhir search requests of the form `[base]/[type]?parameter(s)`, where `[type]` refers to the type of resource you are looking for and `parameter(s)` characterise specific properties those resources should have.
`http://hapi.fhir.org/baseR4/Patient?gender=female` for example downloads all Patient resources from the fhir server at `http://hapi.fhir.org/baseR4/` that represent female patients.

- To specify which elements from the fhir resources you want in your data frame, you should have at least some familiarity with XPath expressions, because this package downloads the resources in xml-format. A good tutorial for XPath can be found [here](https://www.w3schools.com/xml/xpath_intro.asp).

## Download fhir resources from a server

### Example 1: Using fhir_search()

```r
bundles <- fhir_search("http://hapi.fhir.org/baseR4/Patient?gender=female", max.bundles=5)
```

To download resources from a fhir server into R, you use the function `fhir_search()`. This function requires you to state a full fhir search request such as `http://hapi.fhir.org/baseR4/Patient?gender=female` in the argument `request`.

If you want to connect to a fhir server that uses basic authentification, you can supply the arguments `username` and `password`. Because endpoints can sometimes be hard to reach, `fhir_search()` will start five attempts to connect to the endpoint before it gives up. With the arguments `max.attempts` and `delay.between.attempts` you can control this number as well the time interval between attempts.

In general, a fhir search request returns a *bundle* of the resources you requested. If there are a lot of resources matching your request, the search result isn't returned in one big bundle but distributed over several of them. If the argument `max.bundles` is set to its default `Inf`, `fhir_search()` will return all available bundles, meaning all resources matching your request. If you set it to another number, the download will stop once it has reached the specified number of bundles. Note that in this case, the result *may not contain all* the resources from the server matching your request!

`fhir_search()` returns a list of xml objects where each list element represents one bundle of resources.

### Example 2: Using get_bundle()
```r
bundle <- get_bundle("http://hapi.fhir.org/baseR4/Patient?gender=female")

#identical to
bundles <- fhir_search("http://hapi.fhir.org/baseR4/Patient?gender=female", max.bundles=1)
bundle <- bundles[[1]]
```

If for some reason you just want the first bundle of you fhir request to be downloaded from the server, you can use `get_bundle()`. This function takes the same arguments as `fhir_search()` except for the `max.bundles` argument and returns the first bundle as an xml object. In general, we wouldn't advise using this function. If you want just the first bundle, use `fhir_search()` with `max.bundles=1`.

## Turn fhir resources into data frames
If you want to do statistical analyses, the xml format the resources come in is not very useful. Instead, we need the data in some matrix like form, preferably as a data frame.

### Example 1: Using fhir2dfs
 ```r
#download MedicationStatement resources and associated Patient resources
bundles <- fhir_search("https://hapi.fhir.org/baseR4/MedicationStatement?_include=MedicationStatement:subject", max.bundles=5)

#define which elements of the resources are of interest
design <- list(

  MedicationStatements = list(
    ".//MedicationStatement",
    list(
      STATUS                = "status/@value",
      MEDICATION.SYSTEM     = "medicationCodeableConcept/coding/system/@value",
      MEDICATION.CODE       = "medicationCodeableConcept/coding/code/@value",
      MEDICATION.DISPLAY    = "medicationCodeableConcept/coding/display/@value",
      PATIENT               = "subject/reference/@value",
      START                 = "effectivePeriod/start/@value",
      END                   = "effectivePeriod/end/@value"
    )
  ),

  Patients = list(
    ".//Patient",
    list(
      NAME.GIVEN  = "name/given/@value",
      NAME.FAMILY = "name/family/@value",
      SEX         = "gender/@value",
      BIRTHDATE   = "birthDate/@value"
    )
  )
)

#Convert resources
dfs <- fhir2dfs(bundles, design)

#Inspect results
View(df$Patients)


```
`fhir2dfs()` takes a list of bundles as returned by `fhir_search()` and a list `design` defining the data to be extracted from the resources and returns a list of data frames.

`design` should be a named list, where each element of `design` corresponds to one data frame that will created. The element names of `design` are going to be the names of the data.frames in the result of the function.

It makes sense to create one data frame per type of resource (MedicationStatement and Patient in this case). Lets have a look at the element `Medication`from the above example of `design` to understand how it works:

`Medication` is a list of length 2, where the first element is an XPath expression selecting the nodes (i.e. resources) matching a MedicationStatement, so this element is used to define the type of resource in this data frame.

The second element is again a list, this time a named list. Each element corresponds to one variable (i.e. coloumn) in the resulting data frame. The name (e.g. `Status`) will be the column name, the column values will be taken from the attribut defined by the following XPath (e.g. `"status/@value"`).

The abstract form `design` should therefore have is:

```
list(

  <Name of first data frame> = list(
    <XPath to resource type>,
    list(
      <column name 1> = <XPath to attribute>
      <column name 2> = <XPath to attribute>
      ...
    )
  ),

  <Name of second data frame> = list(
    <XPath to resource type>,
    list(
      <column name 1> = <XPath to attribute>
      <column name 2> = <XPath to attribute>
      ...
    )
  ),
  ...
)
```
### Example 2: Using bundle2dfs
 ```r
#download single bundle containing MedicationStatement resources and associated Patient resources
bundle<- get_bundle("https://hapi.fhir.org/baseR4/MedicationStatement?_include=MedicationStatement:subject")

#define which elements of the resources are of interest
design <- list(

  MedicationStatements = list(
    ".//MedicationStatement",
    list(
      STATUS                = "status/@value",
      MEDICATION.SYSTEM     = "medicationCodeableConcept/coding/system/@value",
      MEDICATION.CODE       = "medicationCodeableConcept/coding/code/@value",
      MEDICATION.DISPLAY    = "medicationCodeableConcept/coding/display/@value",
      PATIENT               = "subject/reference/@value",
      START                 = "effectivePeriod/start/@value",
      END                   = "effectivePeriod/end/@value"
    )
  ),

  Patients = list(
    ".//Patient",
    list(
      NAME.GIVEN  = "name/given/@value",
      NAME.FAMILY = "name/family/@value",
      SEX         = "gender/@value",
      BIRTHDATE   = "birthDate/@value"
    )
  )
)

#Convert resources
dfs <- bundle2dfs(bundle, design)

```

`bundle2dfs()` works the same way as `fhir2dfs()` but takes just one single bundle in form of an xml object instead of a list of bundles as returned by `fhir_search()`.

## Save downloaded bundles
Since `fhir2dfs()`and `bundle2dfs()` discard of all the data not specified in `design` it makes sense to store the original search result for reproducibility and in case you realise later on that you need elements from the resources that you haven't extracted at first.

```r
#download bundles from fhir server
bundles <- fhir_search("http://hapi.fhir.org/baseR4/Patient?gender=female", max.bundles=5)

#save bundles as xml files
save_bundles(bundles, directory="MyDirectory")
```

`save_bundles()` takes a list of bundles in form of xml objects (as returned by `fhir_search()`) and writes them into the directory specified in the argument `directory`. Each bundle is saved as a seperate xml-file. If the folder defined in `directory` doesn't exist, it is created in the current working directory.

## Read bundles from a directory
To read bundles saved with `save_bundles()` back into R, you can use `load_bundles()`:

```r
bundles <- load_bundles("MyDirectory")
```
`load_bundles()` takes the name of the directory (or path to it) as its only argument. All xml-files in this directory will be read into R and returned as a list of bundles in xml format just as returned by `fhir_search()`.


## Other functions

### Extract an attribute from bundle tags
Sometimes it is useful to see how the attributes you extract with an XPath expression look like before you build the design for `fhir2dfs()`. This is possible with the function `tag.attr()`:

```r
#download bundles from fhir server
bundles <- fhir_search("http://hapi.fhir.org/baseR4/Patient?gender=female", max.bundles=1)

#Extract attribute
tag_attr(bundles[[1]],".//name/given/@value")
```
`tag.attr()` takes a bundle in form of an xml object and an XPath expression as its arguments. It returns a vector of all available values corresponding to the XPath expression. Note that missing values *do not appear* in the result, so you should never try to build a data.frame from different `tag.attr()` results on the same bundle, as these will have different lengths and will not align horizontally!

### Download capability statement
The [capability statement](https://www.hl7.org/fhir/capabilitystatement.html) documents  a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR.

```r
cap <- capability_statement("http://hapi.fhir.org/baseR4/")
```
`capabiliy_statement()` takes a fhir endpoint and returns a list of data frames containing all information from the capability statement of this server.


### Convert a xml doc or xml node to one data frame
```
xml2df( xml, design.for.one.data.frame, sep = "›" )
```
e.g. for a female patients data frame
```
design.patients <- list(

  ".//Patient[gender=female]",

  list(
    NAME.GIVEN  = "name/given/@value",
    NAME.FAMILY = "name/family/@value",
    SEX         = "gender/@value",
    BIRTHDATE   = "birthDate/@value"
  )
)

df <- xml2df( xml, design.patients, sep = "›" )
```

This function works similarly to `bundle2dfs()` but can extract only one single data.frame. This is actually a helper function for `bundle2dfs()` and `fhir2dfs()` and will in the long run not be in the exported namespace of the package.
