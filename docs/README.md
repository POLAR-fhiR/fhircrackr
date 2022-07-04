fhircrackr Intro: Handling HL7® FHIR® Resources in R
================

## Introduction

`fhircrackr` is a package designed to help analyzing HL7 FHIR[1]
resources.

FHIR stands for *Fast Healthcare Interoperability Resources* and is a
standard describing data formats and elements (known as “resources”) as
well as an application programming interface (API) for exchanging
electronic health records. The standard was created by the Health Level
Seven International (HL7) health-care standards organization. For more
information on the FHIR standard, visit
<a href="https://www.hl7.org/fhir/" target="_blank">https://www.hl7.org/fhir/</a>.

While FHIR is a very useful standard to describe and exchange medical
data in an interoperable way, it is not at all useful for statistical
analyses of data. This is due to the fact that FHIR data is stored in
many nested and interlinked resources instead of matrix-like structures.

Thus, to be able to do statistical analyses a tool is needed that allows
converting these nested resources into data frames. This process of
tabulating FHIR resources is not trivial, as the unpredictable degree of
nesting and connectedness of the resources makes generic solutions to
this problem not feasible.

We therefore implemented a package that makes it possible to download
FHIR resources from a server into R and to tabulate these resources into
(multiple) data frames.

The package is still under development. The CRAN version of the package
contains all functions that are already stable, for more recent (but
potentially unstable) developments, the development version of the
package can be downloaded from GitHub using
`devtools::install_github("POLAR-fhiR/fhircrackr")`.

This vignette is an introduction on the basic functionalities of the
`fhircrackr` and should give you a broad overview over what the package
can do. For more detailed instructions on each subtopic please have a
look the other vignettes. This introduction covers the following topics:

-   Prerequisites

-   Downloading resources from a FHIR server

-   Flattening resources

-   Multiple entries

-   Saving and loading downloaded bundles

## Prerequisites

The complexity of the problem requires a couple of prerequisites both
regarding your knowledge and access to data. We will shortly list the
preconditions for using the `fhircrackr` package here:

1.  First of all, you need the base URL of the FHIR server you want to
    access. If you don’t have your own FHIR server, you can use one of
    the available public servers, such as `https://hapi.fhir.org/baseR4`
    or `http://fhir.hl7.de:8080/baseDstu3`. The base URL of a FHIR
    server is often referred to as \[base\].

2.  To download resources from the server, you should be familiar with
    <a href="https://www.hl7.org/fhir/search.html" target="_blank">FHIR
    search requests</a>. FHIR search allows you to download sets of
    resources that match very specific requirements. The `fhircrackr`
    package offers some help building FHIR search requests, for this
    please see the vignette on downloading FHIR resources.

3.  In the first step, `fhircrackr` downloads the resources in xml
    format into R. To specify which elements from the FHIR resources you
    want in your data frame, you should have at least some familiarity
    with XPath expressions. A good tutorial on XPath expressions can be
    found here:
    <a href="https://www.w3schools.com/xml/xpath_intro.asp" target="_blank">https://www.w3schools.com/xml/xpath\_intro.asp</a>.

In the following we’ll go through a typical workflow with `fhircrackr`
step by step. The first and foremost step is of course, to install and
load the package:

``` r
install.packages("fhircrackr")
library(fhircrackr)
```

## Downloading resources

To download resources from a FHIR server, you need to send a FHIR search
request using `fhir_search()`. This introduction will not go into the
details of building a valid FHIR search request. For that, please see
the vignette on downloading FHIR resources or have a look at
`?fhir_url`. Here we will use a simple example of downloading all
Patient resources from a public HAPI server:

``` r
request <- fhir_url(url = "http://fhir.hl7.de:8080/baseDstu3", resource = "Patient")
patient_bundles <- fhir_search(request = request, max_bundles = 2, verbose = 0)
```

The minimum information `fhir_search()` requires is a url containing the
full FHIR search request in the argument `request` which you can build
by a call to `fhir_url()` or by providing an explicit string. In
general, a FHIR search request returns a *bundle* of the resources you
requested. If there are a lot of resources matching your request, the
search result isn’t returned in one big bundle but distributed over
several of them. If the argument `max_bundles` is set to its default
`Inf`, `fhir_search()` will return all available bundles, meaning all
resources matching your request. If you set it to `2` as in the example
above, the download will stop after the first two bundles. Note that in
this case, the result *may not contain all* the resources from the
server matching your request.

If you want to connect to a FHIR server that uses basic authentication,
you can supply the arguments `username` and `password`. If your server
uses some form of bearer token authorization, you can supply the token
in the argument `token`.

As you can see in the next block of code, `fhir_search()` returns a
`fhir_bundle_list` object, which is basically a list of xml objects
where each list element represents one bundle of resources, so a list of
two xml objects in our case:

``` r
length(patient_bundles)
#> [1] 2
patient_bundles
#> An object of class "fhir_bundle_list"
#> [[1]]
#> A fhir_bundle_xml object
#> No. of entries : 20
#> Self Link: http://hapi.fhir.org/baseR4/Patient
#> Next Link: http://hapi.fhir.org/baseR4?_getpages=ce958386-53d0-4042-888c-cad53bf5d5a1 ...
#> 
#> {xml_node}
#> <Bundle>
#>  [1] <id value="ce958386-53d0-4042-888c-cad53bf5d5a1"/>
#>  [2] <meta>\n  <lastUpdated value="2021-05-10T12:12:43.317+00:00"/>\n</meta>
#>  [3] <type value="searchset"/>
#>  [4] <link>\n  <relation value="self"/>\n  <url value="http://hapi.fhir.org/b ...
#>  [5] <link>\n  <relation value="next"/>\n  <url value="http://hapi.fhir.org/b ...
#>  [6] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837602"/ ...
#>  [7] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/example-r ...
#>  [8] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837624"/ ...
#>  [9] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837626"/ ...
#> [10] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837631"/ ...
#> [11] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837716"/ ...
#> [12] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837720"/ ...
#> [13] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837714"/ ...
#> [14] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837721"/ ...
#> [15] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837722"/ ...
#> [16] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837723"/ ...
#> [17] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837724"/ ...
#> [18] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/cfsb16116 ...
#> [19] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837736"/ ...
#> [20] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837737"/ ...
#> ...
#> 
#> [[2]]
#> A fhir_bundle_xml object
#> No. of entries : 20
#> Self Link: http://hapi.fhir.org/baseR4?_getpages=ce958386-53d0-4042-888c-cad53bf5d5a1 ...
#> Next Link: http://hapi.fhir.org/baseR4?_getpages=ce958386-53d0-4042-888c-cad53bf5d5a1 ...
#> 
#> {xml_node}
#> <Bundle>
#>  [1] <id value="ce958386-53d0-4042-888c-cad53bf5d5a1"/>
#>  [2] <meta>\n  <lastUpdated value="2021-05-10T12:12:43.317+00:00"/>\n</meta>
#>  [3] <type value="searchset"/>
#>  [4] <link>\n  <relation value="self"/>\n  <url value="http://hapi.fhir.org/b ...
#>  [5] <link>\n  <relation value="next"/>\n  <url value="http://hapi.fhir.org/b ...
#>  [6] <link>\n  <relation value="previous"/>\n  <url value="http://hapi.fhir.o ...
#>  [7] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837760"/ ...
#>  [8] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837766"/ ...
#>  [9] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837768"/ ...
#> [10] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837781"/ ...
#> [11] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837783"/ ...
#> [12] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837784"/ ...
#> [13] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837787"/ ...
#> [14] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837788"/ ...
#> [15] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837789"/ ...
#> [16] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837790"/ ...
#> [17] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837791"/ ...
#> [18] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837792"/ ...
#> [19] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837793"/ ...
#> [20] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837794"/ ...
#> ...
```

If for some reason you cannot connect to a FHIR server at the moment but
want to explore the following functions anyway, the package provides two
example lists of bundles containing Patient and MedicationStatement
resources. See `?patient_bundles` and `?medication_bundles` for how to
use them.

## Flattening resources

Now we know that inside these xml objects there is the patient data
somewhere. To bring it into a tabular format, we will use `fhir_crack()`
which creates one table per resource type requested in the `design`
argument. The most important argument `fhir_crack()` takes is `bundles`,
the list of bundles that is returned by `fhir_search()`. The second
important argument is `design`, an object that tells the function which
data to extract from the bundle and how. `fhir_crack()` returns (a list
of) data.frames or data.tables (if argument `data.table = TRUE`).

The object that is passed to the `design` argument can be of class
`fhir_table_description` or `fhir_design`. A `fhir_table_description` is
used when you want to extract just one resource type, resulting in a
single table. A `fhir_design` is basically a named list of
`fhir_table_descriptions` and is used when you want to extract several
resource types at once, resulting in a named list of tables.

The details of what the different elements of a `fhir_table_description`
or `fhir_design` mean are described in the vignette on flattening
resources. Please refer to this document for more information, as we
will just use one simple example here.

``` r
#define table_description
table_description <- fhir_table_description(
    resource = "Patient",
    cols     = c(
        PID         = "id",
        use_name    = "name/use",
        given_name  = "name/given",
        family_name = "name/family",
        gender      = "gender",
        birthday    = "birthDate"
    ),
    sep           = " ~ ",
    brackets      = c("<<", ">>"),
    rm_empty_cols = FALSE,
    format        = 'compact',
    keep_attr     = FALSE
)

#have a look
table_description
#> A fhir_table_description with the following elements: 
#> 
#> resource: Patient
#> 
#> cols: 
#> ------------ -----------------
#> column name | xpath expression
#> ------------ -----------------
#> PID         | id
#> use_name    | name/use
#> given_name  | name/given
#> family_name | name/family
#> gender      | gender
#> birthday    | birthDate
#> ------------ -----------------
#> 
#> sep:           ' ~ '
#> brackets:      '<<', '>>'
#> rm_empty_cols: FALSE
#> format:        'compact'
#> keep_attr:     FALSE
```

Each of the five style elements `sep`, `brackets`,
`remove_empty_columns`, `format` and `keep_attr` in `table_description`
can also be controlled directly by the argument of the same name of
`fhir_crack()`. If one of these function arguments is `NULL` (the
default value for each argument), the corresponding value specified from
the `table_description` will be used. If the argument in `fhir_crack` is
set, the corresponding value in `fhir_table_description` will be
overruled. If both the `fhir_crack` function argument and the
corresponding component in `fhir_table_description` are `NULL`, the
respective default value (`sep = ':::'`, `brackets = NULL`,
`rm_empty_cols = TRUE`, `format = 'compact'`, `keep_attr = FALSE`) will
be applied.

After it is defined, the `fhir_table_description` can be used in
`fhir_crack()` like this:

``` r
#flatten resources
patients <- fhir_crack(bundles = patient_bundles, design = table_description, verbose = 0)

#have look at the results
head(patients)
#>            PID                                       use_name
#> 1 <<1>>2072744                                <<1.1>>official
#> 2 <<1>>2431578                                <<1.1>>official
#> 3 <<1>>2431568 <<1.1>>official ~ <<2.1>>usual ~ <<3.1>>maiden
#> 4 <<1>>2431577                                <<1.1>>official
#> 5 <<1>>2431757                                     <<1.1>>old
#> 6 <<1>>2431759                                <<1.1>>official
#>                                                               given_name
#> 1                                                 <<1.1>>K ~ <<1.2>>Kari
#> 2                                                           <<1.1>>Roman
#> 3 <<1.1>>Peter ~ <<1.2>>James ~ <<2.1>>Jim ~ <<3.1>>Peter ~ <<3.2>>James
#> 4                                         <<1.1>>Ganpat ~ <<1.2>>Malekar
#> 5                                                                   <NA>
#> 6                                                             <<1.1>>ABC
#>                        family_name      gender        birthday
#> 1                  <<1.1>>Nordmann <<1>>female <<1>>2018-09-12
#> 2                     <<1.1>>Smith   <<1>>male <<1>>2021-07-19
#> 3 <<1.1>>Chalmers ~ <<3.1>>Windsor   <<1>>male <<1>>1974-12-25
#> 4                   <<1.1>>Malekar   <<1>>male <<1>>1996-02-07
#> 5                    <<1.1>>murali   <<1>>male            <NA>
#> 6                       <<1.1>>XYZ   <<1>>male <<1>>1998-01-03
```

## Extract more than one resource type

Of course the previous example is using just one resource type. If you
are interested in several types of resources, you use a `fhir_design`
containing several `fhir_table_descriptions`.

Consider the following example where we want to download
MedicationStatements referring to a certain medication we specify with
its SNOMED CT code and also the Patient resources these
MedicationStatements are linked to.

We can build the request like this:

``` r
request  <- fhir_url(
    url        = "https://hapi.fhir.org/baseR4", 
    resource   = "MedicationStatement", 
    parameters = c(
         "code"    = "http://snomed.info/ct|429374003",
        "_include" = "MedicationStatement:subject"))
```

Then we can download the resources:

``` r
medication_bundles <- fhir_search(request = request, max_bundles = 3)
```

Now our `design` needs two `table_descriptions` (called
`MedicationStatements` and `Patients` in our example), one for the
MedicationStatement resources and one for the Patient resources:

``` r
MedicationStatements <- fhir_table_description(
    resource = "MedicationStatement",
    cols     = c(
        MS.ID              = "id",
        STATUS.TEXT        = "text/status",
        STATUS             = "status",
        MEDICATION.SYSTEM  = "medicationCodeableConcept/coding/system",
        MEDICATION.CODE    = "medicationCodeableConcept/coding/code",
        MEDICATION.DISPLAY = "medicationCodeableConcept/coding/display",
        DOSAGE             = "dosage/text",
        PATIENT            = "subject/reference",
        LAST.UPDATE        = "meta/lastUpdated"
    ),
    sep           = "|",
    brackets      = NULL,
    rm_empty_cols = FALSE,
    format        = "compact",
    keep_attr     = FALSE
)

Patients <- fhir_table_description(resource = "Patient")

design <- fhir_design(MedicationStatements, Patients)
```

In this example, we have spelled out the table description
`MedicationStatement` completely, while we have used a short form for
`Patients`. The resulting design looks like this:

``` r
design
#> A fhir_design with 2 table descriptions:
#> A fhir_table_description with the following elements: 
#> 
#> resource: MedicationStatement
#> 
#> cols: 
#> ------------------- -----------------------------------------
#> column name        | xpath expression
#> ------------------- -----------------------------------------
#> MS.ID              | id
#> STATUS.TEXT        | text/status
#> STATUS             | status
#> MEDICATION.SYSTEM  | medicationCodeableConcept/coding/system
#> MEDICATION.CODE    | medicationCodeableConcept/coding/code
#> MEDICATION.DISPLAY | medicationCodeableConcept/coding/display
#> DOSAGE             | dosage/text
#> PATIENT            | subject/reference
#> LAST.UPDATE        | meta/lastUpdated
#> ------------------- -----------------------------------------
#> 
#> sep:           '|'
#> brackets:      no brackets
#> rm_empty_cols: FALSE
#> format:        'compact'
#> keep_attr:     FALSE
#> A fhir_table_description with the following elements: 
#> 
#> resource: Patient
#> 
#> cols: 
#> An empty fhir_columns object
#> 
#> sep:           ':::'
#> brackets:      no brackets
#> rm_empty_cols: FALSE
#> format:        'compact'
#> keep_attr:     FALSE
```

We can now use this `design` for `fhir_crack()`:

``` r
list_of_tables <- fhir_crack(bundles = medication_bundles, design = design, verbose = 0)

list_of_tables$MedicationStatements[1:5,]
#>   MS.ID STATUS.TEXT STATUS     MEDICATION.SYSTEM MEDICATION.CODE
#> 1 30233   generated active http://snomed.info/ct       429374003
#> 2 42091   generated active http://snomed.info/ct       429374003
#> 3 45724   generated active http://snomed.info/ct       429374003
#> 4 59597   generated active http://snomed.info/ct       429374003
#> 5 69117   generated active http://snomed.info/ct       429374003
#>   MEDICATION.DISPLAY           DOSAGE       PATIENT
#> 1   simvastatin 40mg 1 tab once daily Patient/30163
#> 2   simvastatin 40mg 1 tab once daily Patient/42024
#> 3   simvastatin 40mg 1 tab once daily Patient/45657
#> 4   simvastatin 40mg 1 tab once daily Patient/59530
#> 5   simvastatin 40mg 1 tab once daily Patient/69050
#>                     LAST.UPDATE
#> 1 2019-09-26T14:34:44.543+00:00
#> 2 2019-10-09T22:44:05.728+00:00
#> 3 2019-10-11T16:30:24.411+00:00
#> 4 2019-11-12T14:27:00.098+00:00
#> 5 2019-11-16T16:51:50.759+00:00

list_of_tables$Patients[18:20,]
#>    address.city address.country address.district
#> 18     Westford              US             <NA>
#> 19     Westford              US             <NA>
#> 20   Talad Kwan              TH            Muang
#>                                      address.extension
#> 18 http://hl7.org/fhir/StructureDefinition/geolocation
#> 19 http://hl7.org/fhir/StructureDefinition/geolocation
#> 20                                                <NA>
#>    address.extension.extension address.extension.extension.valueDecimal
#> 18        latitude:::longitude    42.58942256332994:::-71.3827654850569
#> 19        latitude:::longitude    42.58942256332994:::-71.3827654850569
#> 20                        <NA>                                     <NA>
#>          address.line address.postalCode address.state
#> 18  378 Krajcik Lodge               <NA> Massachusetts
#> 19  378 Krajcik Lodge               <NA> Massachusetts
#> 20 88/20 Tiwanon Road              11000    Nonthaburi
#>                                                          address.text
#> 18                                                               <NA>
#> 19                                                               <NA>
#> 20 88/20 Tiwanon Road, Talad Kwan, Muang, Nonthaburi, 11000, Thailand
#>    address.type address.use  birthDate communication.language.coding.code
#> 18         <NA>        <NA> 1946-03-29                              en-US
#> 19         <NA>        <NA> 1946-03-29                              en-US
#> 20       postal        work 1988-03-25                               <NA>
#>    communication.language.coding.display communication.language.coding.system
#> 18                               English                      urn:ietf:bcp:47
#> 19                               English                      urn:ietf:bcp:47
#> 20                                  <NA>                                 <NA>
#>    communication.language.text
#> 18                     English
#> 19                     English
#> 20                        <NA>
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                extension
#> 18 http://hl7.org/fhir/us/core/StructureDefinition/us-core-race:::http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity:::http://hl7.org/fhir/StructureDefinition/patient-mothersMaidenName:::http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex:::http://hl7.org/fhir/StructureDefinition/patient-birthPlace:::http://synthetichealth.github.io/synthea/disability-adjusted-life-years:::http://synthetichealth.github.io/synthea/quality-adjusted-life-years
#> 19 http://hl7.org/fhir/us/core/StructureDefinition/us-core-race:::http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity:::http://hl7.org/fhir/StructureDefinition/patient-mothersMaidenName:::http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex:::http://hl7.org/fhir/StructureDefinition/patient-birthPlace:::http://synthetichealth.github.io/synthea/disability-adjusted-life-years:::http://synthetichealth.github.io/synthea/quality-adjusted-life-years
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <NA>
#>                        extension.extension extension.extension.valueCoding.code
#> 18 ombCategory:::text:::ombCategory:::text                      2106-3:::2186-5
#> 19 ombCategory:::text:::ombCategory:::text                      2106-3:::2186-5
#> 20                                    <NA>                                 <NA>
#>    extension.extension.valueCoding.display
#> 18          White:::Not Hispanic or Latino
#> 19          White:::Not Hispanic or Latino
#> 20                                    <NA>
#>                               extension.extension.valueCoding.system
#> 18 urn:oid:2.16.840.1.113883.6.238:::urn:oid:2.16.840.1.113883.6.238
#> 19 urn:oid:2.16.840.1.113883.6.238:::urn:oid:2.16.840.1.113883.6.238
#> 20                                                              <NA>
#>    extension.extension.valueString extension.valueAddress.city
#> 18  White:::Not Hispanic or Latino                      Boston
#> 19  White:::Not Hispanic or Latino                      Boston
#> 20                            <NA>                        <NA>
#>    extension.valueAddress.country extension.valueAddress.state
#> 18                             US                Massachusetts
#> 19                             US                Massachusetts
#> 20                           <NA>                         <NA>
#>    extension.valueCode                extension.valueDecimal
#> 18                   M 4.160702818392717:::67.83929718160728
#> 19                   M 4.160702818392717:::67.83929718160728
#> 20                <NA>                                  <NA>
#>    extension.valueString gender generalPractitioner.reference     id
#> 18   Kristyn560 Lesch175   male           Practitioner/634104 634102
#> 19   Kristyn560 Lesch175   male           Practitioner/632760 632758
#> 20                  <NA>   male                          <NA> 921009
#>                                                                                                                                                                                                               identifier.system
#> 18 https://github.com/synthetichealth/synthea:::http://hospital.smarthealthit.org:::http://hl7.org/fhir/sid/us-ssn:::urn:oid:2.16.840.1.113883.4.3.25:::http://standardhealthrecord.org/fhir/StructureDefinition/passportNumber
#> 19 https://github.com/synthetichealth/synthea:::http://hospital.smarthealthit.org:::http://hl7.org/fhir/sid/us-ssn:::urn:oid:2.16.840.1.113883.4.3.25:::http://standardhealthrecord.org/fhir/StructureDefinition/passportNumber
#> 20                                                                                                                                                                                                                         <NA>
#>    identifier.type.coding.code
#> 18          MR:::SS:::DL:::PPN
#> 19          MR:::SS:::DL:::PPN
#> 20                        <NA>
#>                                                         identifier.type.coding.display
#> 18 Medical Record Number:::Social Security Number:::Driver's License:::Passport Number
#> 19 Medical Record Number:::Social Security Number:::Driver's License:::Passport Number
#> 20                                                                                <NA>
#>                                                                                                                                                                    identifier.type.coding.system
#> 18 http://terminology.hl7.org/CodeSystem/v2-0203:::http://terminology.hl7.org/CodeSystem/v2-0203:::http://terminology.hl7.org/CodeSystem/v2-0203:::http://terminology.hl7.org/CodeSystem/v2-0203
#> 19 http://terminology.hl7.org/CodeSystem/v2-0203:::http://terminology.hl7.org/CodeSystem/v2-0203:::http://terminology.hl7.org/CodeSystem/v2-0203:::http://terminology.hl7.org/CodeSystem/v2-0203
#> 20                                                                                                                                                                                          <NA>
#>                                                                   identifier.type.text
#> 18 Medical Record Number:::Social Security Number:::Driver's License:::Passport Number
#> 19 Medical Record Number:::Social Security Number:::Driver's License:::Passport Number
#> 20                                                                                <NA>
#>                                                                                                      identifier.value
#> 18 41166989-975d-4d17-b9de-17f94cb3eec1:::41166989-975d-4d17-b9de-17f94cb3eec1:::999-17-8717:::S99933732:::X75257608X
#> 19 41166989-975d-4d17-b9de-17f94cb3eec1:::41166989-975d-4d17-b9de-17f94cb3eec1:::999-17-8717:::S99933732:::X75257608X
#> 20                                                                                                               <NA>
#>    managingOrganization.reference maritalStatus.coding.code
#> 18            Organization/634103                         M
#> 19            Organization/632759                         M
#> 20                           <NA>                      <NA>
#>    maritalStatus.coding.display
#> 18                            M
#> 19                            M
#> 20                         <NA>
#>                               maritalStatus.coding.system maritalStatus.text
#> 18 http://terminology.hl7.org/CodeSystem/v3-MaritalStatus                  M
#> 19 http://terminology.hl7.org/CodeSystem/v3-MaritalStatus                  M
#> 20                                                   <NA>               <NA>
#>                 meta.lastUpdated       meta.source meta.versionId
#> 18 2020-03-02T10:43:58.614+00:00 #uTFlWjr2fcHf62Xy              1
#> 19 2020-02-29T18:47:48.754+00:00 #dM9HGNXA1F2Yo6lM              1
#> 20 2020-04-01T13:01:17.522+00:00 #WYbX5i8RxFerjmgY              3
#>    multipleBirthBoolean           name.family  name.given name.prefix
#> 18                false         Stiedemann542    Aaron697         Mr.
#> 19                false         Stiedemann542    Aaron697         Mr.
#> 20                 <NA> Melonseed:::Melonseed Edward:::Ed        <NA>
#>                          name.text         name.use telecom.system telecom.use
#> 18                            <NA>         official          phone        home
#> 19                            <NA>         official          phone        home
#> 20 Edward Melonseed:::Ed Melonseed official:::usual          phone      mobile
#>    telecom.value text.status
#> 18  555-213-2064   generated
#> 19  555-213-2064   generated
#> 20  (08)97654321   generated
```

As you can see, the result now contains two tables, one for Patient
resources and one for MedicationStatement resources.

## Multiple entries

A particularly complicated problem in flattening FHIR resources is
caused by the fact that there can be multiple entries to an attribute.
For a more detailed description of this problem, please see the vignette
on flattening resources.

In general, `fhir_crack()` will paste multiple entries for the same
attribute together in the table, using the separator provided by the
`sep` argument.

Let’s have a look at the following simple example, where we have a
bundle containing just two Patient resources. The example is part of the
`fhircrackr` package and you can make it available like this:

``` r
bundles <- fhir_unserialize(bundles = example_bundles1)
```

This represents a bundle list with only one very simple bundle of just
two Patient resources which looks like this:

    <Bundle>

        <Patient>
            <id value='id1'/>
            <address>
                <use value='home'/>
                <city value='Amsterdam'/>
                <type value='physical'/>
                <country value='Netherlands'/>
            </address>
            <name>
                <given value='Marie'/>
            </name>
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
            <name>
                <given value='Frank'/>
            </name>
            <name>
                <given value='Max'/>
            </name>         
        </Patient>      

    </Bundle>

The first resource has just one entry for the address attribute. The
second Patient resource has an address attribute with three entries
containing different elements and also two entries for the name
attribute.

This is where the style elements of the `table_description` comes into
play:

``` r
table_description <- fhir_table_description(
    resource = "Patient",
    brackets      = c("[", "]"),
    sep           = " | ",
    rm_empty_cols = FALSE,
    format        = 'compact',
    keep_attr     = FALSE
)

df <- fhir_crack(bundles = bundles, design = table_description, verbose = 0)

df
#>                address.city            address.country
#> 1            [1.1]Amsterdam           [1.1]Netherlands
#> 2 [1.1]Berlin | [3.1]London [2.1]France | [3.1]England
#>                address.type           address.use     id            name.given
#> 1             [1.1]physical             [1.1]home [1]id1            [1.1]Marie
#> 2 [2.1]postal | [3.1]postal [1.1]home | [3.1]work [1]id3 [1.1]Frank | [2.1]Max
```

Multiple entries are pasted together with the specified separator string
(in this case: `" | "`) in between and the indices (inside the specified
bracket strings (here: `"["` and `"]"`)) display the entry the value
belongs to. That way you can see that Patient resource 2 had three
entries for the attribute `address` and you can also see which
attributes belong to which entry.

## Process Tables with multiple Entries

### Melt tables with multiple entries

If the table produced by `fhir_crack()` contains multiple entries,
you’ll probably want to divide these entries into distinct observations
at some point. This is where `fhir_melt()` comes into play.
`fhir_melt()` takes an *indexed table* with multiple entries in one or
several `columns` and spreads (aka melts) these entries over several
rows.

``` r
fhir_melt(
    indexed_data_frame = df,
    columns            = "address.city",
    brackets           = c("[", "]"),
    sep                = " | ",
    all_columns        = FALSE
)
#>   resource_identifier address.city
#> 1                   1 [1]Amsterdam
#> 2                   2    [1]Berlin
#> 3                   2         <NA>
#> 4                   2    [1]London
```

The new variable `resource_identifier` maps which rows in the created
table belong to which row (usually equivalent to one resource) in the
original table. `brackets` and `sep` have to be the same character
vectors that have been used to build the indices with `fhir_crack()`.
`columns` is a character vector with the names of the variables/columns
you want to melt. You can provide more than one column here but it makes
sense to only have variables from the same repeating attribute together
in one call to `fhir_melt()`:

``` r
cols <- c("address.city", "address.use", "address.type", "address.country")

fhir_melt(
    indexed_data_frame = df,
    columns            = cols,
    brackets           = c("[", "]"),
    sep                = " | ",
    all_columns        = FALSE
)
#>   resource_identifier address.city address.use address.type address.country
#> 1                   1 [1]Amsterdam     [1]home  [1]physical  [1]Netherlands
#> 2                   2    [1]Berlin     [1]home         <NA>            <NA>
#> 3                   2         <NA>        <NA>    [1]postal       [1]France
#> 4                   2    [1]London     [1]work    [1]postal      [1]England
```

With the argument `all_columns` you can control whether the resulting
table contains only the molten columns or all columns of the original
table:

``` r
molten <- fhir_melt(
    indexed_data_frame = df,
    columns            = cols,
    brackets           = c("[", "]"),
    sep                = " | ",
    all_columns        = TRUE
)

molten
#>   address.city address.country address.type address.use     id
#> 1 [1]Amsterdam  [1]Netherlands  [1]physical     [1]home [1]id1
#> 2    [1]Berlin            <NA>         <NA>     [1]home [1]id3
#> 3         <NA>       [1]France    [1]postal        <NA> [1]id3
#> 4    [1]London      [1]England    [1]postal     [1]work [1]id3
#>              name.given resource_identifier
#> 1            [1.1]Marie                   1
#> 2 [1.1]Frank | [2.1]Max                   2
#> 3 [1.1]Frank | [2.1]Max                   2
#> 4 [1.1]Frank | [2.1]Max                   2
```

Values on the other variables will just repeat in the newly created
rows. For more information please see the vignette on flattening
resources.

### Remove indices

Once you have sorted out the multiple entries, you might want to get rid
of the indices in your data frame. This can be achieved using
`fhir_rm_indices()`:

``` r
fhir_rm_indices(indexed_data_frame = molten, brackets = c("[", "]"))
#>   address.city address.country address.type address.use  id  name.given
#> 1    Amsterdam     Netherlands     physical        home id1       Marie
#> 2       Berlin            <NA>         <NA>        home id3 Frank | Max
#> 3         <NA>          France       postal        <NA> id3 Frank | Max
#> 4       London         England       postal        work id3 Frank | Max
#>   resource_identifier
#> 1                   1
#> 2                   2
#> 3                   2
#> 4                   2
```

Again, `brackets` should be given the same character vector that was
used for `fhir_crack()` and `fhir_melt()` respectively.

## Save and load downloaded bundles

Since `fhir_crack()` ignores all data that are not specified in
`design`, it makes sense to store the original search result for
reproducibility and in case you realize later on that you need elements
from the resources that you haven’t extracted at first.

There are two ways of saving the FHIR bundles you downloaded: Either you
save them as R objects, or you write them to an xml file.

### Save and load bundles as R objects

If you want to save the list of downloaded bundles as an `.rda` or
`.RData` file, you can’t just use R’s `save()` or `save_image()` on it,
because this will break the external pointers in the xml objects
representing your bundles. Instead, you have to serialize the bundles
before saving and unserialize them after loading. For single xml objects
the package `xml2` provides serialization functions. For convenience,
however, `fhircrackr` provides the functions `fhir_serialize()` that can
be used directly on the bundles returned by `fhir_search()` and
`fhir_unserialize()`:

``` r
#serialize bundles
serialized_bundles <- fhir_serialize(bundles = patient_bundles)

#have a look at them
head(serialized_bundles[[1]])
#> [1] 58 0a 00 00 00 03
```

``` r
#create temporary directory for saving
temp_dir <- tempdir()

#save
saveRDS(serialized_bundles, file = paste0(temp_dir, "/bundles.rda"))
```

If you reload this bundle, you have to unserialize it before you can
work with it:

``` r
#load bundles
serialized_bundles_reloaded <- readRDS(paste0(temp_dir, "/bundles.rda"))
```

``` r
#unserialize
bundles <- fhir_unserialize(bundles = serialized_bundles_reloaded)

#have a look
bundles
#> An object of class "fhir_bundle_list"
#> [[1]]
#> A fhir_bundle_xml object
#> No. of entries : 20
#> Self Link: http://hapi.fhir.org/baseR4/Patient
#> Next Link: http://hapi.fhir.org/baseR4?_getpages=ce958386-53d0-4042-888c-cad53bf5d5a1 ...
#> 
#> {xml_node}
#> <Bundle>
#>  [1] <id value="ce958386-53d0-4042-888c-cad53bf5d5a1"/>
#>  [2] <meta>\n  <lastUpdated value="2021-05-10T12:12:43.317+00:00"/>\n</meta>
#>  [3] <type value="searchset"/>
#>  [4] <link>\n  <relation value="self"/>\n  <url value="http://hapi.fhir.org/b ...
#>  [5] <link>\n  <relation value="next"/>\n  <url value="http://hapi.fhir.org/b ...
#>  [6] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837602"/ ...
#>  [7] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/example-r ...
#>  [8] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837624"/ ...
#>  [9] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837626"/ ...
#> [10] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837631"/ ...
#> [11] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837716"/ ...
#> [12] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837720"/ ...
#> [13] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837714"/ ...
#> [14] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837721"/ ...
#> [15] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837722"/ ...
#> [16] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837723"/ ...
#> [17] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837724"/ ...
#> [18] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/cfsb16116 ...
#> [19] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837736"/ ...
#> [20] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837737"/ ...
#> ...
#> 
#> [[2]]
#> A fhir_bundle_xml object
#> No. of entries : 20
#> Self Link: http://hapi.fhir.org/baseR4?_getpages=ce958386-53d0-4042-888c-cad53bf5d5a1 ...
#> Next Link: http://hapi.fhir.org/baseR4?_getpages=ce958386-53d0-4042-888c-cad53bf5d5a1 ...
#> 
#> {xml_node}
#> <Bundle>
#>  [1] <id value="ce958386-53d0-4042-888c-cad53bf5d5a1"/>
#>  [2] <meta>\n  <lastUpdated value="2021-05-10T12:12:43.317+00:00"/>\n</meta>
#>  [3] <type value="searchset"/>
#>  [4] <link>\n  <relation value="self"/>\n  <url value="http://hapi.fhir.org/b ...
#>  [5] <link>\n  <relation value="next"/>\n  <url value="http://hapi.fhir.org/b ...
#>  [6] <link>\n  <relation value="previous"/>\n  <url value="http://hapi.fhir.o ...
#>  [7] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837760"/ ...
#>  [8] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837766"/ ...
#>  [9] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837768"/ ...
#> [10] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837781"/ ...
#> [11] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837783"/ ...
#> [12] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837784"/ ...
#> [13] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837787"/ ...
#> [14] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837788"/ ...
#> [15] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837789"/ ...
#> [16] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837790"/ ...
#> [17] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837791"/ ...
#> [18] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837792"/ ...
#> [19] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837793"/ ...
#> [20] <entry>\n  <fullUrl value="http://hapi.fhir.org/baseR4/Patient/1837794"/ ...
#> ...
```

After unserialization, the pointers are restored and you can continue to
work with the bundles. Note that the example bundles
`medication_bundles` and `patient_bundles` that are provided with the
`fhircrackr` package are also provided in their serialized form and have
to be unserialized as described on their help page.

### Save and load bundles as xml files

If you want to store the bundles in xml files instead of R objects, you
can use the functions `fhir_save()` and `fhir_load()`. `fhir_save()`
takes a list of bundles in form of xml objects (as returned by
`fhir_search()`) and writes them into the directory specified in the
argument `directory`. Each bundle is saved as a separate xml-file. If
the folder defined in `directory` doesn’t exist, it is created in the
current working directory.

``` r
#save bundles as xml files
fhir_save(bundles = patient_bundles, directory = temp_dir)
```

To read bundles saved with `fhir_save()` back into R, you can use
`fhir_load()`:

``` r
bundles <- fhir_load(directory = temp_dir)
```

`fhir_load()` takes the name of the directory (or path to it) as its
only argument. All xml-files in this directory are read into R and
returned as a list of bundles in xml format just as returned by
`fhir_search()`.

## Acknowledgements

This work was carried out by the SMITH consortium and the
cross-consortium use case POLAR\_MI; both are part of the German
Initiative for Medical Informatics and funded by the German Federal
Ministry of Education and Research (BMBF), grant no. 01ZZ1803A ,
01ZZ1803C and 01ZZ1910A.

[1] FHIR is the registered trademark of HL7 and is used with the
permission of HL7. Use of the FHIR trademark does not constitute
endorsement of this product by HL7
