# fhircrackr 2.2.1

## Bug fixes
- Errors when removing tags in `fhir_search()` (e.g. with `rm_tag = "div"`) now get caught and converted to a warning.

## New functions
- `fhir_collapse()` collapses multiple entries that belong to the same higher level FHIR element and should be kept together, e.g. `name.given` or `address.line`

- `fhir_melt_all()` divides all multiple entries into separate rows by repeatedly calling `fhir_melt()`.

Thanks to [@atruebi](https://github.com/astruebi) and [@Flow191](https://github.com/Flow191) who contributed the code for these functions!

# fhircrackr 2.2.0

## New functionalities
- `fhir_crack()` can now deal with XPath expressions using predicates, e.g. `code/coding[system[@value='http://loinc.org']]/code`
- deprecated argument `fhir_remove_empty_cols` fully removed

## New functions
- `as_fhir()` coerces a character vector with xml strings representing FHIR bundles to a fhir_bundles_list object.

# fhircrackr 2.1.1

## General

- Citation information now contains reference to ACI article
- Added HL7 trademark information to package description

## Bugs fixed

- `fhir_url()` now properly formats requests containing `=`, e.g. when searching for snomed expressions in coding elements

# fhircrackr 2.1.0

## Bugs fixed
- Relative next links for Paging: Bugs in connection with POST search and http-type search parameter values have been fixed.
- Examples querying a FHIR Server have been wrapped in `try()` to stop them from crashing during CRAN checks
- `fhir_search()` now prints the correct base URL in downloading messages even when `http://` type CodeSystems occur in search params.

## New functions/arguments
- `fhir_search()` has a new argument `add_headers` that allows adding custom headers to the HTTP request.
- `fhir_load()` can now load FHIR bundles with arbitrary file names from a given directory.

## New behavior
- `fhir_search()` will not url encode next links for paging anymore, as servers are expected to deliver a URL that can be sent back to them without any preprocessing steps. On all tested server types (HAPI, Vonk, Blaze) this change of code doesn't affect the paging behavior in any way.

# fhircrackr 2.0.0

## Bugs fixed 
- Erroneous warnings when using `fhir_search()` with token authentication have been fixed.
- `fhir_authenticate()` doesn't cache a `.httr-oauth` file anymore.
- `fhir_url()` now url encodes string type parameter values too.
- `fhir_search()` can now log errors from FHIR servers regardless of whether they are valid xml. 
- `fhir_search()` can now handle relative next links when paging through bundles

## New functions
- `fhir_build_bundle()`: Build a xml FHIR Bundle from a wide table.
- `fhir_build_resource()`: Build a xml FHIR resource from a wide table.
- `fhir_bundle_list()`: Constructor function to create `fhir_bundle_list` objects is now exported.
- `fhir_cast()`: Similar to `fhir_melt()` but spreads multiple entries across columns instead of rows.
- `fhir_count_resource()`: Count resources matching a specific search request on a server.
- `fhir_get_resource_ids()`: Extract the logical IDs of all FHIR resources matching a specific search request from a server.
- `fhir_get_resources_by_ids()`: Extract a set of resources based on their logical IDs.
- `fhir_post()`: POST an object to a FHIR server.
- `fhir_put()`: PUT an object to a FHIR server.
- `fhir_recent_http_error()`: Returns the most recent http error to the console.
- `fhir_request()`: Wrapper for `fhir_url()`
- `fhir_resource_xml()`: Constructor function to create `fhirresource_xml()` objects is now exported.
- `fhir_rm_div()`: Remove html elements from FHIR bundles.
- `fhir_rm_tag()`: Remove elements enclosed by a specified xml-tag from FHIR bundles.
- `fhir_sample_resources_by_ids()`: Randomly sample resources from a given logical ID vector and download them from a FHIR server.
- `fhir_sample_resources()`: Randomly sample from a set of resources matching a specific search request in a FHIR server.
- `fhir_tree()`: Returns a string representing the tree structure implicit in the column names of a wide table.
- `pastep()`: Concatenates two or more strings to a path string correctly.



## New behavior
- `fhir_melt()` has been rewritten to be faster and more memory efficient.

- `fhir_style`: This class is now deprecated, all the information contained in the `fhir_style` object has been moved to the `fhir_table_description`. 

- `fhir_table_description`: 
	- The structure of a `fhir_table_description` has changed insofar that the elements that used to be part of `fhir_style` are now direct elements of `fhir_table description`. 
	- There are two new elements to describe the created table: `format` and `keep_attr`. For more information see `?fhir_table_description`

- `fhir_crack()`: 
	- Rewritten to be faster and more efficient, can now crack bundles in parallel (only on linux) when argument `ncores > 1`. 
	- Can now optionally return tables in `wide` format where multiple entries are distributed over several columns instead of being pasted together in one column (argument `format`).
	- Will now extract elements regardless of their xml-attribute (`@value`, `@id`, `@url`), attributes can be attached to the column names when argument `keep_attr = TRUE`.
	- Argument `remove_empty_columns` renamed to `rm_empty_cols`.

- `fhir_search()`: 
	- New argument `rm_tag="div"` per default removes all html bits in the bundle immediately after download. Can be disabled by setting `rm_tag=NULL`.
	
	- `delay_between_attempts` can now be a vector with different waiting times between retries of reaching the server. The length of `delay_between_attempts` determines the number of retries.

	- `max_attempts` is deprecated in favor of `delay_between_attempts`.

- `fhir_load()` and `fhir_save()` have a new argument `max_bundles` that allows to restrict the number of bundles that is loaded/saved.

---------------------------------------

# fhircrackr 1.0.0
The package is now rewritten using S4 classes. Almost all of the code written with fhircrackr < 1.0.0 will still work, though in a few cases the user will be prompted to change their code to the new syntax.

The most prominent change is how to create a `design` that tells `fhir_crack()` how to flatten HL7 FHIR® Resources. It should now be created using the functions `fhir_table_description()` and `fhir_design()`. `fhir_crack()` is now also able to create a single data.frame/data.table instead of a list of tables when just one resource type is extracted. Please see the documentation under `?fhir_table_description` and `?fhir_design`! 

To get an overview about how the general workflow has changed, please have a look at the intro vignette (`vignette("fhircrackr_intro", package="fhircrackr")`).

There are a couple of new constructor functions for the newly defined classes which will not be listed here. Please have look at the package vignettes which go through them in detail. 

Other new functions or behavior are listed in the following:

## New functions
- `fhir_authenticate()`: Set up authentication using OAuth2/OpenID Connect
- `fhir_current_request()`: Get search request used in most recent call to `fhir_search()`or `fhir_url()`

## New behavior
- `fhir_search()` now allows for a search via POST via the argument `body`.

- `fhir_search()` can now handle bearer token authentication via the argument `token`.

- Argument `save_to_file` in `fhir_search()` now takes `NULL` or a string with a directory name and saves the bundles only if there is a specified directory. For backwards compatibility `TRUE`/`FALSE` in combination with `directory` are still allowed but discouraged with a warning.

- Argument `log_errors` in `fhir_search()` now takes a string with a filename and writes an xml (no tables anymore) to the specified file. For backwards compatibility numbers are still allowed but discouraged with a warning. 

- New argument `delay_between_bundles` for `fhir_search()` allows to put a delay between the download of bundles (i.e. pages) in a bigger search request to prevent weak servers from choking.

- The output of `fhir_capability_statement()` is slightly restructured: The names of the tables and their structure has changed.

- The deprecated argument `add_indices` in `fhir_crack()` is now fully removed.

- The most recently used FHIR search request is now implicitly saved whenever `fhir_search()` or `fhir_url()` is called. It can be accessed with the new function `fhir_current_request()`.

- The default value of `rm_empty_cols` is now `FALSE`

------------------------------------

# fhircrackr 0.2.1

- fhir_melt now gives a warning when the brackets provided in the function call don't appear in the data frame 

- A number of bugs have been fixed 

	- unintended type changes from data.frame to data.table are now prevented
	- `fhir_melt()` now takes the name provided in the argument `id_name` (which because of a bug it didn't before)
	- inconsistencies in assignment of default values to the design for `fhir_crack()` have been cleared
	- fixed bug causing column names to disappear when `cols` element of design was of length one
	

------------------------------


# fhircrackr 0.2.0

- design for `fhir_crack()` has new form now:

   1. has now named elements `resource`, `cols`, `style` (with style elements `sep`, `brackets`, `rm_empty_cols`)

   2. old versions of design still work

- new function `fhir_canonical_design()` returns the full (potentially automatically completed) design of the most recent call to `fhir_crack()`

- argument `add_indices` of `fhir_crack()` is now deprecated, indices will be added when `brackets` is not NULL

- new argument `columns` of `fhir_rm_indices()` gives control over the columns in which indices should be removed

- new functions `fhir_save_design()` and `fhir_load_design()` for saving/loading design as xml-document

- new function `fhir_next_bundle_url()` returns next-link of the last bundle processed by the most recent call to `fhir_search()`

- new arguments `save_to_disc` and `directory` of `fhir_search()` allow for saving bundles consecutively as xml files instead of loading them into the R session all at once

- Faster results of `fhir_crack()` because it now uses data.table internally

- new argument `data.table` of `fhir_crack` to choose between data.frame vs. data.table as output format

-----------------------------------------------

# fhircrackr 0.1.1

- fixed errors in `fhir_crack()` when resource type doesn't appear in bundle

- handle errors caused by the accidental use of serialized objects more gracefully

- `@value` at the end of an XPath expression pointing to an attribute for `design` used in `fhir_crack()` is now optional and will be added automatically by fhir_crack if omitted

- column names automatically generated by `fhir_crack()` are now shorter.

----------------------------------------

# fhircrackr 0.1.0

First Release of R-Package fhircrackr v0.1.0
