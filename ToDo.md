
# Plans for next Version of fhircrackr


## Simplify fhir_crack

- deprecate add_indices (information is taken from brackets argument)

- rename argument designs to design

- Arguments remove_empty_columns, brackets and sep can be provided within design 

- fhir_crack should be able to create single df (not list of dfs) where possible

- update is_invalid_design accordingly

- update documentation, vignette and readme accordingly



## Performance Issues

- Find out how quickly functions break down when there are a lot of resources

- Consider changing from data.frames to data.tables

- Maybe add option to write intermediate results to disk temporarily?


## Transition to S3 classes

- S3 vs S4?

