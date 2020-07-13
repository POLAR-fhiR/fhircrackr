
# Plans for next Version of fhircrackr

## Simplify fhir_crack

- fhir_crack should be able to create single df (not list of dfs) where possible

- add function to fhir_crack that automatically recognises whether design suggests single df or list of dfs

- update is_invalid_design accordingly

- update documentation, vignette and readme accordingly

## Performance Issues

- Find out how quickly functions break down when there are a lot of resources

- Consider changing from data.frames to data.tables

- Maybe add option to write intermediate results to disk temporarily?


## Transition to S3 classes

- S3 vs S4?

