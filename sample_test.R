rm(list = ls())
devtools::load_all()
for(p in c(
	"data.table",
	"dplyr"
)
)library(p, character.only = T)

endpoints <- list(
#	azure = "http://nprogram.azurewebsites.net/",
	hapi = "https://hapi.fhir.org/baseR4",
	agiop = "https://mii-agiop-3p.life.uni-leipzig.de/fhir",
	blaze = "https://mii-agiop-3p.life.uni-leipzig.de/blaze",
	vonk  = "https://vonk.fire.ly/R4"
)

endpoint <- endpoints$agiop
resource_name <- "Patient"
sep <- " <~> "
brackets <- c("[:", ":]")
bundle_size <- 5

# bundles <- fhir_search(paste0(paste_paths(endpoint, resource_name), "?_count=", bundle_size), max_bundles = 3, verbose = 2)
# tables  <- fhir_crack(bundles, fhir_table_description(res_name, style = fhir_style(sep, brackets)))
# names(tables)

# v1 <- sample_resources_vonk(endpoint = endpoints$vonk, resource_name = resource_name, number_of_resources = 20, bundle_size = 5, seed = 10)
# v2 <- sample_resources_vonk(endpoint = endpoints$vonk, resource_name = resource_name, number_of_resources = 20, bundle_size = 5, seed = 10)
#
# dv1 <- fhir_crack(bundles = v1, design = fhir_table_description(resource_name))
# dv2 <- fhir_crack(bundles = v2, design = fhir_table_description(resource_name))
#
# identical(dv1, dv2)
#
#
# h1 <- sample_resources_vonk(endpoint = endpoints$hapi, resource_name = resource_name, number_of_resources = 20, bundle_size = 5, seed = 10)
# h2 <- sample_resources_vonk(endpoint = endpoints$hapi, resource_name = resource_name, number_of_resources = 20, bundle_size = 5, seed = 10)
#
# dh1 <- fhir_crack(bundles = h1, design = fhir_table_description(resource_name))
# dh2 <- fhir_crack(bundles = h2, design = fhir_table_description(resource_name))
#
# identical(dh1, dh2)
#
# b <- fhir_search("http://nprogram.azurewebsites.net/Patient?_id=1", verbose = 2, max_bundles = 1)




get_resource_count(endpoints$hapi, "Patient")
get_resource_count(endpoints$hapi, "Observation")
get_resource_count(endpoints$hapi, "Observation")
get_resource_count(endpoints$vonk, "Patient")
#get_resource_count(endpoint = endpoints$azure, "Observation")

(ids_obs_agiop <- get_resource_ids(endpoint = endpoints$agiop, resource_name = "Observation"))
(ids_obs_blaze <- get_resource_ids(endpoint = endpoints$blaze, resource_name = "Observation"))
(ids_obs_hapi  <- get_resource_ids(endpoint = endpoints$hapi, resource_name = "Observation")) #hapi schickt nur knapp 12500 ids
(ids_obs_vonk  <- get_resource_ids(endpoint = endpoints$vonk, resource_name = "Observation")) #hapi schickt nur knapp 12500 ids
#(ids_obs_azure <- get_resource_ids(endpoint = endpoints$azure, resource_name = "Observation")) #

resource_name <- "Patient"
fhir_crack(get_samples(endpoint = endpoints$agiop, resource_name, sample_size = 10, bundle_size = 5, seed = 2), fhir_table_description(resource_name))
resource_name <- "Patient"
fhir_crack(get_samples(endpoints$vonk, resource_name, 100, 10, 2), fhir_table_description(resource_name))
resource_name <- "Observation"

resource_name <- "Observation"
start <- Sys.time()
s <- get_samples(
	endpoint = endpoints$hapi,
	resource_name = resource_name,
	sample_size = 10000,
	bundle_size = 500,
	seed = 1000)
Sys.time() - start

start <- Sys.time()
d <- fhir_crack(s, fhir_table_description(resource_name))
Sys.time() - start


d2 <- fhir_crack(get_samples(endpoints$hapi, resource_name, 1000, 1000), fhir_table_description(resource_name))
identical(d1, d2)

d1 <- fhir_crack(get_samples(endpoints$vonk, resource_name, 100, 1000), fhir_table_description(resource_name))
d2 <- fhir_crack(get_samples(endpoints$vonk, resource_name, 100, 1000), fhir_table_description(resource_name))
identical(d1, d2)

d1 <- fhir_crack(get_samples(endpoints$vonk, resource_name, 100, 1001), fhir_table_description(resource_name))
d2 <- fhir_crack(get_samples(endpoints$vonk, resource_name, 100, 1000), fhir_table_description(resource_name))
identical(d1, d2)

d1
d2
