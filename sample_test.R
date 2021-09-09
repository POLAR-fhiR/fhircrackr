rm(list = ls())
devtools::load_all()
for(p in c(
	"data.table",
	"dplyr"
)
)library(p, character.only = T)

N <- 10

(ids <- sapply(1:N, function(x)paste0(sample(c(LETTERS[1:3], letters[4:10], 0:100), sample(1:10,1)), collapse = "-")))
order(ids)
sort(ids)
order_correctly_strings_with_numbers(ids)
sort_correctly_strings_with_numbers(s=ids)
endpoints <- list(
#	azure = "http://nprogram.azurewebsites.net/",
#	polar = "https://mii-agiop-polar.life.uni-leipzig.de/blaze",
	hapi = "https://hapi.fhir.org/baseR4",
	agiop = "https://mii-agiop-3p.life.uni-leipzig.de/fhir",
	blaze = "https://mii-agiop-3p.life.uni-leipzig.de/blaze",
	vonk  = "https://vonk.fire.ly/R4"
)

cap_stats <- lapply(
	endpoints,
	fhir_capability_statement
)

metas <- rbindlist(
	fill = T,
	lapply(
		cap_stats,
		function(x) {
		   data.table(x$Meta)
		}
	)
)
metas$endpoint <- names(endpoints)



rests <- rbindlist(
	fill = T,
	lapply(
		cap_stats,
		function(x) {
			data.table(x$Rest)
		}
	)
)
rests$endpoint <- names(endpoints)

endpoint <- endpoints$hapi
resource_name <- "Patient"
sep <- " <> "
brackets <- NULL#c("<|", "|>")
bundle_size <- 500
parameters <- c(
	gender = "female",
	name   = "Simpson"
)

bundles <- sample_resources(endpoint = endpoint, resource_name = resource_name, parameters = parameters, sample_size = 2000, verbose = 2)
dt <- fhir_crack(bundles, design = fhir_table_description(resource_name))
dt








ids <- get_resources_ids(
	endpoint = endpoint,
	resource_name = resource_name,
	parameters = parameters,
	verbose = 2
)

bundles <- get_resources_by_ids_post(
	endpoint = endpoint,
	resource_name = resource_name,
	ids = paste0(ids, collapse = ","),
	seed = 1,
	verbose = 2
)

dt <- fhir_crack(bundles, design = fhir_table_description(resource_name))

body <- fhir_body(
	content = list(
		"_id" = paste0(ids, collapse = ",")
	)
)

request <- fhir_url(
	url = endpoint,
	resource = resource_name
)

bundles <- fhir_search(
	request = request,
	body = body
)

dt <- fhir_crack(
	bundles = bundles,
	design = fhir_table_description(
		resource = resource_name,
		style = fhir_style(
			sep = sep,
			brackets = brackets,
			rm_empty_cols = T
		)
	),
	verbose = 2,
	data.table = T
)
dt

dt[dt$id %in% ids,]

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



(endpoint <- endpoints$blaze)
get_resources_count(endpoint, "Patient", list("gender" = "male"))
get_resources_count(endpoint, "Patient", c("gender" = "male"))
get_resources_count(endpoint, "Patient", "gender=male")
get_resources_count(endpoint, "Patient")

(m <- get_resources_count(endpoint, "Patient", c("gender" = "male")))
(f <- get_resources_count(endpoint, "Patient", c("gender" = "female")))
(a <- get_resources_count(endpoint = endpoint, resource_name = "Patient", parameters = "", verbose = 2))
a - m - f

resource_name <- "Patient"
parameters <- list(
	male = c("gender" = "male"),
	female = c("gender" = "female"),
	all = ""
)

for(endpoint in endpoints) {
	cat(endpoint, "\n")
	cat("males:              ", m <- get_resources_count(endpoint, resource_name = resource_name, parameters = parameters$male), "\n")
	cat("females:            ", f <- get_resources_count(endpoint, resource_name = resource_name, parameters = parameters$female), "\n")
	cat("all:                ", a <- get_resources_count(endpoint, resource_name = resource_name, parameters = parameters$all), "\n")
	cat("all - male - female:", a - m - f, "\n\n")
}

resource_name <- "Observation"
parameters <- list(
	male = c("patient.gender" = "male"),
	female = c("patient.gender" = "female"),
	all = ""
)

for(endpoint in endpoints) {
	cat(endpoint, "\n")
	cat("males:              ", m <- get_resources_count(endpoint, resource_name, parameters$male), "\n")
	cat("females:            ", f <- get_resources_count(endpoint, resource_name, parameters$female), "\n")
	cat("all:                ", a <- get_resources_count(endpoint, resource_name, parameters$all), "\n")
	cat("all - male - female:", a - m - f, "\n\n")
}


rn <- "Observation"
(ids_obs_agiop <- sort_correctly_strings_with_numbers(get_resources_ids(endpoint = endpoints$agiop, resource_name = rn)))
(ids_obs_blaze <- sort_correctly_strings_with_numbers(get_resources_ids(endpoint = endpoints$blaze, resource_name = rn))) #blaze laed die komplette resource herunter, kann kein _elements
(ids_obs_hapi  <- sort_correctly_strings_with_numbers(get_resources_ids(endpoint = endpoints$hapi, resource_name = rn, verbose = 2)))
(ids_obs_vonk  <- sort_correctly_strings_with_numbers(get_resources_ids(endpoint = endpoints$vonk, resource_name = rn, verbose = 2))) #hapi schickt nur knapp 12500 ids
#(ids_obs_azure <- get_resource_ids(endpoint = endpoints$azure, resource_name = rn)) #

resource_name <- "Patient"
patients_bundles1 <- sample_resources(
	resource_name = resource_name,
	endpoint = endpoints$vonk,
	sample_size = 500,
	seed = 1
)

patients_bundles2 <- sample_resources_fix_bundle_size(
	resource_name = resource_name,
	endpoint = endpoints$vonk,
	sample_size = 500,
	bundle_size = 250,
	seed = 1
)

(d1 <- fhir_crack(patients_bundles1, fhir_table_description(resource_name)))
(d2 <- fhir_crack(patients_bundles2, fhir_table_description(resource_name)))

d1 <- d1[order(d1$id),]
d2 <- d2[order(d2$id),]

identical(d1, d2)
identical(names(d1), names(d2))
all(
	sapply(
		names(d1),
		function(n){
			identical(d1[[n]], d2[[n]])
		}
	)
)

resource_name <- "Patient"
fhir_crack(sample_resources_fix_bundle_size(endpoints$vonk, resource_name, 100, 10, 2), fhir_table_description(resource_name))

start <- Sys.time()
s <- sample_resources_fix_bundle_size(
	endpoint = endpoints$hapi,
	resource_name = resource_name,
	sample_size = 10000,
	bundle_size = 1000,
	seed = 1000)
Sys.time() - start

start <- Sys.time()
s <- sample_resources(
	endpoint = endpoints$hapi,
	resource_name = resource_name,
	sample_size = 10000,
	seed = 1000)
Sys.time() - start

d1 <- fhir_crack(sample_resources_fix_bundle_size(endpoints$vonk, resource_name, 100, 1000, 1), fhir_table_description(resource_name))
d2 <- fhir_crack(sample_resources(endpoints$vonk, resource_name, 100, 1), fhir_table_description(resource_name))
d1 <- d1[order(d1$id),]
d2 <- d2[order(d2$id),]
identical(d1, d2)
all(
	sapply(
		names(d1),
		function(n) {
			identical(d1[[n]], d2[[n]])
		}
	)
)

d1 <- fhir_crack(sample_resources_fix_bundle_size(endpoints$vonk, resource_name, 500, 1000, 1), fhir_table_description(resource_name))
d2 <- fhir_crack(sample_resources_fix_bundle_size(endpoints$vonk, resource_name, 500, 1000, 2), fhir_table_description(resource_name))
identical(d1, d2)

d1
d2

sum(d1$id %in% d2$id)
sum(d2$id %in% d1$id)


