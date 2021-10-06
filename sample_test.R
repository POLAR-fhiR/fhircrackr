rm(list = ls())
devtools::load_all()
for(p in c(
	"data.table",
	"dplyr"
)
)library(p, character.only = T)

N <- 10

(ids <- sapply(1:N, function(x)paste0(sample(c(LETTERS[1:3], letters[4:6], 0:20 + 2**(0:20)), sample(1:10,1)), collapse = "-")))
order(ids)
order_strings_with_numbers_correctly(ids)
sort(ids)
sort_strings_with_numbers_correctly(s=ids)

endpoints <- list(
#	azure = "http://nprogram.azurewebsites.net/",
#	polar = "https://mii-agiop-polar.life.uni-leipzig.de/blaze",
	hapi = "https://hapi.fhir.org/baseR4",
	agiop = "https://mii-agiop-3p.life.uni-leipzig.de/fhir",
	blaze = "https://mii-agiop-3p.life.uni-leipzig.de/blaze",
	vonk  = "https://vonk.fire.ly/R4"
)
#
# cap_stats <- lapply(
# 	endpoints,
# 	fhir_capability_statement
# )
#
# metas <- rbindlist(
# 	fill = T,
# 	lapply(
# 		cap_stats,
# 		function(x) {
# 		   data.table(x$Meta)
# 		}
# 	)
# )
#
# metas$endpoint <- names(endpoints)
#
# rests <- rbindlist(
# 	fill = T,
# 	lapply(
# 		cap_stats,
# 		function(x) {
# 			data.table(x$Rest)
# 		}
# 	)
# )
#
# rests$endpoint <- names(endpoints)

endpoint <- endpoints$hapi
resource <- "Patient"
sep <- " <> "
brackets <- NULL#c("<|", "|>")
bundle_size <- 500
parameters_list <- c(
	name   = "Simpson",
	name   = "Bart,Homer,Marge,Lisa,Maggie,Ape,Mona"
)
(parameters <- paste_parameters(parameters_list))
n <- 5

###
# 3 times the same samples for the same seed = 1
###
equal <- lapply(
	1:n,
	function(loop) {
		bundles <- sample_resources(
			endpoint = endpoint,
			resource = resource,
			parameters = parameters,
			sample_size = 10,
			verbose = 0,
			seed = 1
		)
		dt <- fhir_crack(
			bundles,
			design = fhir_table_description(resource),
			verbose = 0,
			data.table = T
		)
		cols <- names(dt)[grepl("id|name|address", names(dt))]
		cols <- cols[!grepl("identifier|meta|text", cols)]
		dt <- dt[,..cols]
		print(n - loop)
		dt
	}
)
equal
###
# 10 times the different samples for every different seed
###
start = Sys.time()
different <- lapply(
	1:n,
	function(seed) {
		bundles <- sample_resources(
			endpoint = endpoint,
			resource = resource,
			parameters = parameters,
			sample_size = 10,
			verbose = 0,
			seed = seed
		)
		dt <- fhir_crack(
			bundles,
			design = fhir_table_description(resource),
			verbose = 0,
			data.table = T
		)
		cols <- names(dt)[grepl("id|name|address", names(dt))]
		cols <- cols[!grepl("identifier|meta|text", cols)]
		dt <- dt[,..cols]
		print(n - seed)
		dt
	}
)
Sys.time() - start
different

start = Sys.time()
ids <- get_resources_ids(endpoint = endpoint, resource = resource, parameters = parameters, verbose = 0)
different2 <- lapply(
	1:n,
	function(seed) {
		bundles <- sample_identified_resources(
			endpoint = endpoint,
			resource = resource,
			ids = ids,
			sample_size = 10,
			verbose = 0,
			seed = seed
		)
		dt <- fhir_crack(
			bundles,
			design = fhir_table_description(resource),
			verbose = 0,
			data.table = T
		)
		cols <- names(dt)[grepl("id|name|address", names(dt))]
		cols <- cols[!grepl("identifier|meta|text", cols)]
		dt <- dt[,..cols]
		print(n - seed)
		dt
	}
)
Sys.time() - start
different2

identical(different, different2)

ids <- get_resources_ids(
	endpoint = endpoint,
	resource = resource,
	parameters = c("name" = "Marge"),
	verbose = 2
)

bundles <- get_resources_by_ids(
	endpoint = endpoint,
	resource = resource,
	ids = paste0(ids, collapse = ","),
	verbose = 2
)

dt <- fhir_crack(bundles = bundles, design = fhir_table_description(resource))
dt

resource <- "Patient"
parameters <- list(
	male = c("gender" = "male"),
	female = c("gender" = "female"),
	"male or female" = c("gender" = "male,female"),
	all = ""
)

counts_patients <- sapply(
	endpoints,
	function(endpoint) {
		cat(endpoint, "\n")
		nms <- names(parameters)
		len <- max(nchar(nms))
		cnts <- lapply(
			lst(names(parameters)),
			function(n) {
				m <- get_resources_count(endpoint, resource = resource, parameters = parameters[[n]])
				cat(stringr::str_pad(n, len, "right", " "), ":", m, "\n")
				m
			}
		)
		cnts[["not defined"]] <- cnts[["all"]] - cnts[["male or female"]]
		cat(stringr::str_pad("not defined", len, "right", " "), ":", cnts[["not defined"]], "\n")
		cnts
	}
)

counts_patients

resource <- "Observation"
parameters <- list(
	male = c("patient.gender" = "male"),
	female = c("patient.gender" = "female"),
	"male or female" = c("patient.gender" = "male,female"),
	all = ""
)

counts_observations <- sapply(
	endpoints,
	function(endpoint) {
		cat(endpoint, "\n")
		nms <- names(parameters)
		len <- max(nchar(nms))
		cnts <- lapply(
			lst(names(parameters)),
			function(n) {
				m <- get_resources_count(endpoint, resource = resource, parameters = parameters[[n]])
				cat(stringr::str_pad(n, len, "right", " "), ":", m, "\n")
				m
			}
		)
		cnts[["not defined"]] <- cnts[["all"]] - cnts[["male or female"]]
		cat(stringr::str_pad("not defined", len, "right", " "), ":", cnts[["not defined"]], "\n")
		cnts
	}
)

#Blaze cannot filter combined patient.gender for Observation
counts_observations


rn <- "Observation"
(ids_obs_agiop <- sort_strings_with_numbers_correctly(get_resources_ids(endpoint = endpoints$agiop, resource = rn)))
(ids_obs_blaze <- sort_strings_with_numbers_correctly(get_resources_ids(endpoint = endpoints$blaze, resource = rn))) #blaze laed die komplette resource herunter, kann kein _elements
(ids_obs_hapi  <- sort_strings_with_numbers_correctly(get_resources_ids(endpoint = endpoints$hapi, resource = rn, verbose = 2)))
(ids_obs_vonk  <- sort_strings_with_numbers_correctly(get_resources_ids(endpoint = endpoints$vonk, resource = rn, verbose = 2))) #hapi schickt nur knapp 12500 ids
