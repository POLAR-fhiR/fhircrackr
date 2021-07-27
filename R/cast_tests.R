rm(list = ls())

source("R/cast.R")

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
		<address>
		</address>
		<birthDate value='1980-05-23'/>
	</Patient>

	<Patient>
		<id value='id3'/>
		<address>
		</address>
		<birthDate value='1980-05-24'/>
		<birthDate value='1980-05-25'/>
	</Patient>

	<Patient>
		<id value='id4'/>
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
		<birthDate value='1978-11-13'/>
		<birth_date value='1918-11-13'/>
	</Patient>

	<Patient>
		<id value='id5'/>
	</Patient>

	<Patient>
	</Patient>

	<Patient>
		<ID value='id7'/>
	</Patient>

</Bundle>"
)

desc.patients <- fhir_table_description(
	resource = "Patient",
	style = fhir_style(
		brackets = c("<(", ")>"),
		sep = " <~> "
	)
)

(df.patients <- fhir_crack(
	bundles = list(bundle),
	design = desc.patients,
	remove_empty_columns = FALSE))


(df.patients_cast <- fhir_cast(indexed_df = df.patients, sep = desc.patients@style@sep, brackets = desc.patients@style@brackets, verbose = 1, keep_1st_index = T, shift_index = 0, use_brackets = T))
(df.patients_cast <- fhir_cast(indexed_df = df.patients, sep = desc.patients@style@sep, brackets = desc.patients@style@brackets, verbose = 1, keep_1st_index = F, shift_index = -1, use_brackets = T))
(df.patients_cast <- fhir_cast(indexed_df = df.patients, sep = desc.patients@style@sep, brackets = desc.patients@style@brackets, verbose = 1, keep_1st_index = T, shift_index = 0, use_brackets = F))
(df.patients_cast <- fhir_cast(indexed_df = df.patients, sep = desc.patients@style@sep, brackets = desc.patients@style@brackets, verbose = 1, keep_1st_index = T, shift_index = -1, use_brackets = F))
(df.patients_cast <- fhir_cast(indexed_df = df.patients, sep = desc.patients@style@sep, brackets = desc.patients@style@brackets, verbose = 1))
(df.patients_cast <- fhir_cast(indexed_df = df.patients, sep = desc.patients@style@sep, brackets = desc.patients@style@brackets, verbose = 1, keep_1st_index = T, shift_index = 0, use_brackets = F))
(tree.patients_cast <- build_tree(row = df.patients_cast[4,], root = "Patient"))
cat(tree2string(tre = tree.patients_cast))
cat(tree2string(tre = fit_tree_for_xml_document(tre = tree.patients_cast)))
print_tree(tre = fit_tree_for_xml_document(tre = tree.patients_cast))
print(xml2::as_xml_document(fit_tree_for_xml_document(tre = tree.patients_cast)))
cat(toString(xml2::as_xml_document(fit_tree_for_xml_document(tre = tree.patients_cast))))

tree_bundles <- build_tree_bundles(df = df.patients_cast, resource_name = "Patient", bundle_size = 2)
cat(toString(xml2::as_xml_document(tree_bundles$Bundle1)))
xml_bundles <- lapply(tree_bundles, function(b)xml2::as_xml_document(b))
fhir_crack(bundles = xml_bundles, fhir_table_description(resource = "Patient"), verbose = 2)

###########################################################################################################################
#endpoint <- "https://mii-agiop-3p.life.uni-leipzig.de/fhir"
endpoint <- "https://hapi.fhir.org/baseR4"
resource_name <- "Observation"
bundle_size <- 500
xml_bundles <- fhir_search(paste0(paste_paths(endpoint, resource_name), "?_count=", bundle_size), max_bundles = 5, verbose = 2)

sep <- " <~> "
brackets <- c("<|", "|>")
style <- fhir_style(sep = sep, brackets = brackets)
descr <- fhir_table_description(resource = resource_name, style = style)
table_orig <- fhir_crack(bundles = xml_bundles, design = descr, data.table = T)
cast_table_orig <- fhir_cast(table_orig, sep = sep, brackets = brackets)

cat(frame_string("Version 1"))
start_total <- Sys.time()
start <- Sys.time()
tree_bundles <- build_tree_bundles(df = cast_table_orig, resource_name = resource_name, bundle_size = bundle_size)
Sys.time() - start
start <- Sys.time()
xml_bundles1 <- lapply(tree_bundles, function(b) xml2::as_xml_document(b))
Sys.time() - start
Sys.time() - start_total

cat(frame_string("Version 2"))
start_total <- Sys.time()
start <- Sys.time()
tree_bundles <- build_tree_bundles(df = cast_table_orig, resource_name = resource_name, bundle_size = bundle_size)
Sys.time() - start
start <- Sys.time()
fit_tree <- lapply(tree_bundles, fit_tree_for_xml_document)
Sys.time() - start
start <- Sys.time()
xml_bundles_str <- lapply(fit_tree, tree2xml)
Sys.time() - start
start <- Sys.time()
xml_bundles2 <- lapply(xml_bundles_str, xml2::read_xml)
Sys.time() - start
Sys.time() - start_total

cat(frame_string("Version 2 short"))
start_total <- Sys.time()
start <- Sys.time()
tree_bundles <- build_tree_bundles(df = cast_table_orig, resource_name = resource_name, bundle_size = bundle_size)
Sys.time() - start
start <- Sys.time()
xml_bundles2s <- lapply(tree_bundles, function(b) xml2::read_xml(tree2xml(fit_tree_for_xml_document(b))))
Sys.time() - start
Sys.time() - start_total

cat(frame_string("Version 3"))
start <- Sys.time()
xml_bundles3 <- build_xml_bundles(cast_table = cast_table_orig, resource_name = resource_name, bundle_size = bundle_size)
Sys.time() - start


sep <- " <~> "
brackets <- c("<|", "|>")
#endpoint <- "https://hapi.fhir.org/baseR4"
#endpoint <- "https://hapi.fhir.org/baseR4"
endpoint <- "https://mii-agiop-3p.life.uni-leipzig.de/fhir"
bundle_size <- 500
#
cs <- fhir_capability_statement(endpoint, verbose = 2, sep = sep)

totals <- sapply(
	cs$Resources$type,
	function(res_) {
		cat(res_, "\n")
		b <- fhir_search(paste0(endpoint, "/", res_, "?_summary=", bundle_size), verbose = 0)
		d <- fhir_table_description(
			resource = "Bundle",
			cols = c("total" = "./total")
		)
		df <- fhir_crack(b, d, verbose = 0)
		as.numeric(df$total)
	}
)

totals <- totals[0 < totals]
(totals <- totals[order(totals, decreasing = T)])
totals_small <- totals[totals <= 10000]

all_data <- lapply(
	lst(names(totals_small)),
	function(resource_name) {
		#resource_name <- lst(names(totals_small))[1]
		cat(frame_string(text = resource_name))
		cat(frame_string("get original bundles"))
		bundles_orig <- fhir_search(paste0(endpoint, "/", resource_name, "?_count=", bundle_size), verbose = 2, max_bundles = 20)
		#fhir_save(bundles = bundles_orig, paste0("tmp/bundles/", resource_name))

		cat(frame_string("crack original bundles"))
		table_desc <- fhir_table_description(resource_name, style = fhir_style(sep = sep, brackets = brackets))
		table_orig <- fhir_crack(bundles = bundles_orig, table_desc, verbose = 2, data.table = T)

		cat(frame_string("cast table"))
		table_cast <- fhir_cast(table_orig, table_desc@style@sep, table_desc@style@brackets, verbose = 1)
		cat(frame_string("build new bundles"))

		bundles_new <- build_xml_bundles(cast_table = table_cast, resource_name = resource_name, bundle_size = bundle_size)
		cat(frame_string("build new bundles list for newly crack"))

		cat(frame_string("crack new bundles"))
		table_new <- fhir_crack(bundles = bundles_new, design = table_desc, verbose = 2, data.table = T)

		list(orig = table_orig, new = table_new)
	}
)

cat(frame_string("check whether original and new tables are identical"))


# (all_data_eq <- sapply(
# 	all_data,
# 	function(ad) {
# 		identical(ad$orig, ad$new)
# 	}
# ))
#
# all(all_data_eq)
#
# all_data_av <- lapply(
# 	lst(names(all_data)),
# 	function(n) {
# 		#n <- lst(names(all_data))[[1]]
# 		d <- all_data[[n]]$new
# 		e <- sapply(d, function(e)sum(!is.na(e)))
# 		build_tree(e)
# 	}
# )
#
# cat(frame_string(paste0("show counts of availables")))
#
# for(n in names(all_data_av)) {
# 	cat(
# 		frame_string(
# 			text = paste0(
# 				"\n", frame_string(n), "\n",
# 				frame_string(tree2string(all_data_av[[n]]))
# 			),
# 			side = "both"
# 		),
# 		"\n"
# 	)
# }
#
# all_data_not_av <- lapply(
# 	lst(names(all_data)),
# 	function(n) {
# 		d <- all_data[[n]]$new
# 		e <- sapply(d, function(e)sum(is.na(e)))
# 		build_tree(e)
# 	}
# )
#
# cat(frame_string("show counts of missings"))
#
# for(n in names(all_data_not_av)) {
# 	cat(
# 		frame_string(
# 			text = paste0(
# 				"\n", frame_string(n), "\n",
# 				frame_string(tree2string(all_data_not_av[[n]]))
# 			),
# 			side = "both"
# 		),
# 		"\n"
# 	)
# }

all_data_equality_summary <- lapply(
	lst(names(all_data)),
	function(n) {
		#n<-lst(names(all_data))[[1]]
		dori <- all_data[[n]]$orig
		dnew <- all_data[[n]]$new
		if(!all(names(dori) == names(dnew))) stop("names are not equal for ", n, " tables.")
		e <- sapply(
			names(dori),
			function(nm) {
				#nm <- names(dori)[[2]]
				#nm <- "code.coding.code"
				print(nm)
				do_av <- !is.na(dori[[nm]])
				dn_av <- !is.na(dnew[[nm]])
				if(all(do_av == dn_av)) {
					do <- dori[dn_av, ..nm]
					dn <- dnew[dn_av, ..nm]
					if(all(do == dn)) "equal in both tables" else "nor equal in both tables"
				} else "not the same missings"
			}
		)
		build_tree(e, n)
	}
)

cat(frame_string("show equality summaries"))
for(n in names(all_data_equality_summary)) {
	cat(
		frame_string(
			text = paste0(
				"\n", frame_string(n), "\n",
				frame_string(
					tree2string(all_data_equality_summary[[n]])
				)
			),
			side = "both"
		),
		"\n"
	)
}


all_data_availables_summary <- lapply(
	lst(names(all_data)),
	function(n) {
		d <- all_data[[n]]$new
		e <- sapply(d, function(e)paste0(sum(!is.na(e)), " + ", sum(is.na(e)), " = ", length(e), "  [ ", round(100 * sum(!is.na(e)) / length(e), 3), "% ]"))
		build_tree(e, n)
	}
)

cat(frame_string("show availables summaries"))
for(n in names(all_data_availables_summary)) {
	cat(
		frame_string(
			text = paste0(
				"\n", frame_string(n), "\n",
				frame_string(
					paste0(
						"availables + missings = total  [ percentage ]\n",
						tree2string(all_data_availables_summary[[n]])
					)
				)
			),
			side = "both"
		),
		"\n"
	)
}

