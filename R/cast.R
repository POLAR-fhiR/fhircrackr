rm(list = ls())

library(fhircrackr)
library(data.table)
library(dplyr)

fhir_cast <- function(indexed_df = df.patients, sep = desc.patients@style@sep, brackets = desc.patients@style@brackets, keep_1st_index = F, shift_index = 0, use_brackes = F, verbose = 0) {

	lst <- function(v){names(v)<-v;v}
	esc <- function(s) {gsub("([\\.|\\^|\\$|\\*|\\+|\\?|\\(|\\)|\\[|\\{|\\\\\\|\\|])", "\\\\\\1", s)}

	if(!inherits(indexed_df, "data.table")) setDT(indexed_df)

	df_names <- names(indexed_df)
	s <- strsplit(df_names, "\\.")
	names(s) <- df_names
	sep_ <- esc(sep)
	bra_ <- esc(brackets[1])
	ket_ <- esc(brackets[2])
	regexpr_ids <- paste0(bra_, "([0-9]+(\\.[0-9]+)*)", ket_, "(.*$)")
	if(0 < verbose) cat("Pass 1\n")
	map <- sapply(
		names(indexed_df),
		function(name) {
			if(0 < verbose) cat(name, "\n")
			entries <- strsplit(indexed_df[[name]], sep_)
			ids <- lapply(entries, function(entry)gsub(regexpr_ids, "\\1", entry))
			name_vec <- strsplit(name, "\\.")[[1]]
			u <- unlist(
				lapply(
					ids,
					function(id){
						if(1 < length(name_vec)) {
							sapply(
								id,
								function(i){
									if(!all(is.na(i))) {
										paste0(paste0(name_vec, strsplit(i, "\\.")[[1]]), collapse = ".")
									} else NULL
								},
								simplify = F
							)
						} else {
							if(!all(is.na(id))){
								a <- paste0(name_vec, id)
								names(a) <- id
								a
							} else NULL
						}
					}
				),
				use.names = T
			)
			u[unique(names(u))]
		},
		simplify = F
	)
	df_new_names <- unlist(map, use.names = F)
	d <- data.table(matrix(nrow = nrow(indexed_df), ncol = length(df_new_names))) %>% setnames(df_new_names)
	if(0 < verbose) cat("Pass 2\n")
	for(name in names(map)) {
		if(0 < verbose) cat(paste0(name, ":\n"))
		for(id in names(map[[name]])) {
			sname <- map[[name]][[id]]
			if(0 < verbose) cat("  ", sname, "\n")
			g <- grep(paste0(bra_, id, ket_), indexed_df[[name]], perl = T)
			s <- strsplit(indexed_df[[name]][g], sep_)
			p <- sapply(
				s,
				function(s_) {
					s_[grep(paste0(bra_, id, ket_), s_, perl = T)]
				},
				simplify = F
			)
			d[[sname]][g] <- gsub(paste0(bra_, id, ket_), "", p)
		}
	}
	d
}

tree <- function(column_subnames, value, node, i = 1) {

	l <- length(column_subnames)
	if(i < l) {
		node[[column_subnames[i]]] <- tree(column_subnames = column_subnames, value = value, node[[column_subnames[i]]], i + 1)
	}
	if(i == l) {
		node[[column_subnames[i]]] <- value
	}
	node
}

build_tree <- function(df, fun = n_av, verbose = 0) {
	df_names <- names(df)
	col_subnames <- strsplit(df_names, "\\.")
	names(col_subnames) <- df_names
	l <- list()
	for(n in df_names) {
		if(0 < verbose) cat(n, "\n")
		csn <- col_subnames[[n]]
		l <- tree(column_subnames = csn, value = fun(df[[n]]), node = l)
	}
	l
}

print_tree <- function(tre = tree_, tab = "") {
	for(n in names(tre)) {
		#n <- names(tre)[3]
		tr <- tre[[n]]
		cat(tab, n)
		if(!is.list(tr)) {
			cat(":", tr, "\n")
		} else {
			cat("\n")
			print_tree(tre = tr, paste0(tab, "  "))
		}
	}
}

build_xml <- function(tre, tab = "") {
	s <- ""
	for(n in names(tre)) {
		#n <- names(tre)[[1]]
		tr <- tre[[n]]
		n_ <- gsub("(.+)([0-9]+)$", "\\1", n)
		if(!is.list(tr)) {
			if(!is.na(tr)) {
				s <- paste0(s, tab, "<", n_, " value=\"", tr, "\"/>\n")
			}
		} else {
			s_ <- build_xml(tre = tr, tab = paste0(tab, "  "))
			if(s_ != "") {
				s <- paste0(s, tab, "<", n_, ">\n")
				s <- paste0(s, s_)
				s <- paste0(s, tab, "</", n_, ">\n")
			}
		}
	}
	s
}

build_bundle <- function(cast_table, resource_name, bundle_size = 50) {
	max_ <- nrow(cast_table)
	i <- 1
	cat(i - 1, "\n")
	bundles = list()
	while(i <= max_) {
		s <- "<Bundle>\n"
		end_ <- min(c(max_, i + bundle_size - 1))
		while(i <= end_) {
			tree_cast_table <- build_tree(cast_table[i,], fun = function(x)x)
			s <- paste0(s, "  <", resource_name, ">\n")
			s <- paste0(s, build_xml(tree_cast_table, tab = "    "))
			s <- paste0(s, "  </", res_, ">\n")
			i <- i + 1
		}
		s <- paste0(s, "</Bundle>\n")
		bundles <- c(bundles, s)
		cat(i - 1, "\n")
	}
	bundles
}

fun <- function(col) {min(col, na.rm = T)}
sel_row <- function(df) cs_casted[2]
n_av <- function(row) sum(!is.na(row))

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
		brackets = c("<(>", "<)>"),
		sep = "<|>"
	)
)

(df.patients <- fhir_crack(
	bundles = list(bundle),
	design = desc.patients,
	remove_empty_columns = FALSE))


(df.patients_cast <- fhir_cast(indexed_df = df.patients, sep = desc.patients@style@sep, brackets = desc.patients@style@brackets, verbose = 1))
(tree.patients_cast <- build_tree(df.patients_cast[1,], fun = function(x)x))
print_tree(tree.patients_cast)
cat(build_xml(tree.patients_cast))

s <- ""
for(i in seq_len(nrow(df.patients_cast))) {
	tree.patients_cast <- build_tree(df.patients_cast[i,], fun = function(x)x)
	s <- paste0(s, "<Patient>\n")
	s <- paste0(s, build_xml(tree.patients_cast,tab = "  "))
	s <- paste0(s, "</Patient>\n")
}
cat(s)


sep <- desc.patients@style@sep
brackets <- desc.patients@style@brackets

cs <- fhir_capability_statement("https://mii-agiop-3p.life.uni-leipzig.de/fhir", verbose = 2, sep = sep)
#cs <- fhir_capability_statement("https://hapi.fhir.org/baseR4", verbose = 2, sep = sep, brackets = brackets)

res <- cs$Resources

totals <- sapply(
	res$type,
	function(res_) {
		b <- fhir_search(paste0("https://mii-agiop-3p.life.uni-leipzig.de/fhir/", res_, "?_summary=count"), verbose = 1)
		d <- fhir_table_description(
			resource = "Bundle",
			cols = c("total" = "./total")
		)
		df <- fhir_crack(b, d, verbose = 0)
		as.numeric(df$total)
	}
)

totals <- totals[0 < totals]
(totals <- totals[order(totals)])

(res_ <- names(totals)[6])

bundles <- fhir_search(paste0("https://mii-agiop-3p.life.uni-leipzig.de/fhir/", res_, "?_count=500"), verbose = 2)
table_desc <- fhir_table_description(res_, style = fhir_style(sep = "<~>", brackets = c("<[", "]>")))
table_res <- fhir_crack(bundles, table_desc, verbose = 2)
table_cast <- fhir_cast(table_res, table_desc@style@sep, table_desc@style@brackets)
table_tree <- build_tree(table_cast[2,], function(i)i)
print_tree(table_tree)
cat(build_xml(tre = table_tree))

s <- build_bundle(cast_table = table_cast, resource_name = res_, bundle_size = 500)
cat(s[[1]])
xml2::read_xml(s[[1]])
