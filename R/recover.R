#
# bun <- fhir_search("https://mii-agiop-3p.life.uni-leipzig.de/fhir/Patient",
# 					  max_bundles = 1)
#
# b <- bun[[1]]
#
# design <- list(
# 	Pat = list(
# 		resource = "//Patient"
# 	)
# )
#
# dfs <- fhir_crack(bun, design)
#
# d <- dfs[[1]]
#
#
# library(xml2)

#TODO: remove div elements, deal with multiple entries

#create empty resource
create_resource <- function(resourceType, colnames){

	myres <- read_xml("<resource> </resource>")
	xml_add_child(myres, xml_new_root(resourceType))


	nodes <- strsplit(colnames, esc("."))
	depth <- sapply(nodes, length)
	max_depth <- max(sapply(nodes, length))


	current <- sapply(nodes, function(x){x[1]})
	parent <- rep(resourceType, length(current))

	for(j in 1: max_depth){

		parent_red <- parent[!is.na(current)]
		current_red <- current[!is.na(current)]
		parent_red <- parent_red[!duplicated(current_red)]
		current_red <- current_red[!duplicated(current_red)]


		for (i in 1:length(current_red)){
			parent_node <- xml_child(myres, parent_red[i])
			xml_add_child(parent_node, xml_new_root(current_red[i]))
		}

		parent <- paste(parent,current, sep="/")
		current <- sapply(nodes, function(x){x[j+1]})

	}

	myres

}

#fill
fill_resource <- function(resource, row){

	resourceType <- xml_name(xml_child(resource, 1))

	paths <- gsub(esc("."), esc("/"),names(row))
	paths <- paste(resourceType, paths, sep="/")

	for(i in 1:ncol(row)){
		child <- xml_child(resource, paths[i])
		xml_attr(child, "value")  <- row[1,i]
	}
}

res <- create_resource("Patient", names(d))
fill_resource(res, d[2,])

#
create_bundle <- function(resourceType, df){
	#l <- vector("list", nrow(df))

	bundle <- xml_new_root("Bundle")
	xml_add_child(bundle, xml_new_root("type", value= "transaction"))

	for(i in 1:nrow(df)){
		xml_add_child(bundle, create_resource(resourceType = resourceType, colnames = names(df)))
		fill_resource(xml_child(bundle, i+1), row = df[i,])
		xml_add_parent(xml_child(bundle, i+1), xml_new_root("entry"))
		xml_add_child(xml_child(bundle, i+1), xml_new_root("request"))
		xml_add_child(xml_find_all(bundle, "entry/request")[[i]], xml_new_root("method", value="PUT"))
		xml_find_all(bundle, "entry")

	}

	bundle
}

#erg <- create_bundle("Patient", d)
