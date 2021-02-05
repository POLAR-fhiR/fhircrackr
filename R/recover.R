#
# bundle <- fhir_search("https://mii-agiop-3p.life.uni-leipzig.de/fhir/Patient",
# 					  max_bundles = 1)
#
# b <- bundle[[1]]
#
# design <- list(
# 	Pat = list(
# 		resource = "//Patient"
# 	)
# )
#
# dfs <- fhir_crack(bundle, design)
#
# d <- dfs[[1]]
#
#
# library(xml2)
#
# #create empty resource
#
# create_resource <- function(resourceType, colnames){
#
# 	myres <- read_xml("<resource> </resource>")
# 	xml_add_child(myres, xml_new_root(resourceType))
#
#
# 	nodes <- strsplit(colnames, esc("."))
# 	depth <- sapply(nodes, length)
# 	max_depth <- max(sapply(nodes, length))
#
#
# 	current <- sapply(nodes, function(x){x[1]})
# 	parent <- rep(resourceType, length(current))
#
# 	for(j in 1: max_depth){
#
# 		parent_red <- parent[!is.na(current)]
# 		current_red <- current[!is.na(current)]
# 		parent_red <- parent_red[!duplicated(current_red)]
# 		current_red <- current_red[!duplicated(current_red)]
#
#
# 		for (i in 1:length(current_red)){
# 			parent_node <- xml_child(myres, parent_red[i])
# 			xml_add_child(parent_node, xml_new_root(current_red[i]))
# 		}
#
# 		parent <- paste(parent,current, sep="/")
# 		current <- sapply(nodes, function(x){x[j+1]})
#
#
# 	}
#
# }
#
#
#
# #fill
# #fill_resource <- function(resourceType, )
# paths <- gsub(esc("."), esc("/"),names(d))
# paths <- paste("Patient", paths, sep="/")
#
# for(i in 1:ncol(d)){
# 	child <- xml_child(myres, paths[i])
# 	xml_attr(child, "value")  <- d[1,i]
# }


