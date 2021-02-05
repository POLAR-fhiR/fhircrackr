


#TODO: remove div elements, deal with multiple entries

#create empty resource
create_resource <- function(resourceType, colnames){

	myres <- xml2::read_xml("<resource> </resource>")
	xml2::xml_add_child(myres, xml2::xml_new_root(resourceType))


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
			parent_node <- xml2::xml_child(myres, parent_red[i])
			xml2::xml_add_child(parent_node, xml2::xml_new_root(current_red[i]))
		}

		parent <- paste(parent,current, sep="/")
		current <- sapply(nodes, function(x){x[j+1]})

	}

	myres

}

#fill
fill_resource <- function(resource, row){

	resourceType <- xml2::xml_name(xml2::xml_child(resource, 1))

	paths <- gsub(esc("."), esc("/"),names(row))
	paths <- paste(resourceType, paths, sep="/")

	for(i in 1:ncol(row)){
		child <- xml2::xml_child(resource, paths[i])
		xml2::xml_attr(child, "value")  <- row[1,i]
	}
}


#
create_bundle <- function(resourceType, df){

	#remove any narrative elements https://www.hl7.org/fhir/narrative.html
	if(any(grepl("^text", names(df)))){
		d <- df[,!names(df) %in% fhir_common_columns(df, "text")]
	}else{
		d <- df
	}


	#create transaction bundle
	bundle <- xml2::xml_new_root("Bundle")
	xml2::xml_add_child(bundle, xml2::xml_new_root("type", value= "transaction"))

	for(i in 1:nrow(d)){
		#create and fill resources
		xml2::xml_add_child(bundle, create_resource(resourceType = resourceType, colnames = names(d)))
		fill_resource(xml2::xml_child(bundle, i+1), row = d[i,])
		#create entry and request nodes
		xml2::xml_add_parent(xml2::xml_child(bundle, i+1), xml2::xml_new_root("entry"))
		xml2::xml_add_child(xml2::xml_child(bundle, i+1), xml2::xml_new_root("request"))
		xml2::xml_add_child(xml2::xml_find_all(bundle, "entry/request")[[i]], xml2::xml_new_root("method", value="PUT"))
		xml2::xml_add_child(xml2::xml_find_all(bundle, "entry/request")[[i]],
							xml2::xml_new_root("url",
											   value=paste(resourceType,
											   			 xml2::xml_attr(xml2::xml_child(bundle, "entry/resource/*/id"), "value"),
											   			sep="/")))
	}

	bundle
}


