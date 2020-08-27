
####Fixing designs####

#Fix list by assigning proper names and defaults
fix <- function (list, names, defaults = NULL) {
	msg <- NULL

	if (is.null(list)) {

		return(list(value = NULL, msg = "is NULL."))

		} else if (!inherits(list, "list")) {

		return(list(value = NULL, msg = paste0("is a ", class(list), " but must be a list.")))

	}

	if(is.null(names(list)) || "" %in% names(list)) {
		msg <- ("There are unnamed elements in this data.frame description. \n Elements are assumed to be in the following order:  resource, cols, style (with elements sep, brackets, rm_empty_cols) \n")
	}

	#append missing elements with values NULL
	if (length(list) < length(names)) {
		list <- append(list, lapply(seq_len(length(names) - length(list)), function(x) {NULL}))
	}

	lnames <- names(list)

	if (is.null(lnames)) {#if no names in original list add names

		names(list) <- names

	}else{

		wnames <- setdiff(setdiff(lnames, names), "")

		if (0 < length(wnames)) { #if wrong names in original list

			return(value = NULL, msg = paste0("has unknown component ", wnames, ". Names of design components can only be resource, cols, style, sep, brackets and rm_empty_cols\n"))

		}

		#if missing names in original list
		lnames[lnames == ""] <-	setdiff(names, lnames)[seq_len(sum(lnames == ""))]
		names(list) <- lnames
		list <- list[names]#order correctly
	}

	#set defaults
	if (!is.null(defaults)) {
		for (i in seq_along(list)) {
			if (is.null(list[[i]]))
				list[[i]] <- defaults[[i]]
		}
	}

	return(list(value=list, msg = msg))
}


fix_brackets <- function(brackets){

	if (1 == length(brackets)) {c(brackets[1], brackets[1])} else {brackets}

}

#fix df description
fix_df_desc <- function (df_desc) {
	#dbg
	#df_desc <- design[[1]]
	msg <- NULL
	fix_res <- fix(list = df_desc, names = c("resource", "cols", "style"))

	if(is.null(fix_res$value)){

		fix_res$msg <- paste0("data.frame description ", fix_res$msg)

		return(fix_res)

	}else{

		df_desc <- fix_res$value
		msg <- fix_res$msg

	}

	if (is.null(df_desc$style)){

		df_desc$style <- list(
			sep = " ",
			brackets = NULL,
			rm_empty_cols = TRUE
		)

	}else{

		fix_res <- fix(df_desc$style,c("sep", "brackets", "rm_empty_cols"))

		if(is.null(fix_res$value)){

			fix_res$msg <- paste0("style ", fix_res$msg)

			return(fix_res)

		}else{

			df_desc$style <- fix_res$value
			if(is.null(msg)) {msg <- fix_res$msg}
		}
	}

	df_desc$style$brackets <- fix_brackets(df_desc$style$brackets)

	return(list(value = df_desc, msg=msg))
}


fix_design <- function(design) {

	fixed_design <-lapply(seq_along(design), function(i){

		fixed <- fix_df_desc(design[[i]])

		if(is.null(fixed$value)){
			#warning("Something is wrong with the data.frame description named", names(design)[i], ":\n", fixed$msg , "\n Returning NULL for this data.frame description. \n")
			return(NULL)
		}else{
			if(!is.null(fixed$msg)) {
				warning("\n For data.frame description ", names(design)[i], ": ", fixed$msg, "\n")
			}
			fixed$value
		}
	})

	names(fixed_design) <- names(design)
	return(fixed_design)
}

####validating designs####

#check df_description
is_valid_df_desc <- function (df_desc) {

	fixed_df_desc <- fix_df_desc(df_desc = df_desc)

	if(is.null(fixed_df_desc$value)) {return(data.frame(valid = FALSE, message = fixed_df_desc$msg))}

	d <- fixed_df_desc$value

	testbundle <- xml2::read_xml("<Bundle>   </Bundle>")

	#check resource
	if (!is.character(d$resource)) {
		message <-
			paste0(
				"resource component of data.frame description is a ",
				class(d$resource),
				" but must be character."
			)
		return(data.frame(valid = FALSE, message))
	}

	if(length(d$resource) != 1) {
		message <- paste0("resource component of data.frame description has length ", length(d$resource), " but should be of length 1.")
		return(data.frame(valid=FALSE, message))
	}

	out <- tryCatch(
		xml2::xml_find_all(testbundle, d$resource),
		warning = function(x) {
			if (grepl("Invalid expression", x))
				message <- 	paste("The string provided in the resource component of the data.frame description is not a valid XPath expression. Please revise this expression: ",
								  esc(d$resource))
			return(data.frame(valid=FALSE, message))
		}

	)

	if(!is.null(out$valid) && !out$valid) {return(out)}

	#check cols
	if (!is.null(d$cols) && !is.character(d$cols) && !inherits(d$cols, "list")){
		message <- paste0("cols component of data.frame description is a ", class(d$cols), " but must be character, list or NULL.")
		return(data.frame(valid=FALSE, message))
	}


	expressions <- unlist(df_desc$cols)

	for (i in seq_along(expressions)) {
		out <- tryCatch(
			xml2::xml_find_all(testbundle, expressions[[i]]),
			warning = function(x) {
					if (grepl("Invalid expression", x))
					message <- 	paste("One of the strings you have provided in the cols component is not a valid XPath expression. Please revise the following expression: ",
										esc(expressions[[i]]))
					return(data.frame(valid=FALSE, message))
			}
		)
		if(!is.null(out$valid) && !out$valid) {return(out)}
	}
	if (!is.null(d$cols) && !is.character(d$cols) && !inherits(d$cols, "list")) {
		message <-
			paste0(
				"cols component of data.frame description is a ",
				class(d$cols),
				" but must be character, list or NULL."
			)
		return(data.frame(valid = FALSE, message))
	}

	#check style
	if (!is.null(d$style) && (!inherits(d$style, "list"))) {
		message <-
			paste0(
				"style component of data.frame description is ",
				class(d$style),
				" but must be list or NULL."
			)
		return(data.frame(valid = FALSE, message))
	}
	if (inherits(d$style, "list")) {
		s <- d$style
		if (!is.null(s$sep) && !is.character(s$sep)) {
			message <-
				paste0(
					"sep element of style component is ",
					class(s$sep),
					" but must be character or NULL."
				)
			return(data.frame(valid = FALSE, message))
		}
		if (!is.null(s$brackets) && !is.character(s$brackets)) {
			message <-
				paste0(
					"brackets element of style component is a ",
					class(s$brackets),
					" but must be character or list."
				)
			return(data.frame(valid = FALSE, message))
		}

		if (!is.null(d$brackets) && length(d$brackets)!=2) {

			message <- paste0("brackets element of style component has length", length(d$brackets), " but must be of length 2.")
			return(data.frame(valid=FALSE, message))
		}

		if (!is.null(d$rm_empty_cols) && ! is.logical(d$rm_empty_cols)) {
			message <- paste0("rm_empty_cols element of style component is a ", class(d$rm_empty_cols), " but must be logical or NULL.")
			return(data.frame(valid=FALSE, message))
		}
	}

	data.frame(valid = TRUE, message = "fine")
}



#check design
is_valid_design <- function(design) {

	#general checks
	if (is.null(design)) {
		warning("Argument design is NULL, returning NULL. \n")
		return(list(FALSE, NULL))
	}

	if (!inherits(design, "list")) {
		warning("Argument design has to be a list, returning NULL. \n")
		return(list(FALSE, NULL))
	}

	if (is.null(names(design)) || any(names(design) == "")) {
		warning("Argument design should be a named list of data.frame descriptions, but at least one of the elements of design is unnamed. Returning NULL. \n")
		return(list(FALSE, NULL))
	}

	if (length(design) < 1) {
		warning("Argument design has length 0, returning NULL. \n")
		return(list(FALSE, NULL))
	}

	#checks of df_descriptions
	df_descr_results <- plyr::ldply(design, is_valid_df_desc)

	df_descr_results$number <- seq_len(nrow(df_descr_results))

	invalid <- df_descr_results[!df_descr_results$valid, ]

	if (0 < nrow(invalid)) {
		warning(
			"The following data.frame descriptions in your design seem to be invalid:\n\n ",
			paste0("Data.frame description no.", invalid$number, " (", invalid$.id,")"," : ", invalid$message, "\n"),
			"Returning NULL for all invalid data.frame descriptions. \n"
		)
		return(list(FALSE,invalid$number))
	}

	return(list(TRUE, NULL))
}

####save designs####

design2xml <- function (design) {

	design <- fix_design(design)

	#dbg
	#design <- designs
	xml  <- xml2::xml_new_document()
	root <- xml2::xml_add_child(xml, "Design")

	for (nms in names(design)) {
		#dbg
		#nms <- names(design[1])
		df_desc <- design[[nms]]

		if (is_valid_df_desc(df_desc = df_desc)[1, 1]) {
			child <- xml2::xml_add_child(root, nms)

			res <- xml2::xml_add_child(child, "resource")
			xml2::xml_set_attr(res, "value", df_desc$resource)

			cols <- xml2::xml_add_child(child, "cols")

			if (0 < length(df_desc$cols)) {
				if (is.character(df_desc$cols)) {
					xml2::xml_set_attr(cols, "value", df_desc$cols)
				}
				else {
					for (nms_col in names(df_desc$cols)) {
						col <- xml2::xml_add_child(cols, nms_col)
						xml2::xml_set_attr(col, "value", df_desc$cols[[nms_col]])
					}
				}
			}

			stl <- xml2::xml_add_child(child, "style")
			sep <- xml2::xml_add_child(stl, "sep")
			bra <- xml2::xml_add_child(stl, "brackets")
			opn <- xml2::xml_add_child(bra, "open")
			cls <- xml2::xml_add_child(bra, "close")
			rme <- xml2::xml_add_child(stl, "rm_empty_cols")

			xml2::xml_set_attr(sep, "value", df_desc$style$sep)
			xml2::xml_set_attr(opn, "value", df_desc$style$brackets[1])
			xml2::xml_set_attr(cls, "value", df_desc$style$brackets[2])
			xml2::xml_set_attr(rme, "value", df_desc$style$rm_empty_cols)
		}
	}
	xml2::xml_ns_strip(xml2::xml_root(xml))
	xml2::xml_root(xml)
}

save_design <- function (design, file = "design.xml") {

	xml <- design2xml(design = design)

	xml2::write_xml(xml, file)
}


####read designs####
xml2design <- function(xml) {

	xml_design <- xml2::xml_find_all(xml, "//Design")

	if (length(xml_design) < 1) {
		warning("The Argument xml does not contain a Design. \n")
		return(NULL)
	}
	if (1 < length(xml_design)) {
		warning("xml2design() does currently not suppurt more than 1 Design per xml. Returning NULL. \n")
		return(NULL)
	}

	xml_design <- xml_design[[1]]
	xml_df_descriptions <- xml2::xml_find_all(xml_design, "*")

	if (length(xml_df_descriptions) < 1) {
		warning("Design does not contain any entries like resource, cols and style. Returning NULL. \n")
		return(NULL)
	}

	resources_names <- sapply(xml_df_descriptions, xml2::xml_name)

	if (length(unique(resources_names)) < length(resources_names)) {

		warning(paste0("Names of data.frame descriptions have to be unique. ", resources_names[duplicated(resources_names)], " are duplicates. Returning NULL. \n"))
		return(NULL)
	}

	l <- lapply(seq_along(xml_df_descriptions), function (i) {

		#dbg
		#i <- 1
		xml_df_desc <- xml_df_descriptions[[i]]

		resource_xpath <- xml2::xml_attr(xml2::xml_find_all(xml_df_desc, "resource"), "value")

		if (length(resource_xpath) < 1) {
			warning(paste0("data.frame description of resource ", resources_names[i], " needs at least a <Resource value=XPath_To_Resource> entry. Returning empty data.frame description. \n"))
			resource_xpath <- NULL
		} else {

			columns <- xml2::xml_find_all(xml_df_desc, "cols")
			if (length(columns) < 1) {
				columns_list <- NULL
			} else {
				columns_list <- xml2::xml_find_all(columns, "*")
				if (length(columns_list) < 1) columns_list <- NULL
			}

			if (0 < length(columns_list)) {
				col_names <- xml2::xml_name(columns_list)
				col_values <- xml2::xml_attr(columns_list, "value")
				col_list <- as.list(col_values)
				names(col_list) <- col_names
			} else if (1 == length(columns)) {
				col_list <- xml2::xml_attr(columns, "value")
				if (all(is.na(col_list))) {
					warning(paste0("cols entry missing in data frame description for resource ", resources_names[i], ". Returning NULL for cols. \n"))
					col_list <- NULL
				}
			} else {
				col_list <- NULL
			}

			style <- xml2::xml_find_all(xml_df_desc, "style")
			if (length(style) < 1) {
				#warning(paste0("No Style Information in ", resources_names[i], "."))
				sep <- NULL
				bra_open <- NULL
				bra_close <- NULL
				rm_empty_cols <- NULL
			} else {
				sep <- xml2::xml_attr(xml2::xml_find_all(style, "sep"), "value")
				if (length(sep) < 1 || all(is.na(sep))) sep <- NULL
				bra_open <- xml2::xml_attr(xml2::xml_find_all(style, "brackets/open"), "value")
				if (length(bra_open) < 1 || all(is.na(bra_open))) bra_open <- NULL
				bra_close <- xml2::xml_attr(xml2::xml_find_all(style, "brackets/close"), "value")
				if (length(bra_close) < 1 || all(is.na(bra_close))) bra_close <- NULL
				rm_empty_cols <- as.logical(xml2::xml_attr(xml2::xml_find_all(style, "rm_empty_cols"), "value"))
				if (length(rm_empty_cols) < 1 || all(is.na(rm_empty_cols))) rm_empty_cols <- NULL
			}
			fix_df_desc(list(
				resource = resource_xpath,
				cols     = col_list,
				style    = list(
					sep = sep,
					brackets = c(bra_open, bra_close),
					rm_empty_cols = rm_empty_cols
				)
			))$value
		}
	})
	names(l) <- resources_names
	l
}


load_design <- function (file) {
	xml <- xml2::read_xml(file)
	xml2design(xml)
}

#
# (xml <- xml2::read_xml(paste0(
# 	"<Design>",
# 		"<Patient1>",
# 		"</Patient1>",
# 		"<Patient2>",
# 			"<Resource value='//Patient'/>",
# 			"<Columns value='.//*'/>",
# 		"</Patient2>",
# 		"<Patient13>",
# 			"<Resource value='//Patient'/>",
# 			"<Columns>",
# 				"<ID value='id'/>",
# 				"<VNAME value='name/given'/>",
# 				"<NNAME value='name/family'/>",
# 			"</Columns>",
# 		"</Patient13>",
# 		"<Patient3>",
# 			"<Resource value='//Patient'/>",
# 			"<Columns>",
# 			"</Columns>",
# 		"</Patient3>",
# 		"<Patient4>",
# 			"<Resource value='//Patient'/>",
# 			"<Columns>",
# 				"<ID value='id'/>",
# 				"<VNAME value='name/given'/>",
# 				"<NNAME value='name/family'/>",
# 			"</Columns>",
# 			"<Style>",
# 				"<Separator value=' -+- '/>",
# 				"<Brackets>",
# #					"<Open value='OPEN'/>",
# 					"<Close value='CLOSE'/>",
# 				"</Brackets>",
# 			"</Style>",
# 		"</Patient4>",
# 	"</Design>"
# )))
#
# assign("last.warning", NULL, envir = baseenv())
# save_design(design = xml2design(xml = xml), "design.xml")
# warnings()
# assign("last.warning", NULL, envir = baseenv())
# design2 <- load_design("design.xml")
# warnings()
#
# #long design mit zwei Fehlern:
# design <- list(
# 	Pat1 = list(resource = NULL),
# 	Pat2 = list(resource = "//Patient",
# 				cols = ".//*"),
# 	Pat3 = list(
# 		resource = "//Patient",
# 		cols  = list(ID = "id",
# 					 NAME = "name/family",
# 					 VNAME = "name/given")
# 	),
# 	Pat4 = list(
# 		resource = 7,
# 		cols = list(ID = "id"),
# 		style = list(
# 			sep = " ",
# 			brackets = c("[", "]"),
# 			rm_empty_cols = TRUE
# 		)
# 	),
# 	Pat5 = list(
# 		resource = "//Patient",
# 		cols = ".//*",
# 		style = list(
# 			sep = " ",
# 			brackets = c("[", "]"),
# 			rm_empty_cols = TRUE
# 		)
# 	),
# 	Pat6 = list(
# 		resource = "//Patient",
# 		cols = NULL,
# 		# wuerde man nicht machen
# 		style = list(
# 			sep = " ",
# 			brackets = c("[", "]"),
# 			rm_empty_cols = TRUE
# 		)
# 	),
# 	Pat7 = list(
# 		resource = "//Patient",
# 		cols = NULL,
# 		# wuerde man nicht machen
# 		style = list(sep = " ",
# 					 brackets = "#")
# 	),
# 	Pat8 = list(resource = "//Patient",
# 				cols = NULL, # wuerde man nicht machen
# 				list(sep = " ")),
# 	Pat9 = list("//Patient",
# 				NULL,
# 				list(NULL,
# 					 "#")),
# 	Pat10 = list(resource = "//Patient",
# 				 style = list(rm_empty_cols = TRUE)),
# 	Pat11 = list("//Patient"),
# 	Pat12 = list("//Patient",
# 				 ".//*"),
# 	Pat13 = list("//Patient",
# 				 list(ID = "id", NAME = "name/family")),
# 	Pat14 = list("//Patient",
# 				 list(ID = "id"),
# 				 list(" ",
# 				 	 c("[", "]"),
# 				 	 TRUE)),
# 	Pat15 = list("//Patient",
# 				 ".//*",
# 				 list(" ",
# 				 	 c("[", "]"),
# 				 	 TRUE)),
# 	Pat16 = list("//Patient",
# 				 NULL,
# 				 list(" ",
# 				 	 c("[", "]"),
# 				 	 TRUE)),
# 	Pat17 = list("//Patient",
# 				 NULL,
# 				 list(" ",
# 				 	 "#")),
# 	Pat18 = list("//Patient",
# 				 NULL,
# 				 list(" ")),
# 	Pat19 = list("//Patient",
# 				 NULL,
# 				 list()),
# 	Pat20 = list("//Patient",
# 				 NULL,
# 				 list(NULL,
# 				 	 NULL,
# 				 	 TRUE))
# )
#
# #design <- design[6:7]
#
# assign("last.warning", NULL, envir = baseenv())
# save_design(design = design, "DesignOriginal.xml")
# warnings()
# assign("last.warning", NULL, envir = baseenv())
# design2 <- load_design(file = "DesignOriginal.xml")
# warnings()
# assign("last.warning", NULL, envir = baseenv())
# save_design(design = design2, "DesignResult.xml")
# warnings()
# assign("last.warning", NULL, envir = baseenv())
#
#
# design <- list(
# 	Observations = list(
# 		"//Observation",
# 		list(
# 			O.OID = "id",
# 			O.PID = "subject/reference",
# 			O.EID = "encounter/reference",
# 			DIA   = "component[code/coding/code/@value='8462-4']/valueQuantity/value",
# 			SYS   = "component[code/coding/code/@value='8480-6']/valueQuantity/value",
# 			DATE  = "effectiveDateTime"
# 		),
# 		list(" -+- ", '#BRACKET#', T)
# 	),
# 	Encounters = list(
# 		"//Encounter",
# 		".//*",
# 		list("- + -", c('[', ']'), F)
# 	),
# 	Patients = list(
# 		"//Patient"
# 	)
# )
#
#
# assign("last.warning", NULL, envir = baseenv())
# save_design(design = design, "DesignOriginal2.xml")
# warnings()
# assign("last.warning", NULL, envir = baseenv())
# design2 <- load_design("DesignOriginal2.xml")
# warnings()
# assign("last.warning", NULL, envir = baseenv())
# save_design(design = design2, "DesignResult2.xml")
# warnings()
# assign("last.warning", NULL, envir = baseenv())
#
#