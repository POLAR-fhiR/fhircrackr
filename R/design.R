#rm(list = ls())

#Fix list by assigning proper names and defaults
fix <- function (list, names, defaults=NULL) {

	if (is.null(list)) {

		warning("Argument list is NULL")
		return(NULL)
	}
	else if (!is.list(list)) {

		warning(paste0("Argument list is ", typeof(list), " but must be list or NULL."))
		return(NULL)
	}

	if (length(list) < length(names)){

		list <- append(list, lapply(seq_len(length(names) - length(list)), function (x) NULL))
	}

	lnames <- names(list)

	if (is.null(lnames)) {

		names(list) <- names
	}
	else {

		wnames <- setdiff(setdiff(lnames, names), "")

		if (0 < length(wnames)) {

			warning(paste0("Unknown design component ", wnames,
						". Names of design components can only be resource, cols, style, sep, brackets and rm_empty_cols\n"))
			return(NULL)
		}

		lnames[lnames == ""] <- setdiff(names, lnames)[seq_len(sum(lnames == ""))]
		names(list) <- lnames
		list <- list[names]
	}

	if (! is.null(defaults)) {

		for (i in seq_along(list)) {

			if (is.null(list[[i]])) list[[i]] <- defaults[[i]]
		}
	}
	list
}


#fix df description
fix_df_desc <- function (df_desc) {

	df_desc <- fix(list = df_desc, names = c("resource", "cols", "style"))

	df_desc$style <- if (is.null(df_desc$style)){

		NULL
	}
	else {

		fix(df_desc$style, c("sep", "brackets", "rm_empty_cols"), defaults = list(" ", c("<", ">"), TRUE))
	}

	df_desc
}


#check df_description
is_valid_df_desc <- function (df_desc) {

	#dbg df_desc <- design[[1]]

	d <- fix_df_desc(df_desc = df_desc)
	testbundle <- xml2::read_xml("<Bundle>   </Bundle>")

	#check resource
	if (!is.character(d$resource)) {
		message <- paste0("resource component of data.frame description is ", typeof(d$resource), " but must be character.")
		return(data.frame(valid=FALSE, message))
	}

	if(length(d$resource) != 1) {
		message <- paste0("resource component of data.frame description has length ", length(d$resource), " but should be of length 1.")
		return(data.frame(valid=FALSE, message))

	}

	#check columns
	if (!is.null(d$cols) && !is.character(d$cols) && !is.list(d$cols)){
		message <- paste0("cols component of data.frame description is ", typeof(d$cols), " but must be character, list or NULL.")
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
		return(out)
	}

	#check style
	if (!is.null(d$style) && ! is.list(d$style)) {
		message <- paste0("style component of data.frame description is ", typeof(d$style), " but must be list or NULL.")
		return(data.frame(valid=FALSE, message))
	}

	if (is.list(d$style)) {

		d <- d$style

		if (!is.null(d$sep) && ! is.character(d$sep)) {

			message <- paste0("sep element of style component is ", typeof(d$resource), " but must be character or NULL.")
			return(data.frame(valid=FALSE, message))
		}

		if (!is.null(d$brackets) && ! is.character(d$brackets)) {

			message <- paste0("brackets element of style component is ", typeof(d$brackets), " but must be character.")
			return(data.frame(valid=FALSE, message))
		}

		if (!is.null(d$brackets) && length(brackets)!=2) {

			message <- paste0("brackets element of style component has length", length(d$brackets), " but must be of length 2.")
			return(data.frame(valid=FALSE, message))
		}

		if (!is.null(d$rm_empty_cols) && ! is.logical(d$rm_empty_cols)) {
			message <- paste0("rm_empty_cols element of style component is ", typeof(d$rm_empty_cols), " but must be logical or NULL.")
			return(data.frame(valid=FALSE, message))
		}
	}

	data.frame(valid=TRUE, message="fine")
}



#check design
is_valid_design <- function(design){

	#general checks
	if (is.null(design)) {
		warning("Argument design is NULL")
		return(FALSE)
	}

	if (!is.list(design)) {
		warning("Argument design has to be a list")
		return(FALSE)
	}

	if (is.null(names(design)) || any(names(design) == "")) {
		warning("Argument design should be a named list of data.frame descriptions, but at least one of the elements of design is unnamed.")
		return(FALSE)
	}

	if (length(design) < 1) {
		warning("Argument design has length 0")
		return(FALSE)
	}

	#checks of df_descriptions
	df_descr_results <- plyr::ldply(design, is_valid_df_desc)

	df_descr_results$number <- seq_len(nrow(df_descr_results))

	invalid <- df_descr_results[!df_descr_results$valid,]

	if (0 < nrow(invalid)){
		warning(
			"The following data.frame descriptions in your design seem to be invalid:\n",
			paste0("Data.frame description no.", invalid$number, " (", invalid$.id,")"," : ", invalid$message, "\n")
		)
		return(FALSE)
	}

	return(TRUE)
}



#
# ###Tests
#
# #richtig mit Namen
# design1 <- list(
# 	Pat = list(
# 		Resource = "//Patient",
# 		cols = list(
# 			ID = "id"
# 		),
# 		style= list(
# 			" ",
# 			rm_empty_cols = FALSE
# 		)
# 	)
# )
#
# is_valid_design(design = design1)
#
# #Falsche Namen: wirft momentan noch Fehler: In Warnung umwandeln und als invalid markieren?
# design2 <- list(
# 	Pat = list(
# 		resource = "//Patient",
# 		cols = list(
# 			ID = "id"
# 		),
# 		style = list(
# 			" ",
# 			rm_empty_cols = FALSE
# 		)
# 	)
# )
#
# is_valid_design(design2)
#
# #richtig ohne Namen
# design3 <- list(
# 	list(
# 		"//Patient",
# 		list(
# 			ID = "id"
# 		),
# 		list(
# 			" ",
# 			rm_empty_cols = FALSE
# 		)
# 	)
# )
#
# is_valid_design(design3)
#
#
# #fix df description with wrong order
# (f <- fix_df_desc(
# 	df_desc = list(
# 		style = list(
# 			rm_empty_cols = FALSE,
# 			brackets = c('[', ']'),
# 			" >> "
# 		),
# 		"//Patient",
# 		cols = list(
# 			ID = "id"
# 		)
# 	)
# ))
#
# #long design mit zwei Fehlern:
# design <- list(
# 	Pat = list(
# 		resource = NULL
# 	),
# 	Pat = list(
# 		resource = "//Patient",
# 		cols = ".//*"
# 	),
# 	Pat = list(
# 		resource = "//Patient",
# 		cols  = list(
# 			ID = "id"
# 		)
# 	),
# 	Pat = list(
# 		resource = 7,
# 		cols = list(
# 			ID = "id"
# 		),
# 		style = list(
# 			sep = " ",
# 			brackets = c("[", "]"),
# 			rm_empty_cols = TRUE
# 		)
# 	),
# 	Pat = list(
# 		resource = "//Patient",
# 		cols = ".//*",
# 		style = list(
# 			sep = " ",
# 			brackets = c("[", "]"),
# 			rm_empty_cols = TRUE
# 		)
# 	),
# 	Pat = list(
# 		resource = "//Patient",
# 		cols = NULL, # wuerde man nicht machen
# 		style = list(
# 			sep = " ",
# 			brackets = c("[", "]"),
# 			rm_empty_cols = TRUE
# 		)
# 	),
# 	Pat = list(
# 		resource = "//Patient",
# 		cols = NULL, # wuerde man nicht machen
# 		style = list(
# 			sep = " ",
# 			brackets = "#"
# 		)
# 	),
# 	Pat = list(
# 		resource = "//Patient",
# 		cols = NULL, # wuerde man nicht machen
# 		list(
# 			sep = " "
# 		)
# 	),
# 	Pat = list(
# 		"//Patient",
# 		NULL,
# 		list(
# 			NULL,
# 			"#"
# 		)
# 	),
# 	Pat = list(
# 		resource = "//Patient",
# 		style = list(
# 			rm_empty_cols = TRUE
# 		)
# 	),
# 	Pat = list(
# 		"//Patient"
# 	),
# 	Pat = list(
# 		"//Patient",
# 		".//*"
# 	),
# 	Pat = list(
# 		"//Patient",
# 		list(
# 			ID = "id"
# 		)
# 	),
# 	Pat = list(
# 		"//Patient",
# 		list(
# 			ID = "id"
# 		),
# 		list(
# 			" ",
# 			c("[", "]"),
# 			TRUE
# 		)
# 	),
# 	Pat = list(
# 		"//Patient",
# 		".//*",
# 		list(
# 			" ",
# 			c("[", "]"),
# 			TRUE
# 		)
# 	),
# 	Pat = list(
# 		"//Patient",
# 		NULL,
# 		list(
# 			" ",
# 			c("[", "]"),
# 			TRUE
# 		)
# 	),
# 	Pat = list(
# 		"//Patient",
# 		NULL,
# 		list(
# 			" ",
# 			"#"
# 		)
# 	),
# 	Pat = list(
# 		"//Patient",
# 		NULL,
# 		list(
# 			" "
# 		)
# 	),
# 	Pat = list(
# 		"//Patient",
# 		NULL,
# 		list(
#
# 		)
# 	),
# 	Pat = list(
# 		"//Patient",
# 		NULL,
# 		list(
# 			NULL,
# 			NULL,
# 			TRUE
# 		)
# 	)
# )
#
# is_valid_design(design)
#
#
#
# assign("last.warning", NULL, envir = baseenv())
# (designs_fixed <- lapply(designs, fix))
# warnings()
# (is_valid_Design <- all(sapply(designs, is_valid_design)))
# warnings()
#
