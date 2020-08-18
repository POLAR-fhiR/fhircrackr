rm(list = ls())

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

	#TODO: Hier noch andere Checks aus is_invalid_design aus helpers.R ergänzen (zB xml-Ausdrücke checken)

	d <- fix_df_desc(df_desc = df_desc)

	if (!is.character(d$resource)) {

		message <- paste0("resource component of data.frame description is ", typeof(d$resource), " but must be character.")
		return(data.frame(valid=FALSE, message))
	}

	if (!is.null(d$cols) && !is.character(d$cols) && !is.list(d$cols)){
		message <- paste0("cols component of data.frame description  is ", typeof(d$cols), " but must be character, list or NULL.")
		return(data.frame(valid=FALSE, message))
	}

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

			message <- paste0("brackets element of style component is ", typeof(d$brackets), " but must be character or list.")
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

	#TODO: Hier noch andere Checks aus is_invalid_design aus helpers.R ergänzen

	#general checks
	if (is.null(design)) {
		warning("Argument design is NULL")
		return(FALSE)
	}

	if (!is.list(design)) {
		warning("Argument design has to be a list")
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
			"The following data.frame descriptions in you design seem to be invalid:\n",
			paste0("Data.frame description no.", invalid$number, " (", invalid$.id,")"," : ", invalid$message, "\n")
		)
		return(FALSE)
	}

	return(TRUE)
}




###Tests

#richtig mit Namen
design1 <- list(
	Pat = list(
		Resource = "//Patient",
		cols = list(
			ID = "id"
		),
		style= list(
			" ",
			rm_empty_cols = FALSE
		)
	)
)

is_valid_design(design = design1)

#Falsche Namen: wirft momentan noch Fehler: In Warnung umwandeln und als invalid markieren?
design2 <- list(
	Patient = list(
		resource = "//Patient",
		cols = list(
			ID = "id",
			VName = "name/given",
			NName = "name/family"
		),
		style = list(
			" ",
			rm_empty_cols = FALSE
		)
	)
)

is_valid_design(design2)

#richtig ohne Namen
design3 <- list(
	list(
		"//Patient",
		list(
			ID = "id"
		),
		list(
			" ",
			rm_empty_cols = FALSE
		)
	)
)

is_valid_design(design3)


#fix df description with wrong order
(f <- fix_df_desc(
	df_desc = list(
		style = list(
			rm_empty_cols = FALSE,
			brackets = c('[', ']'),
			" >> "
		),
		"//Patient",
		cols = list(
			ID = "id"
		)
	)
))

#long design mit zwei Fehlern:
designs <- list(
	Pat1 = list(
		resource = NULL
	),
	Pat2 = list(
		resource = "//Patient",
		cols = ".//*"
	),
	Pat3 = list(
		resource = "//Patient",
		cols  = list(
			ID = "id"
		)
	),
	Pat4 = list(
		resource = 7,
		cols = list(
			ID = "id"
		),
		style = list(
			sep = " ",
			brackets = c("[", "]"),
			rm_empty_cols = TRUE
		)
	),
	Pat5 = list(
		resource = "//Patient",
		cols = ".//*",
		style = list(
			sep = " ",
			brackets = c("[", "]"),
			rm_empty_cols = TRUE
		)
	),
	Pat6 = list(
		resource = "//Patient",
		cols = NULL, # wuerde man nicht machen
		style = list(
			sep = " ",
			brackets = c("[", "]"),
			rm_empty_cols = TRUE
		)
	),
	Pat7 = list(
		resource = "//Patient",
		cols = NULL, # wuerde man nicht machen
		style = list(
			sep = " ",
			brackets = "#"
		)
	),
	Pat8 = list(
		resource = "//Patient",
		cols = NULL, # wuerde man nicht machen
		list(
			sep = " "
		)
	),
	Pat9 = list(
		"//Patient",
		NULL,
		list(
			NULL,
			"#"
		)
	),
	Pat10 = list(
		resource = "//Patient",
		style = list(
			rm_empty_cols = TRUE
		)
	),
	Pat11 = list(
		"//Patient"
	),
	Pat12 = list(
		"//Patient",
		".//*"
	),
	Pat13 = list(
		"//Patient",
		list(
			ID = "id"
		)
	),
	Pat14 = list(
		"//Patient",
		list(
			ID = "id"
		),
		list(
			" ",
			c("[", "]"),
			TRUE
		)
	),
	Pat15 = list(
		"//Patient",
		".//*",
		list(
			" ",
			c("[", "]"),
			TRUE
		)
	),
	Pat16 = list(
		"//Patient",
		NULL,
		list(
			" ",
			c("[", "]"),
			TRUE
		)
	),
	Pat17 = list(
		"//Patient",
		NULL,
		list(
			" ",
			"#"
		)
	),
	Pat18 = list(
		"//Patient",
		NULL,
		list(
			" "
		)
	),
	Pat19 = list(
		"//Patient",
		NULL,
		list(

		)
	),
	Pat20 = list(
		"//Patient",
		NULL,
		list(
			NULL,
			NULL,
			TRUE
		)
	)
)

is_valid_design(designs)



assign("last.warning", NULL, envir = baseenv())
(designs_fixed <- lapply(designs, fix))
warnings()
(is_valid_Design <- all(sapply(design, is_valid_design)))
warnings()


design2xml <- function (design) {

	#dbg
	#design <- designs
	xml  <- xml2::xml_new_document()
	root <- xml2::xml_add_child(xml, "Design")

	for (nms in names(design)) {

		#dbg
		#nms <- names(design[2])
		df_desc <- fix_df_desc(df_desc = design[[nms]])

		if (is_valid_df_desc(df_desc = df_desc)[1,1]) {

			child <- xml2::xml_add_child(root, nms)

			res <- xml2::xml_add_child(child, "Resource")
			xml2::xml_set_attr(res, "value", df_desc$resource)

			cols <- xml2::xml_add_child(child, "Columns")

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

			stl <- xml2::xml_add_child(child, "Style")
			sep <- xml2::xml_add_child(stl, "Sep")
			bra <- xml2::xml_add_child(stl, "Brackets")
			opn <- xml2::xml_add_child(bra, "Open")
			cls <- xml2::xml_add_child(bra, "Close")
			rme <- xml2::xml_add_child(stl, "Remove_Empty_Columns")

			xml2::xml_set_attr(sep, "value", df_desc$style$sep)
			xml2::xml_set_attr(opn, "value", df_desc$style$brackets[1])
			xml2::xml_set_attr(cls, "value", df_desc$style$brackets[2])
			xml2::xml_set_attr(rme, "value", df_desc$style$rm_empty_cols)
		}
	}
	xml
}

save_design <- function (design, file = "design.xml") {

	xml <- design2xml(design = design)

	xml2::write_xml(xml, file)
}

save_design(design = designs, "Design.xml")
