
#Fix list by assigning proper names and defaults
fix <- function (list, names, defaults=NULL) {

	if (is.null(list)) {

			warning("Argument list is NULL")

		}else if (!is.list(list)) {

			warning(paste0("Argument list is ", typeof(list), " but must be list or NULL."))

		}

	if (length(list) < length(names)){

		list <- append(list, lapply(seq_len(length(names) - length(list)), function (x) NULL))
	}

	lnames <- names(list)

	if (is.null(lnames)) {

		names(list) <- names

	}else{

		wnames <- setdiff(setdiff(lnames, names), "")

		if (0 < length(wnames)) {

			stop(paste0("Unknown design component ", wnames,
						". Names of design components can only be resource, cols, style, sep, brackets and rm_empty_cols\n"))

		}

		lnames[lnames == ""] <- setdiff(names, lnames)[seq_len(sum(lnames == ""))]
		names(list) <- lnames
		list <- list[names]
	}

	if (! is.null(defaults)){

		for (i in seq_along(list)){

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

	}else{

		fix(df_desc$style, c("sep", "brackets", "rm_empty_cols"), defaults = list(" ", c("<", ">"), TRUE))

	}

	df_desc
}


#check df_description
is_valid_df_desc <- function (df_desc) {

	#TODO: Hier noch andere Checks aus is_invalid_design aus helpers.R ergänzen (zB xml-Ausdrücke checken)

	d <- fix_df_desc(df_desc)

	if (!is.character(d$resource)) {
		message <- paste0("resource component of data.frame description is ", typeof(d$Resource), " but must be character.")
		return(data.frame(valid=FALSE, message))
	}

	if (!is.null(d$cols) && !is.character(d$cols) && !is.list(d$cols)){
		message <- paste0("cols component of data.frame description  is ", typeof(d$Columns), " but must be character, list or NULL.")
		return(data.frame(valid=FALSE, message))
	}

	if (!is.null(d$style) && ! is.list(d$style)){
		message <-paste0("style component of data.frame description is ", typeof(d$Decoration), " but must be list or NULL.")
		return(data.frame(valid=FALSE, message))
	}

	if (is.list(d$style)) {
		d <- d$style
		if (!is.null(d$sep) && ! is.character(d$sep)) {
			message <-paste0("sep element of style component is ", typeof(d$Resource), " but must be character or NULL.")
			return(data.frame(valid=FALSE, message))
		}

		if (!is.null(d$brackets) && ! is.character(d$brackets)){
			message <-paste0("brackets element of style component is ", typeof(d$Brackets), " but must be character or list.")
			return(data.frame(valid=FALSE, message))
		}
		if (!is.null(d$rm_empty_cols) && ! is.logical(d$rm_empty_cols)){
			message <-paste0("rm_empty_cols element of style component is ", typeof(d$Remove_Empty_Columns), " but must be logical or NULL.")
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
	df_descr_results <- ?plyr::ldply(design, is_valid_df_desc)

	df_descr_results$number <- 1:nrow(df_descr_results)

	invalid <- df_descr_results[!df_descr_results$valid,]

	if(nrow(invalid)>0){
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
	list(
		resource = "//Patient",
		cols = list(
			ID = "id"
		),
		style= list(
			" ",
			rm_empty_cols = FALSE
		)
	)
)

is_valid_design(design1)

#Falsche Namen: wirft momentan noch Fehler: In Warnung umwandeln und als invalid markieren?
design2 <- list(
	list(
		Resource = "//Patient",
		cols = list(
			ID = "id"
		),
		stylE= list(
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
design <- list(
	Pat = list(
		resource = NULL
	),
	Pat = list(
		resource = "//Patient",
		cols = ".//*"
	),
	Pat = list(
		resource = "//Patient",
		cols  = list(
			ID = "id"
		)
	),
	Pat = list(
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
	Pat = list(
		resource = "//Patient",
		cols = ".//*",
		style = list(
			sep = " ",
			brackets = c("[", "]"),
			rm_empty_cols = TRUE
		)
	),
	Pat = list(
		resource = "//Patient",
		cols = NULL, # wuerde man nicht machen
		style = list(
			sep = " ",
			brackets = c("[", "]"),
			rm_empty_cols = TRUE
		)
	),
	Pat = list(
		resource = "//Patient",
		cols = NULL, # wuerde man nicht machen
		style = list(
			sep = " ",
			brackets = "#"
		)
	),
	Pat = list(
		resource = "//Patient",
		cols = NULL, # wuerde man nicht machen
		list(
			sep = " "
		)
	),
	Pat = list(
		"//Patient",
		NULL,
		list(
			NULL,
			"#"
		)
	),
	Pat = list(
		resource = "//Patient",
		style = list(
			rm_empty_cols = TRUE
		)
	),
	Pat = list(
		"//Patient"
	),
	Pat = list(
		"//Patient",
		".//*"
	),
	Pat = list(
		"//Patient",
		list(
			ID = "id"
		)
	),
	Pat = list(
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
	Pat = list(
		"//Patient",
		".//*",
		list(
			" ",
			c("[", "]"),
			TRUE
		)
	),
	Pat = list(
		"//Patient",
		NULL,
		list(
			" ",
			c("[", "]"),
			TRUE
		)
	),
	Pat = list(
		"//Patient",
		NULL,
		list(
			" ",
			"#"
		)
	),
	Pat = list(
		"//Patient",
		NULL,
		list(
			" "
		)
	),
	Pat = list(
		"//Patient",
		NULL,
		list(

		)
	),
	Pat = list(
		"//Patient",
		NULL,
		list(
			NULL,
			NULL,
			TRUE
		)
	)
)

is_valid_design(design)



assign("last.warning", NULL, envir = baseenv())
(designs_fixed <- lapply(designs, fix))
warnings()
(is_valid_Design <- all(sapply(designs, is_valid_design)))
warnings()

