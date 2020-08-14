fixx <- function (list, names, defaults=NULL) {
	if (is.null(list)) warning("Argument list is NULL") else if (! is.list(list)) warning(paste0("Argument list is ", typeof(list), " but must be list or NULL."))
	if (length(list) < length(names)) list <- append(list, lapply(seq_len(length(names) - length(list)), function (x) NULL))
	lnames <- names(list)
	if (is.null(lnames)) names(list) <- names
	else {
		wnames <- setdiff(setdiff(lnames, names), "")
		if (0 < length(wnames)) {
			stop(paste0("Unknown Argument(s) ", wnames, "."))
		}
		lnames[lnames == ""] <- setdiff(names, lnames)[seq_len(sum(lnames == ""))]
		names(list) <- lnames
		list <- list[names]
	}
	if (! is.null(defaults)) for (i in seq_along(list)) if (is.null(list[[i]])) list[[i]] <- defaults[[i]]
	list
}

fix <- function (design) {
	design <- fixx(list = design, names = c("Resource", "Columns", "Decoration"))
	design$Decoration <- if (is.null(design$Decoration)) NULL else fixx(design$Decoration, c("Separator", "Brackets", "Remove_Empty_Columns"), defaults = list(" ", c("<", ">"), TRUE))
	design
}

is_valid_design <- function (design) {
	d <- fix(design)
	if (! is.character(d$Resource)) stop(paste0("Resource of Design is ", typeof(d$Resource), " but must be character."))
	if (! is.null(d$Columns) && ! is.character(d$Columns) && ! is.list(d$Columns)) warning(paste0("Columns of Design is ", typeof(d$Columns), " but must be character, list or NULL."))
	if (! is.null(d$Decoration) && ! is.list(d$Decoration)) warning(paste0("Decoration of Design is ", typeof(d$Decoration), " but must be list or NULL."))
	if (is.list(d$Decoration)) {
		d <- d$Decoration
		if (! is.null(d$Separator) && ! is.character(d$Separator)) stop(paste0("Separator of Design's Decoration is ", typeof(d$Resource), " but must be character or NULL."))
		if (! is.null(d$Brackets) && ! is.character(d$Brackets) ) stop(paste0("Brackets of Design's Decoration is ", typeof(d$Brackets), " but must be character or list."))
		if (! is.null(d$Remove_Empty_Columns) && ! is.logical(d$Remove_Empty_Columns)) stop(paste0("Remove_Empty_Columns of Design's Decoration is ", typeof(d$Remove_Empty_Columns), " but must be logical or NULL."))
	}
	TRUE
}


is_valid_design(
	design = list(
		"//Patient",
		list(
			ID = "id"
		),
		list(
			" ",
			Remove_Empty_Columns = FALSE
		)
	)
)

(f <- fix(
	design = list(
		Decoration = list(
			Remove_Empty_Columns = FALSE,
			Brackets = c('[', ']'),
			" >> "
		),
		"//Patient",
		Columns = list(
			ID = "id"
		)
	)
))

designs = list(
	Pat = list(
		Resource = "//Patient"
	),
	Pat = list(
		Resource = "//Patient",
		Columns = ".//*"
	),
	Pat = list(
		Resource = "//Patient",
		Columns  = list(
			ID = "id"
		)
	),
	Pat = list(
		Resource = "//Patient",
		Columns = list(
			ID = "id"
		),
		Decoration = list(
			Separator = " ",
			Brackets = c("[", "]"),
			Remove_Empty_Columns = TRUE
		)
	),
	Pat = list(
		Resource = "//Patient",
		Columns = ".//*",
		Decoration = list(
			Separator = " ",
			Brackets = c("[", "]"),
			Remove_Empty_Columns = TRUE
		)
	),
	Pat = list(
		Resource = "//Patient",
		Columns = NULL, # wuerde man nicht machen
		Decoration = list(
			Separator = " ",
			Brackets = c("[", "]"),
			Remove_Empty_Columns = TRUE
		)
	),
	Pat = list(
		Resource = "//Patient",
		Columns = NULL, # wuerde man nicht machen
		Decoration = list(
			Separator = " ",
			Brackets = "#"
		)
	),
	Pat = list(
		Resource = "//Patient",
		Columns = NULL, # wuerde man nicht machen
		list(
			Separator = " "
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
		Resource = "//Patient",
		Decoration = list(
			Remove_Empty_Columns = TRUE
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
assign("last.warning", NULL, envir = baseenv())
(designs_fixed <- lapply(designs, fix))
warnings()
(is_valid_Design <- all(sapply(designs, is_valid_design)))
warnings()

