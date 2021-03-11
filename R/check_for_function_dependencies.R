rm(list = ls())

name <- function(v){
	names(v) <- v
	v
}

all_scripts <- setdiff(list.files("R", ".R"), "check_for_function_dependencies.R")

all_txt <- lapply(
	name(all_scripts),
	function(as) {
		txt_ <- readLines(paste0("R/", as))
		txt_[txt_ != "" & !grepl("^#", txt_)]
	}
)

all_funs <- lapply(
	all_txt,
	function(txt_) {
		sub("^ *([^ ]+) +<-.*", "\\1", txt_[grep(".*<- *function *\\(", txt_)])
	}
)

really_all_funs <- sort(unique(unlist(all_funs)))

dep <- list(Nomfun = data.table::data.table(label = really_all_funs, id = seq_along(really_all_funs)))

dep$fromto <- data.table::rbindlist(
	lapply(
		all_txt,
		function(txt_) {
			data.table::rbindlist(
				lapply(
					really_all_funs,
					function(raf) {
						from <- grep(paste0("^", raf, " *<- *function *\\("), txt_)
						if(0 < length(from)) {
							if(1 < length(from)) print(raf)
							to <- grep("^}$", txt_)
							to <- to[min(which(0 < to - from))]
							id <- dep$Nomfun$id[dep$Nomfun$label == raf]
							emb_funs <- sapply(
								really_all_funs,
								function(raf_) {
									grep(paste0(raf_, " *\\("), txt_[from : to])
								}
							)
							emb_funs <- names(emb_funs[sapply(emb_funs, function(ef) 0 < length(ef))])
							if(0 < length(emb_funs))
								data.table::data.table(from = id, to = match(emb_funs, really_all_funs))
						}
					}
				)
			)
		}
	)
)

dep$fromto <- unique(dep$fromto[from != to,])

attr(dep, "class") <- "dependenciesGraphs"

plot(dep)

# base functions,
# don't use any functions itself
setdiff(
	sort(dep$Nomfun$label[unique(dep$fromto$to)]),
	sort(dep$Nomfun$label[unique(dep$fromto$from)])
)

# end functions,
# are not used by any other functions
setdiff(
	sort(dep$Nomfun$label[unique(dep$fromto$from)]),
	sort(dep$Nomfun$label[unique(dep$fromto$to)])
)
