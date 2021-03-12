rm(list = ls())

require(DependenciesGraphs)
require(plotly)
require(data.table)

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

dep <- list(Nomfun = data.table(label = really_all_funs, id = seq_along(really_all_funs)))

dep$fromto <- rbindlist(
	lapply(
		all_txt,
		function(txt_) {
			#txt_ <- all_txt[[5]]
			rbindlist(
				lapply(
					really_all_funs,
					function(raf) {
						#raf <- really_all_funs[[22]]
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
								data.table(from = id, to = match(emb_funs, really_all_funs))
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

(froms <- sort(dep$Nomfun$label[unique(dep$fromto$from)]))
(tos <- sort(dep$Nomfun$label[unique(dep$fromto$to)]))

# base functions,
# don't use any functions itself
(bases <- setdiff(
	really_all_funs,
	sort(dep$Nomfun$label[unique(dep$fromto$from)])
))

# end functions,
# are not used by any other functions
(ends <- setdiff(
	really_all_funs,
	sort(dep$Nomfun$label[unique(dep$fromto$to)])
))

### bases and ends at the same time
### have now arrows
intersect(ends, bases)

###
### some statistics
subplot(
	nrows = 2, shareX = T,
	plot_ly() %>% add_histogram(factor(really_all_funs[dep$fromto$to], really_all_funs), name = "how often is this function used"),
	plot_ly() %>% add_histogram(factor(really_all_funs[dep$fromto$from],really_all_funs), name = "how many function uses this function")
)

