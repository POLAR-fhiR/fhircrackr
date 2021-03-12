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
		txt_ <- txt_[txt_ != "" & !grepl("^#", txt_)]
		txt_ <- gsub("\".*\"", "", txt_)
		txt_ <- gsub("(.*)#", "\\1", txt_)
		txt_
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
	plot_ly() %>% add_histogram(factor(really_all_funs[dep$fromto$from],really_all_funs), name = "how many functions uses this function")
)

plot(dep)

###
#Find hierarchy for tests
previous_level <- dep$Nomfun[dep$Nomfun$label %in% bases]$id #starting point: base functions

hierarchy <- list(previous_level)

this_level <- NA

while(length(previous_level) < length(really_all_funs)){

	set <- unique(dep$fromto$from[dep$fromto$to %in% previous_level]) #depend on previous level
	rest <- unique(dep$Nomfun[!dep$Nomfun$id %in% previous_level]$id) #depend (also) on higher level functions

	exclude <- unique(dep$fromto$from[dep$fromto$to %in% rest]) #functions in set that also use higher level

	this_level <- unique(setdiff(set, exclude)) #functions in set that only use previous level
	this_level <- this_level[!this_level %in% previous_level] #drop the ones already in previous

	hierarchy <- append(hierarchy,list(this_level))

	previous_level <- c(this_level, previous_level)
}

hierarchy_names <- lapply(hierarchy, function(x){dep$Nomfun$label[dep$Nomfun$id %in% x]})

i <- 1
dep_sub <- dep
dep_sub$Nomfun <- dep$Nomfun[id %in% c(hierarchy[[i]], hierarchy[[i+1]]),]
plot(dep_sub)

