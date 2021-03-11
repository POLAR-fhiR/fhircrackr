rm(list = ls())

name <- function(v){
	names(v) <- v
	v
}

all_scripts <- list.files("R", ".R")

all_txt <- lapply(
	name(all_scripts),
	function(as) {
		readLines(paste0("R/", as))
	}
)


all_txt <- lapply(
	all_txt,
	function(txt_) {
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

dep$fromto <-data.table::rbindlist(
	lapply(
		all_txt,
		function(txt_) {
			#txt_ <- all_txt[[5]]
			data.table::rbindlist(
				lapply(
					really_all_funs,
					function(raf) {
						#raf <- really_all_funs[[9]]
						from <- grep(paste0("^", raf, " *<- *function *\\("), txt_)
						if(0 < length(from)) {
							if(1 < length(from)) print(raf)
							to <- grep("^}$", txt_)
							to <- to[min(which(0 < to - from))]
							id <- dep$Nomfun$id[dep$Nomfun$label == raf]
							emb_funs <- sapply(really_all_funs, grep, txt_[from:to])
							emb_funs <- names(emb_funs[sapply(emb_funs, function(ef) 0 < length(ef))])
							data.table::data.table(from = id, to = match(emb_funs, really_all_funs))
						}
					}
				)
			)
			# all_all <- all_all[!sapply(all_all, is.null)]
			# a
		}
	)
)

dep$fromto <- unique(dep$fromto[dep$fromto$from!=dep$fromto$to,])

attributes(dep)
attr(dep, "class") <- "dependenciesGraphs"
dep
dep_
plot(dep)

