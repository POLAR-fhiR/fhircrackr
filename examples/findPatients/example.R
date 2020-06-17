#This example searches for all FHIR Resources:Patient 
#This example works only at the hapi.fhir.org server.
#start this example with
#Rscript example.R -o result
#last change: 2020_06_16

#devtools::install_github("POLAR-fhiR/fhiR", quiet = T)
rm( list = ls( ) )
endpoint <- "https://hapi.fhir.org/baseR4"

fhir_search_request  <- paste0(
	"Patient?",
	"gender=female",
	"&_format=xml",
	"&_pretty=true",
	"&_count=500000")

url <- fhiR::paste_paths(path1 = endpoint, path2 = fhir_search_request)

tables_design <- list(
	Patients = list(
		".//Patient",
		list( 
			PID             = "id/@value", 
			NAME.VERWENDUNG = "name/use/@value", 
			VORNAME         = "name/given/@value", 
			NACHNAME        = "name/family/@value",
			GESCHLECHT      = "gender/@value", 
			GEBURTSTAG      = "birthDate/@value" 
		)
	)
)


bundles <- fhiR::get_bundle(request = url, verbose = T)

list_of_tables <- fhiR::bundle2dfs(bundle = bundles, design = tables_design)

out_dir <- "result"

if(! dir.exists(out_dir)) {
  
  cat(paste0("   - create directory: ", out_dir, "...\n"))
  
  dir.create(out_dir, recursive = T)
  
} else cat(paste0("   - directory ", out_dir, " already exists. do nothing...\n"))

cat("   - write csv tables...\n")

for(n in names(list_of_tables)) {
	
	write.table(list_of_tables[[ n ]], file = paste0( "result/",n, ".csv" ), na = "", sep = ";", dec = ".", row.names = F, quote=F)
}

cat("   - write tables.RData...\n")
save(list_of_tables, file = "result/tables.RData")
cat("   - finish\n")






