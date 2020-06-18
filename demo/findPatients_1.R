#This example searches for all FHIR Resources:Patient
#This example works only at the hapi.fhir.org server.


tables_design <- list(
	Patients = list(
		".//Patient",
		list(
			PID           = "id/@value",
			NAME.USE      = "name/use/@value",
			NAME.GIVEN    = "name/given/@value",
			NAME.FAMILY   = "name/family/@value",
			SEX           = "gender/@value",
			BIRTHDAY      = "birthDate/@value"
		)
	)
)


bundles <- fhiR::fhir_search("https://hapi.fhir.org/baseR4/Patient?", max.bundles=3)

list_of_tables <- fhiR::fhir2dfs(bundle = bundles, design = tables_design)
cat("   - write tables as csv and Rdata files to disk \n")
for(n in names(list_of_tables)) {

	write.table(list_of_tables[[ n ]], file = paste0(n, ".csv"), na = "", sep = ";", dec = ".", row.names = F, quote=F)
}

save(list_of_tables, file = "tables.RData")
cat("   - finish\n")






