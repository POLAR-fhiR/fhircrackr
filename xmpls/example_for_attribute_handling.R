df_design <- list(

 #define specifically which elements to extract
	MedicationStatement = list(

		".//MedicationStatement",

		list(
			MS.ID              = "id/@value",
			STATUS.TEXT        = "text/status/@value",
			STATUS             = "status/@value",
			MEDICATION.SYSTEM  = "medicationCodeableConcept/coding/system",
			MEDICATION.CODE    = "medicationCodeableConcept/coding/code/@value",
			MEDICATION.DISPLAY = "medicationCodeableConcept/coding/display",
			DOSAGE             = "dosage/text/@value",
			PATIENT            = "subject/reference",
			LAST.UPDATE        = "meta/lastUpdated"
		)
	),

 #extract all values
	Patients = list(

		".//Patient", "./@value"
	)
)

(df_design_w_value <- add_attribute_to_design(df_design,"value"))
(df_design_wo_value <- remove_attribute_from_design(df_design_w_value))
(df_design_w_value <- add_attribute_to_design(df_design_wo_value,"value"))
