existing_resource_types <-  c("Account","ActivityDefinition","AdverseEvent","AllergyIntolerance",
"Appointment","AppointmentResponse","AuditEvent","Basic",
"Binary","BiologicallyDerivedProduct","BodyStructure","Bundle",
"CapabilityStatement","CarePlan","CareTeam","CatalogEntry",
"ChargeItem","ChargeItemDefinition","Claim","ClaimResponse",
"ClinicalImpression","CodeSystem","Communication","CommunicationRequest",
"CompartmentDefinition","Composition","ConceptMap","Condition",
"Consent","Contract","Coverage","CoverageEligibilityRequest",
"CoverageEligibilityResponse","DetectedIssue","Device","DeviceDefinition",
"DeviceMetric","DeviceRequest","DeviceUseStatement","DiagnosticReport",
"DocumentManifest","DocumentReference","EffectEvidenceSynthesis","Encounter",
"Endpoint","EnrollmentRequest","EnrollmentResponse","EpisodeOfCare",
"EventDefinition","Evidence","EvidenceVariable","ExampleScenario",
"ExplanationOfBenefit","FamilyMemberHistory","Flag","Goal",
"GraphDefinition","Group","GuidanceResponse","HealthcareService",
"ImagingStudy","Immunization","ImmunizationEvaluation","ImmunizationRecommendation",
"ImplementationGuide","InsurancePlan","Invoice","Library",
"Linkage","List","Location","Measure",
"MeasureReport","Media","Medication","MedicationAdministration",
"MedicationDispense","MedicationKnowledge","MedicationRequest","MedicationStatement",
"MedicinalProduct","MedicinalProductAuthorization","MedicinalProductContraindication","MedicinalProductIndication",
"MedicinalProductIngredient","MedicinalProductInteraction","MedicinalProductManufactured","MedicinalProductPackaged",
"MedicinalProductPharmaceutical","MedicinalProductUndesirableEffect","MessageDefinition","MessageHeader",
"MolecularSequence","NamingSystem","NutritionOrder","Observation",
"ObservationDefinition","OperationDefinition","OperationOutcome","Organization",
"OrganizationAffiliation","Patient","PaymentNotice","PaymentReconciliation",
"Person","PlanDefinition","Practitioner","PractitionerRole",
"Procedure","Provenance","Questionnaire","QuestionnaireResponse",
"RelatedPerson","RequestGroup","ResearchDefinition","ResearchElementDefinition",
"ResearchStudy","ResearchSubject","RiskAssessment","RiskEvidenceSynthesis",
"Schedule","SearchParameter","ServiceRequest","Slot",
"Specimen","SpecimenDefinition","StructureDefinition","StructureMap",
"Subscription","Substance","SubstanceNucleicAcid","SubstancePolymer",
"SubstanceProtein","SubstanceReferenceInformation","SubstanceSourceMaterial","SubstanceSpecification",
"SupplyDelivery","SupplyRequest","Task","TerminologyCapabilities",
"TestReport","TestScript","ValueSet","VerificationResult",
"VisionPrescription")



female_pat_bundle <- fhir_sample_resources(
	base_url    = "http://hapi.fhir.org/baseR4",
	resource    = "Patient",
	parameters  = c(gender = "female", birthdate = "lt1960-01-01"),
	sample_size = 10
)

female_pat_bundle <- fhir_serialize(female_pat_bundle)

usethis::use_data(existing_resource_types,female_pat_bundle, internal = TRUE, overwrite = TRUE)
