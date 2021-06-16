## To Do

### fhir_url
-  url_enc = TRUE in  hinzufuegen.

--> Done

### fhir_resource_type()
Sollte 'Changing resource type "medicationstatement" into "MedicationStatement".' nicht besser eine Warnung sein?

Wie reagieren wir, wenn es wirklich einmal klein geschriebene Resourcen gibt in Zukunft?

Wir entfernen das Correcting einfach wieder? Sollte ja dann eigentlich kein Problem sein.

--> Argument fix_cases = TRUE hinzugefügt, wenn FALSE, dann wird nicht korrigiert und es kommt die Warnung, 
dass die Ressource nicht in der Liste ist. Ein Warning würde ich aus der Korrektur-Info nicht machen , ich würde das als normales, nützliches Feature verwenden und die sollten keine Warnungen sondern nur messages werfen finde ich.

### Bemerkungen zu den Vignetten

#### Links

Wir sollten in allen Vignetten alle Links als html code schreiben. Damit diese nicht im Tab der Vignette geoeffnet werden.

```<a href="https://www.hl7.org/fhir/capabilitystatement.html" target="_blank">capability statement</a>```

<a href="https://www.hl7.org/fhir/capabilitystatement.html" target="_blank">open capability statement page in a new tab!</a>  
statt  
[open capability statement page in this tab!](https://www.hl7.org/fhir/capabilitystatement.html)!
  
--> Okay, hast du ja schon korrigiert, ne? 

#### Zeilenumbrueche von Ausgaben in Code-Blöcken

Bin auf folgende Loesung gekommen.  
1. Erstmal Compilieren.  
2. Dann alle Original-Ausgaben des Code-Blockes zurueck in den Code-Block kopieren (Ctl-C, Ctl-V).  
3. Jetzt da kuerzen, wo es noetig ist.  
4. Zum Schluss noch das Argument ```{r,results='hide'}``` angeben.  
5. Voila! ;O)  
  
--> Bisschen hacky aber nice, da bin ich nicht drauf gekommen =D
    
#### Zeilenumbrueche von Warnungen in Code-Blöcken
Bei Warnungen funktioniert das analog, nur muss man eben dann das Argument ```{r,warning=FALSE}``` einschalten.
  
--> Top
### downloadResources.Rmd
alle Links und Zeilenumbrueche angepasst.

--> Top

### der bekannte Fehler

Bekomme in flattenResource in Zeile 449 immernoch den Fehler:

Fuehre ich die chunks einzeln aus, laufen sie.

Dann wird aber auch library(fhircrackr) aufgerufen.

Fuehre ich aber devtools::load_all() aus,  
nach dem ich fhircrackr detached habe,  
kommt der Fehler.

--> Ich bekomme den Fehler nicht, egal was ich mache...


### README
3 Links
3 x unterschiedliches Verhalten ???


--> Readme wollte ich ja in Zukunft eigentlich nicht immer gesondert anpassen, das ist so viel Arbeit.
Deshalb eigentlich die Lösung über das .Rmd, das einfach eine Kopie vom Intro ist, die das Github-fähige .md erzeugt (Hatte ich nur vergessen zu aktualisieren). Können wir das beibehalten? Ich glaube nicht, dass das Link-Verhalten die Zusatzarbeit rechtfertigt, die wir haben, wenn wir das Readme in Zukunft immer gesondert pflegen müssen.




download_resources.R
row ~ 108

	####remove at some point####
	if(is.logical(save_to_disc)){
		warning("The use of TRUE/FALSE in the argument save_to_disc in combination with the argument directory is ",
				"deprecated. Please specify the directory name in the save_to_disc argument directly (see ?fhir_search).")
		if(save_to_disc){
			message("Setting save_to_disc to '", directory, "'.")
			save_to_disc <- directory
			}
	}




















<a href="" target="_blank"></a>
