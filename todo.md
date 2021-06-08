## To Do

### fhir_url
-  url_enc = TRUE in  hinzufuegen.

### fhir_resource_type()
Sollte 'Changing resource type "medicationstatement" into "MedicationStatement".' nicht besser eine Warnung sein?

Wie reagieren wir, wenn es wirklich einmal klein geschriebene Resourcen gibt in Zukunft?

Wir entfernen das Correcting einfach wieder? Sollte ja dann eigentlich kein Problem sein.


### Bemerkungen zu den Vignetten

#### Links

Wir sollten in allen Vignetten alle Links als html code schreiben. Damit diese nicht im Tab der Vignette geoeffnet werden.

```<a href="https://www.hl7.org/fhir/capabilitystatement.html" target="_blank">capability statement</a>```

<a href="https://www.hl7.org/fhir/capabilitystatement.html" target="_blank">open capability statement page in a new tab!</a>  
statt  
[open capability statement page in this tab!](https://www.hl7.org/fhir/capabilitystatement.html)!
  
  
#### Zeilenumbrueche von Ausgaben in Code-Blöcken

Bin auf folgende Loesung gekommen.  
1. Erstmal Compilieren.  
2. Dann alle Original-Ausgaben des Code-Blockes zurueck in den Code-Block kopieren (Ctl-C, Ctl-V).  
3. Jetzt da kuerzen, wo es noetig ist.  
4. Zum Schluss noch das Argument ```{r,results='hide'}``` angeben.  
5. Voila! ;O)  
  
    
#### Zeilenumbrueche von Warnungen in Code-Blöcken
Bei Warnungen funktioniert das analog, nur muss man eben dann das Argument ```{r,warning=FALSE}``` einschalten.
  

### downloadResources.Rmd
alle Links und Zeilenumbrueche angepasst.


### der bekannte Fehler

Bekomme in flattenResource in Zeile 449 immernoch den Fehler:

Fuehre ich die chunks einzeln aus, laufen sie.

Dann wird aber auch library(fhircrackr) aufgerufen.

Fuehre ich aber devtools::load_all() aus,  
nach dem ich fhircrackr detached habe,  
kommt der Fehler.



### README
3 Links
3 x unterschiedliches Verhalten ???




<a href="" target="_blank"></a>
