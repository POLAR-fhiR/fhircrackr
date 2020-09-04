# Protokoll über Änderungen in fhircrackr für Thomas nach dem Urlaub

## 25.08.2020
- branch: crack_by_design_jg
- Funktionen: fix_df_desc / is_valid_df_desc

-------------------------------------------------------

### Problem: 
Weil is_valid zunächst fix aufruft, wird eine (uninformative) Warnung geworfen, wenn die df_description falsche Datentypen enthält (z.B. data.frame statt list für df_desc$style), nämlich "Argument list is data.frame but must be list or NULL".

Aber wenn man is_valid ohne das vorherige fix aufruft, dann funktioniert is_valid nicht, weil es auf die Namen der Elemente (resource, cols, style) zurückgreift und die ohne fix ggf. noch nicht da sind.

### Lösung:
fix reicht Warn-Nachrichten jetzt an is_valid durch, so dass in der Ausgabe nachvollziehbar ist, aufgrund welches Teiles des design die Warnung zustande kam

----------------------------------------

### Problem: 
Wenn resource, cols und style ohne Namen und in der falschen Reihenfolge angegeben werden, verteilt fix die Namen falsch.

### Lösung:
Wenn Listenelemente namenlos sind, wirft fix jetzt eine Warnung, die beschreibt, welche Reihenfolge von Elementen angenommen wird.


-----------------------------------------
-----------------------------------------

## 02.09.2020
- branch: crack_by_design_jg
- Funktionen: save_design load_design

### Problem:
Beide Funktionen passen nicht ins Namensschema, weil sämtliche andere Funktionen mit fhir_ beginnen

### Lösung: 
Funktionsnamen in fhir_save_design und fhir_load_design geändert. Außerdem Validitätscheck in fhir_save_design eingebaut.

------------------------------------------
------------------------------------------

## 03.09.2020
- branch: crack_by_design_jg
- Funktion : bundle2df()

### Problem: 
Wenn eine Fehlerhafte Ressource angegeben wird, wird die Warnmedlung, dass die Ressource nicht im Bundle auftaucht wiederholt für jedes Bundle geprintet, d.h. bei vielen Bundles hat man die identische Warnmeldung ganz oft untereinander. 

### Lösung:
Weil ja sowieso einfach ein leerer df zurückgegeben wird, wenn die Ressource nicht im bundle steckt und "nichts schlimmes" passiert, habe ich die Warnung jetzt erstmal auskommentiert.

--------------------------------------------
--------------------------------------------

## 04.09.2020
- branch: crack_by_design_jg

### Aufräumarbeiten
is_invalid_design() (aus helpers.R) is jetzt überflüssig und wurde durch is_valid_design (aus design.R) ersetzt. Habe is_invalid_design() deshalb gelöscht.

add_attribute_to_design() und remove_attribute_from_design() sind jetzt aus helpers.R nach design.R umgezogen.
