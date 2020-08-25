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