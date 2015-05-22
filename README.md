Erklärung zu SVG
----------------

Die Erklärung bezieht sich speziell auf die Testdatei hier.

In einem `<path>`-Element gibt es in der Pfaddefinition viele Koordinatenpaare (Koordinaten mit Komma getrennt).
Jedem Koordinatenpaar ist ein Buchstabe vorangestellt:

 * `M`: move to (absolute)
 * `l`: line to (relative)
 * `L`: line to (absolute)

Außerdem gibt es:

 * `z`: close path

http://www.w3.org/TR/SVG/paths.html

Großbuchstabe bedeutet also "absolute", Kleinbuchstabe bedeutet "relative" Bewegungen.

 * nach `z` ist immer Ende oder `M`
 * nach `M` ist immer `l`
 * nach `L` ist entweder `l` oder `z`
 * wenn nach `z` nicht Ende ist, heißt das wohl, dass in der Mitte des Polygon ein
   Stück herausgeschnitten wird

Inside-polygon Test
-------------------

Wir nehmen einen Punkt außerhalb, und schauen wieviele Schnittpunkte
die Verbindungslinie zum fraglichen Punkt mit einer Area hat.

 * Gerade Anzahl -> liegt nicht in Area
 * Ungerade Anzahl -> liegt in Area

Welchen Punkt wählen wir außerhalb?  Minimaler x-Wert aller Polygonpunkte
von Area minus 1 als x, y-Wert des fraglichen Punktes als y.

Das geht aber nur, wenn keine zwei Ecken von Polygonkanten beide 0 als y-Koordinate haben!
Sonst schneidet die Verbindungsstrecke möglicherweise die Kante auf voller Länge, dann ist keine Aussage möglich.
