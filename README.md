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
