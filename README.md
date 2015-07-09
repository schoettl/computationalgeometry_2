Polygonflächen berechnen und Punkte zuordnen
============================================

Diese Aufgabe besteht aus zwei Teilen:

 1. Es müssen die Flächen der deutschen Bundesländer aus Polygonzügen berechnet.
 2. Es muss zu jeder Landeshauptstadt, gegeben durch Koordinaten, ermittelt
    werden, in welchem Bundesland sie liegt.

Die Eingabedaten liegen als SVG-Datei vor (scalable vector graphic).

Wir haben diese Aufgabe in der funktionalen Sprache Haskell gelöst.  Das
Programm wird wie folgt aufgerufen:

    $ haskell/area
    area version 1.0

    usage:
    area [ -p <pointfile> ] <areafile>
    area [ -q <point> ] <areafile>

    options:
    -p, --points=<pointfile>  for each point in this file,
        test in which areas it is and output the result
    -q, --point=<point>  for this point, test in which areas
        it is and output the result (format: x,y e.g.  1.5,4.2)

Eingabedaten
------------

Auf Anhieb haben wir kein vielversprechendes Package zur Verarbeitung von
SVG-Dateien gefunden, deshalb haben wir die Länder- und die Städtedaten mit GNU
Awk aus der SVG-Datei extrahiert und in eine einfachere Form gebracht.  Im
`Makefile` werden dazu die Skripte `extract-laender.sh` und `extract-staedte.sh`
aufgerufen.

Die neuen Eingabedateien haben ein einfacher einlesbares Format.  Mit folgenden
Befehlen kann man sich das anschauen:

    make
    less laender.txt
    head staedte.txt

### Details zu SVG

Die Beschreibung beschränkt sich auf die wichtigsten Element in
`DeutschlandMitStaedten.svg`.  Eine SVG-Datei ist eine XML-Datei mit speziellen
Elementen, Attributen und Werten.

In einem `<path>`-Element gibt es in der Pfaddefinition viele Koordinatenpaare
(Koordinaten mit Komma getrennt).  Jedem Koordinatenpaar ist ein Buchstabe
vorangestellt:

 * `M`: move to (absolute)
 * `l`: line to (relative)
 * `L`: line to (absolute)

Außerdem gibt es:

 * `z`: close path
 * `H`: horizontal line to (absolute)  
   Dies ist ein Spezialfall, der in der ganzen Datei nur einmal auftaucht.
   Hinter `H` steht nur *eine* Koordinate (die x-Koordinate).

Gemäß Spezifikation gibt es neben `H` auch `h` und `V`/`v` (vertical line to),
diese tauchen hier aber nicht auf.

http://www.w3.org/TR/SVG/paths.html

### Einlesen der Dateien

Großbuchstabe bedeutet also absolute, Kleinbuchstabe bedeutet relative Bewegungen.

 * nach `z` ist immer Ende oder `M`
 * nach `M` ist immer `l`
 * nach `L` ist entweder `l` oder `z`
 * wenn nach `z` nicht Ende ist, heißt das wohl, dass in der Mitte des Polygon ein
   Stück herausgeschnitten wird

Die Implementierung zum Parsen der Dateien befindet sich im Ordner `haskell/` im
Modul `CG/CG2Data`.  Die Funktion zum Parsen der Flächendatei ist folgendermaßen
definiert:

    parseAreaData :: String -> [Area]
    parseAreaData = asAreas . convert . splitPolygons . groupData . parseData . readData

Sie wandelt also den Dateiinhalt von `laender.txt` (ein `String`) in eine Liste
von `Area`s um.  Die Punktschreibweise ist die Syntax für Funktionskomposition.
Es wird also zuerst die Funktion `readData` (ganz rechts) auf das Argument
angewendet.  Auf das Ergebnis wird dann die Funktion `parseData` angewendet,
darauf wiederum die Funktion `groupData` usw.

Die Definition von `Area` ist:

    data Area = Area { areaName::AreaName, polygons::[Polygon] }

`Polygon` ist wiederum eine Liste von Punkten, wobei der Anfangs- und Endpunkt
zweimal in der Liste stehen muss, nämlich am Anfang und am Ende.


Fläche berechnen
----------------

Ein oder mehrere Polygone definieren eine Fläche.  Für jedes Segment der
Polygone wird die Dreiecksfläche zum Ursprung berechnet.  Diese Flächen werden
aufaddiert.  Das funktioniert genauso, wenn eine Fläche aus mehreren Polygonen
besteht, weil die Polygone passend orientiert sind.  Zum Beispiel ist ein
inneres Polygon, das aus der umgebenden Fläche ein Stück herausschneidet,
clockwise orientiert, wenn das äußere Polygon counter-clockwise orientiert ist.


Test in welcher Fläche ein Punkt liegt
--------------------------------------

Wir nehmen einen Punkt außerhalb, und überprüfen wieviele Schnittpunkte
die Verbindungslinie zum fraglichen Punkt mit einer Area hat.

 * Gerade Anzahl -> liegt nicht in Area
 * Ungerade Anzahl -> liegt in Area

Welchen Punkt wählen wir außerhalb?  Minimaler x-Wert aller Polygonpunkte
von Area minus 1 als x, y-Wert des fraglichen Punktes als y.

Dabei darf aber keine Polygonkante waagrecht sein und auf selber y-Höhe liegen
wie die beiden Punkte.  In diesem Fall gibt es nämlich unendlich viele
Schnittpunkte von Verbindungslinie und Polygon.  Vor der Berechnung wird diese
Bedingung überprüft und gegebenenfalls mit einem Fehler abgebrochen (Pech
gehabt).

Test unserer Ergebnisse
-----------------------

Zum Testen unseres Programms haben wir zwei Polygone aufgezeichnet und dessen
Punkte abgespeichert in der Datei `test/test.txt`.  Da funktioniert das
Programm schon mal richtig.

Außerdem haben wir zum Testen unserer Ergebnisse die Verhältnisse zur
Gesamtfläche berechnet.  Diese können mit den echten Größenverhältnissen
verglichen werden.  Die Daten dazu haben wir aus Wikipedia und in der Datei
`test/bundeslaender.txt` abgelegt.

Zum Test steht ein Skript zur Verfügung.  Aufruf: `test/test-brd.sh` (vorher
`make` aufrufen!)
