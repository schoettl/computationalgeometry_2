#!/bin/bash

usage="test/test-brd.sh"

function calculateRatios() {
    awk 'NR==1{s=$2}; {print $1, $2/s}'
}

temp=$(mktemp)
haskell/area -p staedte.txt laender.txt > "$temp"

sepLineNo=$(awk '/^===/{print NR}' "$temp")
head -n $((sepLineNo-1)) "$temp" | sort | calculateRatios > test/computed-part1.txt
tail -n +$((sepLineNo+1)) "$temp" | sort > test/computed-part2.txt

awk '{print $1, $2}' test/bundeslaender.txt \
| sort \
| calculateRatios \
| awk '!/Bundesrepublik/' \
> test/real-part1.txt

awk '{print $3, "[" $1 "]"}' test/bundeslaender.txt \
| sort \
| awk '!/Bundesrepublik/' \
> test/real-part2.txt

echo "Vergleiche computed-part1.txt mit real-part1.txt"
echo "und        computed-part2.txt mit real-part2.txt"
echo "im Ordner test/ -- zum Beispiel mit Meld"
