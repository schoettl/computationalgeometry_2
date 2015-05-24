#!/bin/bash

usage="$0"


function calculateRatios() {
    awk 'NR==1{s=$2}; {print $1, $2/s}'
}

temp=$(mktemp)
haskell/area -p staedte.txt laender.txt > "$temp"

tmpPart1=$(mktemp)
tmpPart2=$(mktemp)

sepLineNo=$(awk '/^===/{print NR}' "$temp")
head -n $((sepLineNo-1)) "$temp" | sort | calculateRatios > "$tmpPart1"
tail -n +$((sepLineNo+1)) "$temp" | sort > "$tmpPart2"

awk '{print $1, $2}' test/bundeslaender.txt \
| sort \
| calculateRatios \
| awk '!/Bundesrepublik/' \
| diff -u "$tmpPart1" -

echo "================================================================================"

awk '{print $3, "[" $1 "]"}' test/bundeslaender.txt \
| sort \
| awk '!/Bundesrepublik/' \
| diff -u "$tmpPart2" -
