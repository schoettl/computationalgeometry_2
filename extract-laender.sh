#!/bin/bash

awk -F\" '/\/>/{land=""}; land; /^<path id="/{land=$2; print "n" land}' DeutschlandMitStaedten.svg \
| sed 's/Baden__x26__Württemberg/Baden-Württemberg/' \
| awk -F, 'BEGIN{OFS=" "};{print substr($1,1,1), substr($1,2), $2}' \
| awk '{if($1!="n"){ if(split($3, arr, "H")>1){print $1, $2, arr[1]; print "H", arr[2], 0} else print;} else print}'
