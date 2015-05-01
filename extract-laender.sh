#!/bin/bash

awk -F\" '/\/>/{land=""}; land; /^<path id="/{land=$2; print "n" land}' DeutschlandMitStaedten.svg \
| sed 's/Baden__x26__Württemberg/Baden-Württemberg/' \
| awk -F, '{print substr($1,1,1) OFS substr($1,2) OFS $2}'
