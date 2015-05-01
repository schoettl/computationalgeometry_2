#!/bin/bash

awk -F\" '/^[[:space:]]+id="/{city=$2}; /cx=/{x=$2}; /cy=/{y=$2; print city OFS x OFS y}' DeutschlandMitStaedten.svg
