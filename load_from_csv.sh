#!/bin/bash

cat ~/Downloads/*.csv | cut -f 1,3,4 -d";" | tail -n +2 | sed -e 's/ /;/g' > /tmp/2.csv
rm ~/Downloads/*.csv
./load_from_csv.py /tmp/2.csv

## find ~/Downloads -name '*.csv' -mmin -3
