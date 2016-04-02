#!/usr/bin/python


import sys, csv;
for i in range(1, len(sys.argv)):
    with open(sys.argv[i], 'rb') as datafile:
        iterator = csv.reader(datafile, delimiter=";")
        asset = "";
        unit = 1;
        for row in iterator:
            if row[2] != asset:
                if asset != "":
                    print ";"
                print("create table %s_SEK_Rates ("
                "day date primary key not null,"
                "rate double);" % row[2]);
                print("insert into %s_SEK_Rates values " % row[2]),
                asset=row[2];
                if int(row[1]) != unit:
                    unit = int(row[1]);
            else:
                print ","
            print ("(\"%s\", %f)" % (row[0], float(row[3])/unit)),
        print ";"
