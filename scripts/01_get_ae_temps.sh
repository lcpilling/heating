#!/bin/bash

# get temps.txt
mysql -u luke -p${SQL_PWD} -Dhomeservices -e "select * from action_events" > action_events.txt

# filter to Heating TempOn
head -n1 action_events.txt > action_events2.txt
grep "TempOn" action_events.txt >> action_events2.txt

# just keep cols 3:4
cut -f 3,4 action_events2.txt > action_events3.txt

# split action col by , and remove 2nd col
head -n1 action_events3.txt > action_events4.txt
tail -n +2 action_events3.txt | sed 's/,/\t/g' | sed 's/Heating //g' | cut -f 1,3 >> action_events4.txt


# get temps.txt
mysql -u luke -p${SQL_PWD} -Dhomeservices -e "select * from temps" > temps.txt

# filter to probe of interest 28-0516804150ff
#head -n1 temps.txt > temps2.txt
#grep "28-0516804150ff" temps.txt >> temps2.txt

# just keep cols 3:5
#cut -f 3- temps2.txt > temps3.txt

