# Data

Data were collected from the PLOS API with the package `rplos`. Considering the amount of data collected, we split up the initial data collection into separate files. These were conjoined into three data files with the following terminal commands (`pwd` for these commands = data folder).

1. all_meta.csv: `find all_meta -type f -print | xargs cat names_all.csv > all_meta.csv`
2. psych_meta.csv: `cat names_psych.csv psych_meta/* > psych_meta.csv`
3. psych_statcheck.csv: `cat names_statcheck.csv psych_statcheck/* > psych_statcheck.csv`