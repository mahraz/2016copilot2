if(!require(stringr)){install.packages('stringr')}
if(!require(statcheck)){install.packages('statcheck')}

# Printing sessioninfo for run
sessionInfo()

# Uncomment if you want to recollect data
# source('functions/extract_data_api.R)

# Read in latest data
latest <- tail(list.files(recursive = TRUE)[grep('all_res',
                                                 list.files(
                                                   recursive = TRUE
                                                 ))], 1)

all_res <- read.csv(latest, header = TRUE, stringsAsFactors = FALSE)

latest <- tail(list.files(recursive = TRUE)[grep('psych_res',
                                                 list.files(
                                                   recursive = TRUE
                                                 ))], 1)

psych_res <- read.csv(latest, header = TRUE, stringsAsFactors = FALSE)

# Cleaning latest 'all_res.csv' -------------------------------------------


# Cleaning latest 'psych_res.csv' -----------------------------------------

## Extract author contributions

psych_res$nr_pval <- NULL
psych_res$nr_error <- NULL
psych_res$nr_gross_error <- NULL


## Run statcheck on each results section
for(i in 1:dim(psych_res)[1]){
  res <- statcheck(psych_res$everything[i])
  res$Source <- psych_res$id[i]
  
  
  psych_res$nr_pval[i] <- dim(res)[1]
  psych_res$nr_error[i] <- sum(res$Error)
  psych_res$nr_gross_error[i] <- sum(res$DecisionError)
  
}