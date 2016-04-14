# Load dependencies
if(!require(rplos)){install.packages('rplos')}
if(!require(statcheck)){install.packages('statcheck')}
if(!require(stringr)){install.packages('stringr')}

# function to make first letter of each word uppercase
simpleCap <- function(x) {
  res <- NULL
  for (i in 1:length(x)){
    s <- strsplit(x[i], " ")[[1]]
    
    res[i] <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
                    sep = "", collapse = " ")
  }
  return(res)
}

# Printing sessioninfo for run
sessionInfo()

# set fields to download
field_all <- c('id', # DOI
               'publication_date',
               'journal', # All journals
               'author',
               'author_affiliate',
               'author_notes', # Contributions
               'competing_interest')

field_psych <- c('id', # DOI
                 'publication_date',
                 'journal', # Only PLOS ONE if all went well
                 'author',
                 'author_affiliate',
                 'author_notes', # Contributions
                 'everything',
                 'competing_interest')

# set searchterms
search_all <- '(doc_type:full) AND (article_type:"Research Article")'
# search_psych <- '(doc_type:full) AND (article_type:"Research Article") AND (subject:psychology) AND (cross_published_journal_key:PLoSONE)'
search_psych <- '(doc_type:full) AND (article_type:"Research Article") AND (subject:psychology)'

# Get number of hits
all_hits <- searchplos(q = search_all, fl = field_all, limit = 1)$meta$numFound
psych_hits <- searchplos(q = search_psych, fl = field_psych, limit = 1)$meta$numFound

size <- 900
loops_all <- as.integer(all_hits / size + ifelse(all_hits %% size > 0, 1, 0))

# If the loop breaks, just replace 1 in the next line with whatever i is
for (i in 1:loops_all){
# for (i in 1:2){
  res <- searchplos(q = search_all, 
                    fl = field_all, 
                    start = 1 + (i - 1) * size,
                    limit = size)$data
  res$journal <- tolower(res$journal)
  write.csv(res, sprintf('data/all_raw/all_raw_%s.csv', i), row.names = FALSE)
  
  Sys.sleep(10)
}

size <- 500
loops_psych <- as.integer(psych_hits / size + ifelse(psych_hits %% size > 0, 1, 0))

for (i in 12:loops_psych){
# for (i in 1:2){
  res <- searchplos(q = search_psych, 
                    fl = field_psych, 
                    start = 1 + (i - 1) * size,
                    limit = size)$data
  res$journal <- tolower(res$journal)
  write.csv(res,
            file = sprintf('data/psych_raw/psych_raw_%s.csv', i),
            row.names = FALSE)
  
  Sys.sleep(10)
}

rm(res)

