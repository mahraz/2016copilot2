# Load dependencies
if(!require(rplos)){install.packages('rplos')}

# Printing sessioninfo for run
sessionInfo()

# set fields to download
field_all <- c('id', # DOI
               'journal', # All journals
               'author',
               'author_affiliate',
               'author_notes', # Contributions
               'subject')

field_psych <- c('id', # DOI
                      'journal', # Only PLOS ONE if all went well
                      'author',
                      'author_affiliate',
                      'author_notes', # Contributions
                      'subject',
                      'everything')

# set searchterms
search_all <- '(doc_type:full) AND (article_type:"Research Article")'
search_psych <- '(doc_type:full) AND (article_type:"Research Article") AND (subject:psychology) AND (cross_published_journal_key:PLoSONE)'

# Search and write out
all_hits <- searchplos(q = search_all, fl = field_all, limit = 1)$meta$numFound
all_res <- searchplos(q = search_all, fl = field_all, limit = 20)$data
# all_res <- searchplos(q = search_all, fl = field_all, limit = all_hits)

psych_hits <- searchplos(q = search_psych, fl = field_psych, limit = 1)$meta$numFound
psych_res <- searchplos(q = search_psych, fl = field_psych, limit = 20)$data
# psych_res <- searchplos(q = search_psych, fl = field_psych, limit = psych_hits)

write.csv(all_res, quote = TRUE, 
          file = sprintf('data/%s_all_res.csv', format(Sys.time(),
                                                      '%Y%m%d_%H%M')),
          row.names = FALSE,
          col.names = FALSE)

write.csv(psych_res, quote = TRUE,
          file = sprintf('data/%s_psych_res.csv', format(Sys.time(),
                                                        '%Y%m%d_%H%M')),
          row.names = FALSE,
          col.names = FALSE)