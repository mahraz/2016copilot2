# Load dependencies
if(!require(rplos)){install.packages('rplos')}
if(!require(statcheck)){install.packages('statcheck')}
if(!require(stringr)){install.packages('stringr')}

# function to make first letter of each word uppercase
simpleCap <- function(x) {
  res <- NULL
  for (i in 1:length(x)){
    s <- strsplit(x[i], " ")[[1]]
    
    res[i] <- paste(toupper(substring(s, 1,1)), substring(s, 2),
                    sep="", collapse=" ")
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
search_psych <- '(doc_type:full) AND (article_type:"Research Article") AND (subject:psychology) AND (cross_published_journal_key:PLoSONE)'

# Get number of hits
all_hits <- searchplos(q = search_all, fl = field_all, limit = 1)$meta$numFound
psych_hits <- searchplos(q = search_psych, fl = field_psych, limit = 1)$meta$numFound

# Collect results for entire PLOS family ----------------------------------

# for (iter in 1:all_hits){
for (iter in 1:5){
  # Get information
  res <- searchplos(q = search_all, 
                    fl = field_all, 
                    start = iter,
                    limit = 1)$data  
  # Ensure continuity of how dois are depicted
  res$id <- gsub('/', '_', res$id)
  
  # Retain only year of publication_date
  res$publication_date <- substr(res$publication_date, 0, 4)
  if (!is.null(res_statcheck)) res_statcheck$publication_date <- res$publication_date
  
  # First author
  res$first_author <- str_split(res$author, '; ')[[1]][1]
  res$first_author_initials <- paste(
    unlist(
      str_extract_all(
        simpleCap(res$first_author),
        '[A-Z]')),
    collapse = '')
  if(!is.null(res_statcheck)) res_statcheck$first_author <- res$first_author
  
  # Get all author initials
  temp <- str_extract_all(simpleCap(str_split(res$author, '; ')[[1]]), '[A-Z]')
  for (i in 1:length(temp)){
    temp[[i]] <- paste(unlist(temp[[i]], '[A-Z]'), collapse = '')
  }
  abbreviated_author <- unlist(temp)
  
  # Author count
  res$author_count <- dim(str_match_all(res$author, ';')[[1]])[1] + 1
  
  # Competing interest?
  # use ! to make false indicate NO competing interest
  res$competing_interest_boolean <- !grepl(res$competing_interest,
                                           pattern = 'no competing interest')
  if (!is.null(res_statcheck)) res_statcheck$competing_interest_boolean <- res$competing_interest_boolean
  
  # Split up contributions
  contributions <- str_split(res$author_notes, '\\. ')[[1]]
  
  # Extract "Conceived and Designed the experiments"
  id_conceived <- grepl('conceived', contributions, ignore.case = TRUE)
  conceived <- str_split(
    str_split(
      contributions[id_conceived], ': ')[[1]][2], ' ')[[1]]
  conceived <- gsub(conceived, pattern = '\\.', replacement = '')
  res$nr_conceived <- length(conceived)
  
  # Extract 'Performed the experiments'
  id_performed <- grepl('performed', contributions, ignore.case = TRUE)
  performed <- str_split(
    str_split(
      contributions[id_performed], ': ')[[1]][2], ' ')[[1]]
  performed <- gsub(performed, pattern = '\\.', replacement = '')
  res$nr_performed <- length(performed)
  
  # Extract 'Analyzed the data'
  id_analyzed <- grepl('analyzed', contributions, ignore.case = TRUE)
  analyzed <- str_split(
    str_split(
      contributions[id_analyzed], ': ')[[1]][2], ' ')[[1]]
  analyzed <- gsub(analyzed, pattern = '\\.', replacement = '')
  res$nr_analyzed <- length(analyzed)
  if (!is.null(res_statcheck)) res_statcheck$nr_analyzed <- length(analyzed)
  
  # Extract 'Wrote the paper'
  id_wrote <- grepl('wrote', contributions, ignore.case = TRUE)
  wrote <- str_split(
    str_split(
      contributions[id_wrote], ': ')[[1]][2], ' ')[[1]]
  wrote <- gsub(wrote, pattern = '\\.', replacement = '')
  res$nr_wrote <- length(wrote)
  
  author <- NULL
  
  for (j in 1:length(abbreviated_author)){
    step_1 <- sum(c(abbreviated_author[j] %in% conceived,
                    abbreviated_author[j] %in% performed,
                    abbreviated_author[j] %in% analyzed,
                    abbreviated_author[j] %in% wrote))
    if (step_1 > 0) {
      author[j] <- TRUE
    } else {
      temp <- strsplit(conceived, '')
      conc <- 0
      for (z in 1:length(temp)){
        conc <- conc + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                             abbreviated_author[j])
      }
      
      temp <- strsplit(performed, '')
      perf <- 0
      for (z in 1:length(temp)){
        perf <- perf + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                             abbreviated_author[j])
      }
      
      temp <- strsplit(analyzed, '')
      anal <- 0
      for (z in 1:length(temp)){
        anal <- anal + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                             abbreviated_author[j])
      }
      
      temp <- strsplit(wrote, '')
      wrot <- 0
      for (z in 1:length(temp)){
        wrot <- wrot + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                             abbreviated_author[j])
      }
      
      if ((conc + perf + anal + wrot) > 0) author[j] <- TRUE
    }
  }
  
  res$first_author_present <- author[1]
  res$ghost_authorship <- ifelse(sum(author == FALSE) > 0, TRUE, FALSE)
  temp <- paste(grep('FALSE', author), collapse = ';') 
  res$ghost_authorship_position <- ifelse(temp == '', NA, temp)
  
  # Generate filename
  filename_meta <- sprintf('data/all_meta/%s',
                           gsub('/',
                                '_',
                                res$id))
  
  # Save the file (without names of the columns!)
  write.table(x = res,
              col.names = FALSE,
              sep = ',',
              file = filename_meta,
              row.names = FALSE)  
}

# save the column names for the data later
write.table(names(res),
            file = 'data/names_all.csv',
            sep = ',',
            row.names = FALSE,
            col.names = FALSE)


# Collect psychology results ----------------------------------------------

# for (iter in 1:psych_hits){
for (iter in 1:5){
  # Ensure res_statcheck is cleaned
  res_statcheck <- NULL
  
  # Get information
  res <- searchplos(q = search_psych,
                    fl = field_psych,
                    start = iter,
                    limit = 1)$data  
  
  # Ensure continuity of how dois are depicted
  res$id <- gsub('/', '_', res$id)
  
  full_text <- res$everything
  res <- res[, 1:(dim(res)[2] - 1)]
  
  # Run statcheck on fulltext
  res_statcheck <- tryCatch(statcheck(full_text), error = function(e) NULL)
  
  if (!is.null(res_statcheck)){
    # Ensure source is the doi
    res_statcheck$Source <- gsub('/', '_', res$id)
  }
  
  # Add meta information about statcheck results
  res$nr_pval <- ifelse(is.null(res_statcheck),
                        NA,
                        dim(res_statcheck)[1])
  res$nr_error <- ifelse(is.null(res_statcheck),
                         NA,
                         sum(res_statcheck$Error))
  res$nr_gross_error <- ifelse(is.null(res_statcheck),
                               NA,
                               sum(res_statcheck$DecisionError))
  
  # Retain only year of publication_date
  res$publication_date <- substr(res$publication_date, 0, 4)
  if (!is.null(res_statcheck)) res_statcheck$publication_date <- res$publication_date
  
  # First author
  res$first_author <- str_split(res$author, '; ')[[1]][1]
  res$first_author_initials <- paste(
    unlist(
      str_extract_all(
        simpleCap(res$first_author),
        '[A-Z]')),
    collapse = '')
  if(!is.null(res_statcheck)) res_statcheck$first_author <- res$first_author
  
  # Get all author initials
  temp <- str_extract_all(simpleCap(str_split(res$author, '; ')[[1]]), '[A-Z]')
  for (i in 1:length(temp)){
    temp[[i]] <- paste(unlist(temp[[i]], '[A-Z]'), collapse = '')
  }
  abbreviated_author <- unlist(temp)
  
  # Author count
  res$author_count <- dim(str_match_all(res$author, ';')[[1]])[1] + 1
  
  # Competing interest?
  # use ! to make false indicate NO competing interest
  res$competing_interest_boolean <- !grepl(res$competing_interest,
                                           pattern = 'no competing interest')
  if (!is.null(res_statcheck)) res_statcheck$competing_interest_boolean <- res$competing_interest_boolean
  
  # Split up contributions
  contributions <- str_split(res$author_notes, '\\. ')[[1]]
  
  # Extract "Conceived and Designed the experiments"
  id_conceived <- grepl('conceived', contributions, ignore.case = TRUE)
  conceived <- str_split(
    str_split(
      contributions[id_conceived], ': ')[[1]][2], ' ')[[1]]
  conceived <- gsub(conceived, pattern = '\\.', replacement = '')
  res$nr_conceived <- length(conceived)
  
  # Extract 'Performed the experiments'
  id_performed <- grepl('performed', contributions, ignore.case = TRUE)
  performed <- str_split(
    str_split(
      contributions[id_performed], ': ')[[1]][2], ' ')[[1]]
  performed <- gsub(performed, pattern = '\\.', replacement = '')
  res$nr_performed <- length(performed)
  
  # Extract 'Analyzed the data'
  id_analyzed <- grepl('analyzed', contributions, ignore.case = TRUE)
  analyzed <- str_split(
    str_split(
      contributions[id_analyzed], ': ')[[1]][2], ' ')[[1]]
  analyzed <- gsub(analyzed, pattern = '\\.', replacement = '')
  res$nr_analyzed <- length(analyzed)
  if (!is.null(res_statcheck)) res_statcheck$nr_analyzed <- length(analyzed)
  
  # Extract 'Wrote the paper'
  id_wrote <- grepl('wrote', contributions, ignore.case = TRUE)
  wrote <- str_split(
    str_split(
      contributions[id_wrote], ': ')[[1]][2], ' ')[[1]]
  wrote <- gsub(wrote, pattern = '\\.', replacement = '')
  res$nr_wrote <- length(wrote)
  
  author <- NULL
  
  for (j in 1:length(abbreviated_author)){
    step_1 <- sum(c(abbreviated_author[j] %in% conceived,
                    abbreviated_author[j] %in% performed,
                    abbreviated_author[j] %in% analyzed,
                    abbreviated_author[j] %in% wrote))
    if (step_1 > 0) {
      author[j] <- TRUE
    } else {
      temp <- strsplit(conceived, '')
      conc <- 0
      for (z in 1:length(temp)){
        conc <- conc + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                             abbreviated_author[j])
      }
      
      temp <- strsplit(performed, '')
      perf <- 0
      for (z in 1:length(temp)){
        perf <- perf + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                             abbreviated_author[j])
      }
      
      temp <- strsplit(analyzed, '')
      anal <- 0
      for (z in 1:length(temp)){
        anal <- anal + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                             abbreviated_author[j])
      }
      
      temp <- strsplit(wrote, '')
      wrot <- 0
      for (z in 1:length(temp)){
        wrot <- wrot + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                             abbreviated_author[j])
      }
      
      if ((conc + perf + anal + wrot) > 0) author[j] <- TRUE
    }
  }
  
  res$first_author_present <- author[1]
  res$ghost_authorship <- ifelse(sum(author == FALSE) > 0, TRUE, FALSE)
  temp <- paste(grep('FALSE', author), collapse = ';') 
  res$ghost_authorship_position <- ifelse(temp == '', NA, temp)
  
  # Generate filenames
  filename_meta <- sprintf('data/psych_meta/%s', gsub('/', '_', res$id))
  filename_statcheck <- sprintf('data/psych_statcheck/%s', gsub('/', '_', res$id))
  
  # Save the files (without names of the columns!)
  ## Meta information
  write.table(x = res,
              col.names = FALSE,
              sep = ',',
              file = filename_meta,
              row.names = FALSE)  
  ## Statcheck
  if(!is.null(res_statcheck)){
    write.table(x = res_statcheck,
                col.names = FALSE,
                sep = ',',
                file = filename_statcheck,
                row.names = FALSE)
  }
}

write.table(names(res),
            file = 'data/names_psych.csv',
            sep = ',',
            row.names = FALSE,
            col.names = FALSE)

write.table(names(res_statcheck),
            file = 'data/names_statcheck.csv',
            sep = ',',
            row.names = FALSE,
            col.names = FALSE)