# Collect results for entire PLOS family ----------------------------------

# for (file_nr in 1:loops_psych){
for (file_nr in 1:2){
  res_raw <- read.csv(sprintf('data/all_raw/all_raw_%s.csv', file_nr))
  
  for (iter in 1:dim(res_raw)[1]){
    # Get information
    res <- res_raw[iter, ]
    # Ensure continuity of how dois are depicted
    res$id <- gsub('/', '_', res$id)
    
    # Retain only year of publication_date
    res$publication_year <- substr(res$publication_date, 0, 4)
    res$publication_month <- substr(res$publication_date, 6, 7)
    
    # First author
    res$first_author <- str_split(res$author, '; ')[[1]][1]
    res$first_author_initials <- paste(
      unlist(
        str_extract_all(
          simpleCap(res$first_author),
          '[ÆØÅA-Z]')),
      collapse = '')
    
    # Get all author initials
    temp <- str_extract_all(simpleCap(str_split(res$author, '; ')[[1]]), '[ÆØÅA-Z]')
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
    
    # Split up contributions
    contributions <- str_split(res$author_notes, '\\.')[[1]]
    contributions <- contributions[as.vector(!is.na(str_match(contributions, ': ')))]
    
    # Get initials for all contributions in a vector
    separate <- unlist(str_split(contributions, ': '))
    if (!length(separate) > 1){
      index <- 1
    } else {
      index <- seq(2, length(separate), 2)
    }
    all_init <- separate[index]
    all_init <- gsub(x = all_init, pattern = '\\.', replacement = '')
    uniq_initials <- unique(unlist(str_split(all_init, pattern = ' ')))
    
    if(is.null(separate) | length(uniq_initials) == 0){
      
      res$nr_conceived <- NA
      res$nr_performed <- NA
      res$nr_analyzed <- NA
      res$nr_wrote <- NA
      res$first_author_present <- NA
      res$first_author_analyze <- NA
      res$ghost_authorship <- NA
      res$nr_ghost_authors <- NA
      res$ghost_authorship_position <- NA
      
    } else {
      
      # Extract "Conceived and Designed the experiments"
      id_conceived <- grepl('conceived', contributions, ignore.case = TRUE)
      if (sum(id_conceived) == 0){
        res$nr_conceived <- NA
      } else {
        conceived <- str_split(
          str_split(
            contributions[id_conceived], ': ')[[1]][2], ' ')[[1]]
        conceived <- gsub(conceived, pattern = '\\.', replacement = '')
        
        # Ensure that double initials are checked and counted only once
        # E.g., Shujun Wang and Sujin Wang
        temp <- conceived[grepl(conceived, pattern = '[A-Z]{1}[a-z]{1,}')]
        temp_conc <- NULL
        
        if (!length(temp) == 0){
          for (q in 1:(length(temp) / 2)){
            from <- ifelse(q == 1, q, q + 1)
            to <- ifelse(q == 1, 2, seq(2, length(temp), 2)[q])
            temp_conc <- paste(temp[from:to], collapse = ' ')  
            
            sel <- grepl(conceived, pattern = '[A-Z]{1}[a-z]{1,}')
            conceived[sel][from:to] <- temp_conc
          }
        }
        
        res$nr_conceived <- length(unique(conceived))
      }
      
      # Extract 'Performed the experiments'
      id_performed <- grepl('performed', contributions, ignore.case = TRUE)
      if (sum(id_performed) == 0){
        res$nr_performed <- NA
      } else {
        performed <- str_split(
          str_split(
            contributions[id_performed], ': ')[[1]][2], ' ')[[1]]
        performed <- gsub(performed, pattern = '\\.', replacement = '')
        
        # Ensure that double initials are checked and counted only once
        # E.g., Shujun Wang and Sujin Wang
        temp <- performed[grepl(performed, pattern = '[A-Z]{1}[a-z]{1,}')]
        temp_perf <- NULL
        
        if (!length(temp) == 0){
          for (q in 1:(length(temp) / 2)){
            from <- ifelse(q == 1, q, q + 1)
            to <- ifelse(q == 1, 2, seq(2, length(temp), 2)[q])
            temp_perf <- paste(temp[from:to], collapse = ' ')  
            
            sel <- grepl(performed, pattern = '[A-Z]{1}[a-z]{1,}')
            performed[sel][from:to] <- temp_perf
          }
          
          res$nr_performed <- length(unique(performed))
        }
      }
      
      
      # Extract 'analyzed the data'
      id_analyzed <- grepl('analyzed', contributions, ignore.case = TRUE)
      if (sum(id_analyzed) == 0){
        res$nr_analyzed <- NA
      } else {
        analyzed <- str_split(
          str_split(
            contributions[id_analyzed], ': ')[[1]][2], ' ')[[1]]
        analyzed <- gsub(analyzed, pattern = '\\.', replacement = '')
        
        # Ensure that double initials are checked and counted only once
        # E.g., Shujun Wang and Sujin Wang
        temp <- analyzed[grepl(analyzed, pattern = '[A-Z]{1}[a-z]{1,}')]
        temp_anal <- NULL
        
        if (!length(temp) == 0){
          for (q in 1:(length(temp) / 2)){
            from <- ifelse(q == 1, q, q + 1)
            to <- ifelse(q == 1, 2, seq(2, length(temp), 2)[q])
            temp_anal <- paste(temp[from:to], collapse = ' ')  
            
            sel <- grepl(analyzed, pattern = '[A-Z]{1}[a-z]{1,}')
            analyzed[sel][from:to] <- temp_anal
          }
        }
        res$nr_analyzed <- length(unique(analyzed))
      }
      
      # Extract 'Wrote the paper'
      id_wrote <- grepl('wrote', contributions, ignore.case = TRUE)
      if (sum(id_wrote) == 0){
        res$nr_wrote <- NA
      } else {
        wrote <- str_split(
          str_split(
            contributions[id_wrote], ': ')[[1]][2], ' ')[[1]]
        wrote <- gsub(wrote, pattern = '\\.', replacement = '')
        
        # Ensure that double initials are checked and counted only once
        # E.g., Shujun Wang and Sujin Wang
        temp <- wrote[grepl(wrote, pattern = '[A-Z]{1}[a-z]{1,}')]
        temp_wrot <- NULL
        
        if (!length(temp) == 0){
          for (q in 1:(length(temp) / 2)){
            from <- ifelse(q == 1, q, q + 1)
            to <- ifelse(q == 1, 2, seq(2, length(temp), 2)[q])
            temp_wrot <- paste(temp[from:to], collapse = ' ')  
            
            sel <- grepl(wrote, pattern = '[A-Z]{1}[a-z]{1,}')
            wrote[sel][from:to] <- temp_wrot
          }
        }
        res$nr_wrote <- length(unique(wrote))
      }
      
      author_present <- NULL
      author_conceived <- NULL
      author_performed <- NULL
      author_analyzed <- NULL
      author_wrote <- NULL
      
      for (j in 1:length(abbreviated_author)){
        
        # Present at all
        temp <- strsplit(uniq_initials, '')
        at_all <- 0
        
        # In case the initials in contributions are < char than author
        for (z in 1:length(temp)){
          at_all <- at_all + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                                   abbreviated_author[j])
        }
        
        temp <- strsplit(abbreviated_author[j], '')
        # In case the initials in contributions are < char than author
        regex_patt <- sprintf('^(%s).*%s(%s)$',
                              temp[[1]][1],
                              ifelse(length(temp[[1]]) > 2,
                                     paste0(temp[[1]][2:(length(temp[[1]]) - 1)],
                                            '.*',
                                            collapse = ''),
                                     ''),
                              tail(temp[[1]], 1))
        at_all <- at_all + sum(grepl(pattern = regex_patt,
                                     uniq_initials))
        
        # Present in conceived
        temp <- strsplit(conceived, '')
        conc <- 0
        
        # In case the initials in contributions are < char than author
        for (z in 1:length(temp)){
          conc <- conc + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                               abbreviated_author[j])
        }
        
        temp <- strsplit(abbreviated_author[j], '')
        # In case the initials in contributions are < char than author
        regex_patt <- sprintf('^(%s).*%s(%s)$',
                              temp[[1]][1],
                              ifelse(length(temp[[1]]) > 2,
                                     paste0(temp[[1]][2:(length(temp[[1]]) - 1)],
                                            '.*',
                                            collapse = ''),
                                     ''),
                              tail(temp[[1]], 1))
        conc <- conc + sum(grepl(pattern = regex_patt,
                                 conceived))
        
        # Present in performed
        temp <- strsplit(performed, '')
        perf <- 0
        
        # In case the initials in contributions are < char than author
        for (z in 1:length(temp)){
          perf <- perf + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                               abbreviated_author[j])
        }
        
        temp <- strsplit(abbreviated_author[j], '')
        # In case the initials in contributions are < char than author
        regex_patt <- sprintf('^(%s).*%s(%s)$',
                              temp[[1]][1],
                              ifelse(length(temp[[1]]) > 2,
                                     paste0(temp[[1]][2:(length(temp[[1]]) - 1)],
                                            '.*',
                                            collapse = ''),
                                     ''),
                              tail(temp[[1]], 1))
        perf <- perf + sum(grepl(pattern = regex_patt,
                                 performed))
        
        # Present in analyzed
        temp <- strsplit(analyzed, '')
        anal <- 0
        
        # In case the initials in contributions are < char than author
        for (z in 1:length(temp)){
          anal <- anal + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                               abbreviated_author[j])
        }
        
        temp <- strsplit(abbreviated_author[j], '')
        # In case the initials in contributions are < char than author
        regex_patt <- sprintf('^(%s).*%s(%s)$',
                              temp[[1]][1],
                              ifelse(length(temp[[1]]) > 2,
                                     paste0(temp[[1]][2:(length(temp[[1]]) - 1)],
                                            '.*',
                                            collapse = ''),
                                     ''),
                              tail(temp[[1]], 1))
        anal <- anal + sum(grepl(pattern = regex_patt,
                                 analyzed))
        
        
        # Present in wrote
        temp <- strsplit(wrote, '')
        wrot <- 0
        
        # In case the initials in contributions are < char than author
        for (z in 1:length(temp)){
          wrot <- wrot + grepl(pattern = paste0(temp[[z]], '.*', collapse = ''),
                               abbreviated_author[j])
        }
        
        temp <- strsplit(abbreviated_author[j], '')
        # In case the initials in contributions are < char than author
        regex_patt <- sprintf('^(%s).*%s(%s)$',
                              temp[[1]][1],
                              ifelse(length(temp[[1]]) > 2,
                                     paste0(temp[[1]][2:(length(temp[[1]]) - 1)],
                                            '.*',
                                            collapse = ''),
                                     ''),
                              tail(temp[[1]], 1))
        wrot <- wrot + sum(grepl(pattern = regex_patt,
                                 wrote))
        
        author_present[j] <- at_all
        author_conceived[j] <- conc
        author_performed[j] <- perf
        author_analyzed[j] <- anal
        author_wrote[j] <- wrot
      }
      
    }
    
    if (is.na(res$nr_conceived) &
        is.na(res$nr_performed) & 
        is.na(res$nr_analyzed) &
        is.na(res$nr_wrote)){
      res$first_author_present <- NA
      res$first_author_analyze <- NA
      res$ghost_authorship <- NA
      res$nr_ghost_authors <- NA
      res$ghost_authorship_position <- NA
    } else {
      res$first_author_present <- ifelse(author_present[1] > 0, TRUE, FALSE)
      res$first_author_analyze <- ifelse(author_analyzed[1] > 0, TRUE, FALSE)
      res$ghost_authorship <- ifelse(sum(author_present == 0) > 0, TRUE, FALSE)
      res$nr_ghost_authors <- sum(author_present == 0)
      temp <- paste0(which(author_present == 0), '', collapse = ';')
      res$ghost_authorship_position <- ifelse(temp == '',
                                              NA,
                                              temp)
    }
    
    
    check <- NULL
    check[1] <- ifelse(res$first_author_present == FALSE & res$first_author_analyze == FALSE,
                       TRUE, FALSE)
    check[2] <- ifelse(res$nr_ghost_authors == res$author_count,
                       TRUE, FALSE)
    check[3] <- ifelse(res$ghost_authorship_position == res$author_count,
                       TRUE, FALSE)
    check[4] <- ifelse(res$nr_ghost_authors > 1,
                       TRUE, FALSE)
    if (sum(check, na.rm = TRUE) > 0) res$check <- TRUE else res$check <- FALSE
    
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
}
# save the column names for the data later
write.table(t(as.matrix(names(res))),
            file = 'data/names_all.csv',
            sep = ',',
            row.names = FALSE,
            col.names = FALSE)


