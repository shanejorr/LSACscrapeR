#################################################################
#
# These are a series of auxilary functions that clean the tables
#
#################################################################

bind_students <- function(applicants, tables) {

  ###################################################
  #this nested for loop iterates through each student
  #then iterates through each dataframe for the student
  #and binds the individual student dataframe
  #to a dataframe holding all students observations for that table
  ###################################################

  #iterate through each applicant
  for (persons in 1:length(applicants)) {
    #iterate through each dataframe within each applicant
    for(item in 1:length(applicants[[persons]])) {
      #turn the individual data frame for the student into its own object
      item_table <- applicants[[persons]][item]
      #name the dataframe within the list holding all student dataframes the same as the name of the
      #dataframe in the in the list of dataframes by student
      #they need the same name so that the student dataframes are automatically matched
      #with the dataframes containing all observations
      table_name <- names(item_table)
      #bind the individual student dataframe to the dataframe containing all observations
      tables[[table_name]] <- dplyr::bind_rows(tables[[table_name]], item_table)
    }
  }

  return(tables)

}

create_transcript_section <- function(single_app, trans) {

  # This function formats the grades box in the transcript section

  ##school_scores table
  scores <- trans[c('school_code', 'school_year', 'lsat_mean', 'lsat_takers', 'gpa_mean')]

  scores <- varFilter(scores, c('lsat_mean', 'gpa_mean'))
  ##convert data types

  ##college_percentileGPA table
  #only use last percentileGPA score
  college_percentileGPA <- trans[c('school_code', 'school_year', 'percentileGPA')]

  college_percentileGPA <- varFilter(college_percentileGPA, "percentileGPA")

  transcript <- list(school_scores = scores,
                     college_percentileGPA = college_percentileGPA)

  return(transcript)
}

clean_address <- function(address_table) {

  # This function cleans the address

  ### address table

  #covnert city_state_zip column into three seperate columns

  #regular expressoin that isolates city, state, and zip
  addy <- regex("(.*), ([A-Z][A-Z]) ([0-9]+)")

  #extract city, state, and zip, and place into seperate columns in matrix
  addy_split <- stringr::str_match(address_table$city_state_zip, addy)

  #title case city
  addy_split[,2] <- stringr::str_to_title(addy_split[,2])

  #store matrix columns of city, state, and zip into address dataframe
  address_table$city <- addy_split[,2]
  address_table$state <- addy_split[,3]
  address_table$zip <- addy_split[,4]

  #drop the combined city_state_zip column
  address_table <- dplyr::select(address_table, -city_state_zip)

  return(address_table)
}

clean_grade_breakdown <- function(text_extract_grades, lsacNum) {

  # clean the grade breakdowns by letter grade

  #first 5 values are letter grades, so they are the only ones needed
  grade_table <- t(tibble::tibble(text_extract_grades))
  colnames(grade_table) <- c('a_grade', 'b_grade', 'c_grade', 'd_grade', 'f_grade')
  grade_table <- tibble::as_tibble(grade_table)
  grade_table$lsac_num <- lsacNum

  return(grade_table)

}

clean_lsat <- function(text_extract_lsat, lsacNum) {

  # clean lsat scores

  ##lsat table
  lsatFull <- tibble::as_tibble(text_extract_lsat)
  colnames(lsatFull) <- c('lsat_score')

  #'=====' reflects that lsat was taken multiple times
  #in such a case, the final score is the average, and is not needed
  #if row starts with '=' then that row and the next (average) are removed
  if (any(startsWith(lsatFull$lsat_score, "="))) {
    #identify the row number of the row that starts with '='
    num <- which(startsWith(lsatFull$lsat_score, "="))
    #only keep the rows that are less than the row number that starts with '='
    #the row after the row that starts with '=' is the average and is not needed
    lsat_results <- tibble::as_tibble(lsatFull[1:num-1,])
  } else {
    lsat_results <- lsatFull
  }
  lsat_results$lsac_num <- lsacNum

  return(lsat_results)
}

table_schools_attended <- function(schools_attended_table) {


  ## schools_attended table and creation of schools table

  # split column that contains degree acheived, date of degree, level, and school code

  #degree acheived and date of degree
  schools_graduate <- stringr::str_match(schools_attended_table$school_other,
                                         '([A-Z][A-Z]) ([0-9][0-9][/][0-9][0-9])')
  schools_attended_table$degree <- schools_graduate[,2]
  schools_attended_table$date_degree <- schools_graduate[,3]

  #the date_degree column only contains month and year;
  #add a day so it can be converted to date data type and then age at graduation can be counted
  #next line does three things: #1) adds day ('20/') to month and year (paste); 2) NA's show as '20/NA' so convert these back to NA (gsub), and
  # convert to dmy (dmy)
  schools_attended_table$date_degree <- lubridate::dmy(gsub('20/NA', NA,
                                                            stringr::str_c('20/', schools_attended_table$date_degree)))

  #level of schooling and school code
  schools_code <- stringr::str_match(schools_attended_table$school_other, '([A-Z]) ([0-9]{4})')
  schools_attended_table$school_code <- as.integer(schools_code[,3])
  schools_attended_table$level_degree <- schools_code[,2]

  return(schools_attended_table)
}

table_percentile_gpa <- function(percent_gpa_table) { # tables$college_percentileGPA

  #for school year, only use last year
  percent_gpa_table <- percent_gpa_table %>%
    #school_year column contains start and end years (ex. 16-17)
    #split into two columns, one for start year and another for end year
    tidyr::separate(school_year, into = c('year_start', 'year_end'), sep = '-')

  #for percentileGPA column, remove percentage sign and turn into decimal
  percent_gpa_table$percentileGPA <- stringr::str_extract(percent_gpa_table$percentileGPA, '[0-9]+')
  percent_gpa_table$percentileGPA <- round(as.numeric(percent_gpa_table$percentileGPA) * 0.01, 2)

  percent_gpa_table[c(1,2, 3)] <-  sapply(percent_gpa_table[c(1,2, 3)], as.integer)

  return(percent_gpa_table)

}

table_school_scores <- function(scores_table) {

  # create table for school scores

  scores_table <- scores_table %>%
    #school_year column contains start and end years (ex. 16-17)
    #split into two columns, one for start year and another for end year
    tidyr::separate(school_year, into = c('year_start', 'year_end'), sep = '-') %>%
    #only keep observations with distinct school_code, year_start, and year_end values
    dplyr::distinct(school_code, year_start, year_end, .keep_all = TRUE)

  scores_table[-6] <- sapply(scores_table[-6], as.integer)
  scores_table$gpa_mean <- as.numeric(scores_table$gpa_mean)

  return(scores_table)
}

table_transcript <- function(transcript_table) {

  # create table for transcripts lsit

  transcript_table <- transcript_table %>%
    tidyr::separate(school_year, c('year_start', 'year_end'), sep= '-')

  transcript_table[c(1,2,3,5)] <- sapply(transcript_table[c(1,2,3,5)], as.integer)
  transcript_table$year_gpa <- as.numeric(transcript_table$year_gpa)

  return(transcript_table)
}
