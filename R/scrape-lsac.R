#' Scrape LSAC PDF reports and place into data frames
#'
#' This function scrapes all the pages from the LSAC PDF report.  Results
#' are placed in a series of data frames, based on the topic; and all data frames
#' are returned as a list.
#'
#' @param pdf_file String.  Full path to the PDF file to be scraped.
#'
#' @return A list, with each element in the list containing a data frame of the scraped results.
#'
#' @export
scrape_lsac_report <- function(pdf_file) {

  applicants <- text_to_list(pdf_file)

  # the combined dataframe represents all oberservations in each table

  #initialize a list that holds a seperate dataframe for each table, with each dataframe holding all student observations
  tables <- list()

  #initialize empty dataframes in the list that correspond to each table needed
  for (table_name in names(applicants[[1]])) {
    tables[[table_name]] <- tibble::tibble()
  }

  # bind each individual student dataframe to dataframe holding all observations for student
  tables <- bind_students(applicants, tables)

  # format student lists of data frames to create database tables ----------------------------

  ## identifier table

  #convert name to lower case so matching names is easier wither different student datasets
  tables$identifier$name <- tolower(tables$identifier$name)

  #seperate full names into first and last
  tables$identifier <- tables$identifier %>%
    #seperate name column at comma
    tidyr::separate(name, c('name_last_full', 'name_first_full'), sep = ', ', remove = TRUE, extra = 'drop') %>%
    #only use first word of last name
    tidyr::separate(name_last_full, c('name_last'), sep = ' ', remove = TRUE, extra = 'drop') %>%
    #seperate first name column at space to only return one word in name, and to eliminate middle initial
    tidyr::separate(name_first_full, c('name_first'), sep = ' ', remove = TRUE, extra = 'drop')


  # clean address with function
  tables$address <- clean_address(tables$address)

  ## admissions term

  #term only contains year; add month and day so it can be converted to date type
  #this allows us to calculate students' ages at the time of matriculation
  tables$admissions_term$admissions_term <- lubridate::mdy(paste('8/23/',tables$admissions_term$admissions_term, sep = ""))


  ## demographic table

  #only keep first listed ethnicity
  tables$demographic$ethnicity <- stringr::str_extract(tables$demographic$ethnicity, '[A-Z][A-Z]')

  #convert birthday to date
  tables$demographic$dob <- lubridate::mdy(gsub("\\s", "", tables$demographic$dob))


  ## undergrad_major table

  #convert majors to title case
  tables$undergrad_major$major <- stringr::str_to_title(tables$undergrad_major$major)
  #remove duplicates
  tables$undergrad_major <- dplyr::distinct(tables$undergrad_major, lsac_num, major, .keep_all = TRUE)

  # school attended
  tables$schools_attended <- table_schools_attended(tables$schools_attended)

  #the schools table is school names and school codes, which come from the schools_attended table
  #take the school code and school name from the schools_attended table and delete duplicate schools
  tables$schools <- tables$schools_attended %>%
    dplyr::select(school_code, school_name) %>%
    dplyr::distinct(school_code, .keep_all = TRUE)

  #convert school names to title case
  tables$schools$school_name <- stringr::str_to_title(tables$schools$school_name)

  #delete column that was split and school name (not needed because school_code identifies school)
  tables$schools_attended <- tables$schools_attended %>%
    dplyr::select(-school_name, -school_other)

  #delete rows without information
  tables$schools_attended <- tables$schools_attended[complete.cases(tables$schools_attended[4:5]),]

  ## school_scores table
  tables$school_scores <- table_school_scores(tables$school_scores)

  ## college_percentileGPA table
  tables$college_percentileGPA <- table_percentile_gpa(tables$college_percentileGPA)

  ## transcript table
  tables$transcript <- table_transcript(tables$transcript)

  ## grade_breakdown table

  #convert grade columns to integers
  tables$grade_breakdown[-6] <- sapply(tables$grade_breakdown[-6], as.integer)


  ## lsat table

  #filter for lsat scores that only contain the number 1
  #this removes scores that contain letters
  tables$lsat <- dplyr::filter(tables$lsat,
                               stringr::str_detect(lsat_score, '[0-9]+'))

  tables$lsat$lsat_score <- as.integer(tables$lsat$lsat_score)

  return(tables)
}
