#' Extract text from LSAC reports
#'
#' This function extracts the raw text from the LSAC reports.
#' The extracted text is unformatted and each extracted section is
#' returned in a list.
#'
#' @param file_name String.  Full path to the PDF file to be scraped.
#' @param page_num Integer.  The page number of the PDF file to be scraped.
#'
#' @return The raw, untransformed and uncleaned, data from the LSAC report; returned
#'      as data frames stored within a list.
extracted_areas <- function(file_name, page_num) {

  #list areas to extract
  #numbers come from 'cas_Areas.R'
  #numbers represent the top, left, bottom, and right pixels in the pdf

  #header and personal information###
  areas <- list(c(30.18293,  79.02439,  42.25610, 262.53659), #name
                c(30.18293, 314.45122,  42.25610, 390.51220), #lsat number
                c(730.42683,  35.56098, 773.89024, 285.47561), #address
                c(44.67073, 411.64024,  56.74390, 542.03049 ), #term for admissions
                ###Background###
                c(99.00000, 77.81707, 113.48780, 179.23171), #dob
                c(100.2073, 254.0854, 112.2805, 297.5488), #sex
                c(117.1098, 215.4512, 129.1829, 285.4756), #ethnicity
                c(132.80488, 71.78049, 156.95122, 176.81707), #major
                c(74.4,  301.2,  154.8,  493.2 ), #school names
                ###Academic Data (transcript Analysis)###
                #note:  College field does not properly seperate, so it must be skipped
                c(284.9268, 130.3354, 303.0366, 587.9085), #year and education level
                c(311.4878, 132.7500, 398.4146, 585.4939), #grade trend information
                c(417.7317, 111.0183, 487.7561, 164.1402), #grades by letter grade breakdown
                ###Summary' section###
                c(511.90244,  89.28659, 635.04878, 142.40854), #lsat scores
                c(729.2195, 344.0305, 743.7073, 591.5305), #report date
                ###Misc###
                c(74.4,  496.2,  159.6,  593.4), #school code and graduation
                ###Cumulative GPA and hours
                c(498.7509, 404.5515, 524.2995, 455.6504), # major gpa and hour
                c(498.7509, 546.8985, 524.2995, 590.6976)) # cumulative GPA and hours

  #extract text and store in nested list

  #function is from tabulizer package and removes text in pdf from predefined areas
  #the text is placed into a nested list, with each list item being an area
  tabulizer::extract_tables(file_name, area =  areas,
                            pages = rep(page_num, length(areas)), guess = FALSE)
}

#' Convert object from `extracted_areas` to data frames
#'
#' This function takes as inputs an object created with `extracted_areas` and
#' converts the object's data to a series of dataframes.  The data frames are
#' output in a list.
#'
#' @param text_extract An object from `extracted_areas`
#'
#' @return A list with each element of the list representing a data frame with
#'      information from the scraped LSAC report.
convert_df <- function(text_extract) {

  # input is object from extracted_areas function
  # output is a series of dataframes representing tables in the database

  #initialize list to store dataframes
  single_app <- list()

  #transform extracted text into seperate data frames

  #create object for LSAT number since it will be used to identify students between tables
  lsacNum <- as.integer(gsub("L", "", text_extract[[2]][[1]]))

  ##identifier table
  single_app$identifier <- tibble::tibble(lsac_num = lsacNum,
                                          name = text_extract[[1]][[1]])
  #need to split name

  ##address table
  single_app$address <- tibble::tibble(lsac_num = lsacNum,
                                       street = text_extract[[3]][[1]],
                                       city_state_zip = text_extract[[3]][[2]])
  #need to split city_state_zip

  ##term that the studetn is seeking admissions for
  single_app$admissions_term <- tibble::tibble(lsac_num = lsacNum,
                                               admissions_term = text_extract[[4]][[2]])

  ##demographic table
  single_app$demographic <- tibble::tibble(lsac_num = lsacNum,
                                           dob = text_extract[[5]][[1]],
                                           sex = text_extract[[6]][[1]],
                                           ethnicity = text_extract[[7]][[1]])

  ##major table
  single_app$undergrad_major <- tibble::tibble(lsac_num = rep(lsacNum, length(text_extract[[8]][,1])),
                                               major = text_extract[[8]][,1])

  ##school_attended table

  #remove 'foreign educated' from schools, since this does not have a code or level
  schools <- stringr::str_subset(text_extract[[9]][,1], "^((?!FOREIGN).)*$")

  #remove blanks ('') from school leves because these are there when the school is foreign educated
  other <- text_extract[[15]][,1][text_extract[[15]][,1] != ""]

  single_app$schools_attended <- tibble::tibble(lsac_num = rep(lsacNum, length(schools)),
                                                school_name = schools,
                                                school_other = other)

  ##transcript table, school_scores table, and college_percentileGPA table
  #the fields in the transcript table are in two different extracted areas
  #these two areas are transposed so that each year is a row, and combined as columns

  #remove rows that just say 'see foreign eval'
  grades <- tibble::as_tibble(t(text_extract[[11]])) %>%
    dplyr::filter_all(dplyr::all_vars(. != "FOREIGN"))

  trans <- dplyr::bind_cols(tibble::as_tibble(t(text_extract[[10]])), grades)

  #only create the data frame from the transcript section if there is transcript information
  #if there is only 1 row in trans there is no transcript information
  if (nrow(trans) >= 2) {

    trans <- trans[c(1,2,3,4,5,6,7,9,10)]
    # #create column names that match the database field names
    # #this also allows us to use discriptive column names in filtering, so dplyr can be used
    colnames(trans) <- c('school_year', 'school_level', 'school_code',
                         'lsat_mean', 'lsat_takers',
                         'year_hours', 'year_gpa',
                         'percentileGPA', 'gpa_mean')


    # pull out school scores and college percentile gpa
    transcript <- create_transcript_section(single_app, trans)

    single_app$school_scores <- transcript$school_scores
    single_app$college_percentileGPA <- transcript$college_percentileGPA

    #add lsacNum to table only if the table has values (only if there is a college percentile GPA)
    if (nrow(single_app$college_percentileGPA) > 0) {
      single_app$college_percentileGPA$lsac_num <- lsacNum
    }

    ###convert data types

    ##transcript table
    single_app$transcript <- trans[c('school_year', 'school_code', 'school_level', 'year_hours', 'year_gpa')]

    single_app$transcript <- varFilter(single_app$transcript, c('year_hours', 'year_gpa'))

    single_app$transcript$lsac_num <- lsacNum
    ##convert data types

    ##grade_breakdown table
    single_app$grade_breakdown <- clean_grade_breakdown(text_extract[[12]][1:5], lsacNum)

  }

  # clean lsat scores
  single_app$lsat <- clean_lsat(text_extract[[13]], lsacNum)

  # cumulative GPA table
  single_app$cum_gpa <- tibble::tibble(lsac_num = lsacNum,
                                       degree_gpa = as.numeric(text_extract[[16]][[1]]),
                                       degree_hours = as.numeric(text_extract[[16]][[2]]),
                                       cum_gpa = as.numeric(text_extract[[17]][[1]]),
                                       cum_hours = as.numeric(text_extract[[17]][[2]]))

  return(single_app)

}

#' Filter out rows in the transcript with no values
#'
#' @param df Data frame to filter rows
#' @param columns Columns to filter
varFilter <- function(df, columns) {

  df %>%
    dplyr::filter_at(dplyr::vars(columns),
                     # these strings represent rows without values
                     dplyr::any_vars(. != "" &
                                    . != "INSF" &
                                    . != "SEE" &
                                    . != "TRANS" &
                                    . != "UNACK" &
                                    . != "FOREIGN" &
                                    . != "EVAL"))
}

#' Extract text areas and place in individual dataframes
#'
#' This function extracts and cleans the data and places it into
#' data frames for individual students.  Each student's data will be placed
#' in a separate data frame.
#'
#' @param pdf_file String.  Full path to the PDF file to be scraped.
#' @param start_page Integer. The page number of the first PDF page that needs to be scraped.
#'
#' @return A nested list with each student as the highest hierarchy in the list, and
#'      each student is another list with a different data frame for each table.
text_to_list <- function(pdf_file, start_page) {

  ###########################################################################
  # each student will have a seperate dataframe for each individual table
  # the dataframes for each student will be placed in a single nested list
  # one level is the student and the next level is a dataframe of the specific table
  ############################################################################

  # return an error if start page is not an integer
  if (!start_page%%1==0) {
    stop("start_page msut be an integer")
  }

  # get the number of pages in the pdf file
  num_pages <- tabulizer::get_n_pages(pdf_file)

  # make a vector of the pages to scrape,
  # by starting at the first page to scrape and ending at the final page
  scrape_pages <- seq(start_page, num_pages)

  #initialize dataframe to hold lsat number and report creation date
  #this will be used to see if an applicant has a later report already extracted
  app_list <- tibble::tibble(lsat = NA,
                             report_date = lubridate::mdy("01/01/2000"))

  #initialize nested list that will store lists of data frames for each applicant
  applicants <- list()

  for (pg in scrape_pages) {

    extracted <- extracted_areas(pdf_file, pg)

    #extract lsat number and remove 'L' so it can be stored as int
    lsatNumber <- as.integer(gsub("L", "", extracted[[2]][[1]]))

    #extract the report creation date, remove whitespace, convert to date-time
    report_dateSingle <- lubridate::mdy(gsub("\\s", "", extracted[[14]][1]))

    #will return records of lsat number and date of report that is earlier in time to current report
    #will not have any rows if there are no earlier records
    dups <- dplyr::filter(app_list,
                          lsat == lsatNumber & report_date > report_dateSingle)

    #add the current applicant to the dataframe of applicants and report dates
    singleApp <- tibble::tibble(lsat = lsatNumber,
                                report_date = report_dateSingle)
    app_list <- dplyr::bind_rows(app_list, singleApp)

    #only create data frames from extracted areas if the current applicant summary is the applicant's latest
    if (nrow(dups) == 0) {
      #if extracted area in region identified below is "" then it contains no school or grade information
      #only pull data for students that have grade information
      if (extracted[[10]][1,1] != "") {
        #convert areas into data frames for applicant
        #and place in list where list name is lsat number
        #this will write over any other applicant already read
        #place in try statement, so if code fails, program continues
        tryCatch({

          # try converting extracted text to data frames
          applicants[[paste('L', lsatNumber, sep='')]] <- convert_df(extracted)
        },

        # if this causes an error, print an error message and write to error log
        error=function(cond) {
          message(stringr::str_c("Page ", pg, " error"))
          readr::write_lines(str_c("Page ", pg, " error"),
                             'error_log.txt',
                             sep = "\n", append = TRUE)
        })
      }
    }
    print(stringr::str_c("Page ", pg, " complete"))
  }
  return(applicants)
}


