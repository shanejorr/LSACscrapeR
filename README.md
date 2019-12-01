# Scrape LSAC admissions files

R package that scrapes single page LSAC CAS Summar Report PDF files and inserts the results into a list containing multiple R data frames.

### Installation

The packagage can be installed from GitHub:

```r
devtools::install_github('shanejorr/LSACscrapeR')
```
### Usage

The package has one function, `scrape_lsac_report()`.  This function returns a list, with each element of the list being a data frame corresponding to an element of information from the LSAC CAS summary report.

Users can then export this list of data frames to the medium of their choosing.  For example, the following code blocks convert the data frames to a series of csv files and to an in memory SQLite database.

#### Converting data frames to csv files

This section is common to the csv and SQLite sections.

```r
# common code for both csv and SQLite sections

# file name to single file that contains all PDF CAS summary reports for 2018
file_name <- "cas_report_2018.pdf"

#extract information from pdf files into a list of data frames
apps <- scrape_lsac_report(file_name)
```

Now, we'll create separate csv fiules for each data frame.
```r
# iterate through every data frame, outputting as csv file
walk(names(apps),
     function(x) {
        write_csv(apps[[x]], 
                 path = str_c(x, ".csv"))
      })
```

#### Converting data frames to in memory SQLite data base

```r
# create database connection and database file
con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
# iterate through each table, adding to database
walk(names(apps),
      function(x) {
        dbWriteTable(con, value = apps[[x]], 
                     name = x, row.names = FALSE, 
                     overwrite = FALSE, append = TRUE)
      })
      
dbDisconnect(con)
```

Fields:

* Document Type: CAS
* Assembly Output Format: PDF
* Eapps or CAS Reports first?: CAS Reports
* Bookmark Order Settings: Standard
* JD/CAS Doc Type: Report Summary
