# Install and load the required packages
install.packages(c("synthesisr"))
install.packages("revtools", dependencies = TRUE)
install.packages("bibliometrix", dependencies = TRUE)
install.packages("stringr", " >= 1.5.2") # this is needed because the stringr version in Noteable is too old
install.packages("magrittr", " >= 2.0.4")# this is needed because the magrittr version in Noteable is too old 
#if you are running it locally you may need to run the below as well
#install.packages ("dplyr")
library(revtools)
library(synthesisr)
library(dplyr)
library(bibliometrix)



#==============================Section one: Import and Merge data=============================
# Define the import folder path
folder_path <- "Data/"

# ------------------------1.1 import Ris files------------------------
## import single document
ref1 <- read_bibliography("Data/wos1000.ris")
# It fails let's explore what the problems with the data
lines <- readLines("Data/wos1000.ris", encoding = "UTF-8")
# check first 20 lines
head(lines, 20)  
# check last 20 lines and we see one empty
tail(lines, 20)  
# remove empty lines
lines <- lines[lines != ""]  

writeLines(lines, "Data/wos1000_clean.ris")
ref1 <- read_bibliography("Data/wos1000_clean.ris")
# a preview of column name and type
str(ref1)
# practice with other ris see what you find out (Due to multiple issues:1.weird characters (e.g. smart quotes, accents)2.broken lines3.encoding issues (UTF-8 vs Latin1)4.long abstract)

# practice with synthesisr
ref <- read_refs("Data/DiscoverEd.ris")
# generate lists with all ris data, have a preview of the data
files <- list.files(
  "Data",
  pattern = "\\.ris$",
  full.names = TRUE
)
refs_list <- lapply(files, read_refs)

## import multiple data and use synthesisr to solve the limitation of revtools and combine the ris data into one
# List all .ris files and import(These are the literature data download from Web of Science and DiscoverEd)
risfiles <- list.files(folder_path, pattern = "\\.ris$", full.names = TRUE)

# Read and combine all files into a list
dfris_list  <- lapply(risfiles, function(f) {
  df <- read_refs(f, return_df = TRUE)
  # convert list to character
  df[] <- lapply(df, function(col) {
    if (is.list(col)) {
      sapply(col, function(x) paste(unlist(x), collapse = "; "))
    } else {
      col
    }
  })
  # track source
  df$source_file <- basename(f)
  df
})
# View the column names under each column to see the number&name of the columns
lapply(dfris_list,colnames)
# View the column type before merge them together
lapply(dfris_list, function(df) sapply(df, class))
# Use bind_rows to combine lists as columns numbers not same (18vs19),the missing ones fill with NA
combinedris_df <- bind_rows(dfris_list)
colnames(combinedris_df)

# ------------------------1.2 import CSV files------------------------
# List all CSV files fomr folder path
csvfiles <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read and combine
dfcsv_list <- lapply(csvfiles, read.csv)
# View the column names under each column to see the number&name of the columns
lapply(dfcsv_list,colnames)
# View the column type before merge them together
lapply(dfcsv_list, function(df) sapply(df, class))
# As columns from two dataset are partly match(some overlapping names, different format and different names, we need to standardise name before combine them)
clean_names <- function(df) {
  #lowercase all column names
  names(df) <- tolower(names(df))
  #fix the source and source.title issue from list 2
  if (all(c("source.title", "source") %in% names(df))) {
    names(df)[names(df) == "source"] <- "source_file"
    names(df)[names(df) == "source.title"] <- "source"
    
  } else if ("source.title" %in% names(df)) {
    # only source.title exists
    names(df)[names(df) == "source.title"] <- "source"
  }
  #standardise known overlapping columns (as we already lowercase, any contains upercase cannot match)
  names(df)[names(df) %in% c("cites", "cited.by")] <- "citations"
  names(df)[names(df) %in% c("authors")] <- "authors"
  names(df)[names(df) %in% c("title")] <- "title"
  names(df)[names(df) %in% c("year")] <- "year"
  names(df)[names(df) %in% c("doi")] <- "doi"
  names(df)[names(df) %in% c("abstract")] <- "abstract"
  names(df)[names(df) %in% c("page.start","startpage")] <- "start_page"
  names(df)[names(df) %in% c("page.end","endpage")] <- "end_page"
  names(df)[names(df) %in% c("document.type")] <- "source_type"
  return(df)
}

# apply to all csv files
cleancsv_list <- lapply(dfcsv_list, clean_names)
# View the column names again
lapply(cleancsv_list,colnames)
# View the column format again
lapply(cleancsv_list, function(df) sapply(df, class))
combinedcsv_df <- bind_rows(cleancsv_list)
colnames(combinedcsv_df)

# ------------------------1.3 combine CSV and RIS------------------------
# view two data frame find commons and differences
colnames(combinedcsv_df)
unique(combinedcsv_df$fulltexturl)
colnames(combinedris_df)
#keep all the url columns
url_cols <- c(
  "articleurl", "citationurl", "citesurl",
  "fulltexturl", "relatedurl", "link"
)
# List columns has shared structure
common_cols <- c(
  "title", "author", "year", "source",
  "publisher", "volume", "issue",
  "start_page", "end_page",
  "doi", "issn", "abstract", "source_file",
  url_cols
)
# rename columns to match
csv_df <- combinedcsv_df
names(csv_df)[names(csv_df) == "authors"] <- "author"
ris_df <- combinedris_df
# add columns 
add_missing_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  df[missing] <- NA
  df[, cols]
}

csv_df <- add_missing_cols(csv_df, common_cols)
ris_df <- add_missing_cols(ris_df, common_cols)
combined_all <- rbind(csv_df, ris_df)
str(combined_all)
head(combined_all)

#==============================Section two: detect duplicates=============================
#---------------Finding duplicates from 6457-------------- 
##  deduplicate DOI
# Remove duplicates for rows that have a DOI
doi_unique <- combined_all %>%
  filter(!is.na(doi) & doi != "") %>%  # keep only rows with valid DOI
  distinct(doi, .keep_all = TRUE)      # keep the first occurrence of each DOI

# Keep rows without DOI (NA or empty string)
no_doi <- combined_all %>%
  filter(is.na(doi) | doi == "")

# Combine DOI-unique rows with no-DOI rows
combined_unique <- bind_rows(doi_unique, no_doi)

#check the result 5770 left
nrow(combined_unique)

## Deduplicate by title for rows without DOI (NA or empty string)
no_doi_unique <- combined_unique %>%
  filter(is.na(doi) | doi == "") %>%
  distinct(title, .keep_all = TRUE)

#Keep DOI rows as is
doi_rows <- combined_unique %>%
  filter(!is.na(doi) & doi != "")

# Combine DOI-unique rows with deduplicated no-DOI rows
combined_unique <- bind_rows(doi_rows, no_doi_unique)

# check the result 5763 left
nrow(combined_unique)

# Standardise title and author
combined_unique <- combined_unique %>%
  mutate(
    doi = ifelse(doi == "", NA, doi),
    title_clean = tolower(trimws(title)),
    author_clean = tolower(trimws(author))
  )

# Identify groups with the same title + author but multiple DOIs
possible_duplicate_doi <- combined_unique %>%
  group_by(title_clean, author_clean) %>%
  filter(n_distinct(doi) > 1) %>%   # only groups with >1 DOI
  mutate(row_number_in_group = row_number()) %>%  # order within group
  ungroup()
# Keep only first and third rows in each duplicate group
keep_rows <- possible_duplicate_doi %>%
  filter(row_number_in_group %in% c(1, 3))
# Combine with non-duplicate 5761left
non_duplicates <- combined_unique %>%
  anti_join(possible_duplicate_doi, by = c("title_clean", "author_clean"))
combined_unique <- bind_rows(non_duplicates, keep_rows) %>%
  arrange(title_clean, author_clean)

## deduplicate from title 
combined_unique$title <- tolower(trimws(combined_unique$title))
combined_final <- combined_unique %>%
  group_by(title_clean) %>%
  slice(1) %>%  # keep only the first row in each group
  ungroup()
removed_rows <- combined_unique %>%
  anti_join(combined_final, by = colnames(combined_unique))
View(removed_rows)  # to review which rows were removed 5730 left

# Write the combined data frame to a new CSV file
dir.create("Data/clean")
write.csv(combined_unique, "Data/clean/metadata.csv", row.names = FALSE)
write_refs(combined_unique, "Data/clean/metadata.ris", format = "ris")
