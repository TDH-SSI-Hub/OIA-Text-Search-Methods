# Author: Kate Durst
# Contact info: kate.durst@tn.gov

# Project: Text Search Methods for Identifying Drug Overdose Deaths
# Date Created: 3/11/23
# Date Updated: 

# Purpose of this file: Quantify overdose deaths by drug category using
#                       text search over ICD-10 codes

#-----------------------------------------------------
# Notes: Literal text search method, drug terms was created before and stored
 #       in a separate excel file

# this search looks at the broad drug categories, however can be repeated to look
# for specific substances

#-----------------------------------------------------

###### Step 1: Set Up Environment #######
library(RODBC)
library(tidyverse)
library(lubridate)
library(dplyr)
library(anytime)
library(openxlsx)


###### Step 2: Load initial data #######

## TDH-OIA accesses death certificates using SQL server
#Connect to SQL
sql_server_name <- odbcConnect("Name of SQL Server")

death_certificate_data <- sqlQuery(sql_server_name, 
                                  "select [Unique ID for Counting],
                                  [Cause of Death Text Field 1],
                                  [Cause of Death Text Field 2],
                                  [Cause of Death Text Field 3],
                                  [Cause of Death Text Field 4],
                                  [Decendents Resident State],
                                  [Date of Death]
                                  from Table.Name", as.is= TRUE)

###### Step 3: Filter and Clean Data #######

#Filters table to only include deaths occurring in TN
#TN counts based on residence, not location of death
death_certificate_data <- death_certificate_data %>%
  filter(`Decendents Resident State`== 'TN')


#Filters table to specific period of time
#Create a start date
start_date <- paste0("01-01-2019")
start_date <- anydate(start_date)

#create an end date
end_date <- paste0("12-31-2021")
end_date <- anydate(end_date)

#Filters table to only include data between start and end date
death_certificate_data <- death_certificate_data %>%
  filter(DateOfDeath >= start_date & DateOfDeath <= end_date)

##Create a single text column for searching and clean text to remove special characters
# TDH puts these into a new table instead of replacing existing
cases <- death_certificate_data %>%
  # replaces null/empty fields with actually nothing, not the words null or a space
  mutate(`Cause of Death Text Field 1` = str_squish(replace_na(`Cause of Death Text Field 1`, "")),
         `Cause of Death Text Field 2` = str_squish(replace_na(`Cause of Death Text Field 2`, "")),
         `Cause of Death Text Field 3` = str_squish(replace_na(`Cause of Death Text Field 3`, "")),
         `Cause of Death Text Field 4` = str_squish(replace_na(`Cause of Death Text Field 4`, ""))) %>%
  #combines multiple fields of text into one field but joins with a space so that words do not get accidentally joined together and therefore unsearchable
  mutate(grand_terms = tolower((paste0(`Cause of Death Text Field 1`," ", `Cause of Death Text Field 2`," ",`Cause of Death Text Field 3`," ", `Cause of Death Text Field 4`))),
         #cleans of backslash
         grand_terms = str_replace(grand_terms, "/", " "),
         # replace commas with spaces 
         grand_terms = str_replace_all(grand_terms, ",", " "),
         # remove all punctuation
         grand_terms = gsub("[[:punct:]]+", " ",grand_terms),
         # replace commas with spaces 
         nocomma_terms = str_replace_all(grand_terms, ",", " "),
         # remove all punctuation
         nopunct_terms = gsub("[[:punct:]]+", " ",nocomma_terms)) %>%
  filter(!is.na(grand_terms)) # removes cases with no text at all

grandterms <- cases %>%
  select(`Unique ID for Counting`, grand_terms)

###### Step 4: Load key terms for text search #######

#Pull in lists of key terms and clean them
#some words are not easily searchable because they were too broad or too specific so they were removed and kept in the excel columns C and D
regex_words <- read_excel("list_of_key_terms.xlsx", sheet= "Sheet1" )%>%
  mutate(cleanterms = tolower(DC_terms), # makes all terms lowercase
         clean_detail = gsub("[[:punct:]]", "", tolower(DC_terms_detailed)),
         fentanyl_terms = gsub("[[:punct:]]", "", tolower(Fentanyl_search)),
         cocaine_terms = gsub("[[:punct:]]", "", tolower(Cocaine_Search)),
         heroin_terms = gsub("[[:punct:]]", "", tolower(Heroin_Search)),
         opioid_terms = gsub("[[:punct:]]", "", tolower(Opioid_Search)),
         psychostim_terms = gsub("[[:punct:]]", "", tolower(Psychostimulants_search))) # removes punctuation


## To ease analysis, I create a vectors for all the categories
# this step also removes NAs which is caused when the list is transformed from a dataframe
# these categories include common misspellings

#Vector of broad terms that will help us identify if the death was an overdose (i.e. poisoning, overdose, toxic)
broad_words <- regex_words$cleanterms[!is.na(regex_words$cleanterms)]

#Vector of specific terms that will help us identify if drugs were involved (i.e. fentanyl, meth, tramadol)
specific_words <- regex_words$clean_detail[!is.na(regex_words$clean_detail)]

## Vector of specific terms that will help us identify if fentanyl was involved
fentanyl_words <- regex_words$fentanyl_terms[!is.na(regex_words$fentanyl_terms)]

## Vector of specific terms that will help us identify if cocaine was involved
cocaine_words <- regex_words$cocaine_terms[!is.na(regex_words$cocaine_terms)]

## Vector of specific terms that will help us identify if heroin was involved
heroin_words <- regex_words$heroin_terms[!is.na(regex_words$heroin_terms)]

## Vector of specific terms that will help us identify if any opioid was involved
opioid_words <- regex_words$opioid_terms[!is.na(regex_words$opioid_terms)]

## Vector of specific terms that will help us identify if psychostimulants were involved
psychostim_words <- regex_words$psychostim_terms[!is.na(regex_words$psychostim_terms)]

###### Step 5: Run Text Search #######

#this method first searches death certificate text for words that indicate an overdose may have occurred

# Adds column that counts the number of broad terms detected
cases$broad_term <- 0

# Adds column that puts the broad term detected
cases$the_term <- ""

# searches for each broad word in the text field using a loop
for(b in broad_words){
  # if a word is found, flags "broad_term" with a 1
  cases$broad_term[str_detect(cases$grand_terms, b)] <- 1
  # appends all terms to a list
  cases$the_term[str_detect(cases$grand_terms, b)] <-  paste0(cases$the_term[str_detect(cases$grand_terms, b)],"/", b)
}



# The second step for this method searches for a specific drug named in the overdose
# Adds column that counts the number of specific terms detected
cases$specific_term <- 0

# Adds column that puts the specific term detected
cases$all_drugs <- ""

# Searches for each specific drug in the text field
# This loop is more complex, because we are looking for perfect, not partial, word matches
# It splits the DC text into individual words, and then checks to see if each word matches a drug in the list of drugs

# loops over every case
for(i in 1:nrow(cases)){
  #pulls out the words for the case
  words <- cases$nopunct_terms[i]
  # splits the words on spaces
  word_v <- str_split(words, " ")[[1]]
  drugs <- ""
  # loops over every drug
  for(b in specific_words){
    # for every word (w) in the list (word_v), this looks for the drug (b)
    for(w in word_v){
      found <- w==b
      # if the drug is found, spec2 is increased and the drug is appended to "drugs"
      if(found){
        cases$specific_term[i] <- cases$specific_term[i] + 1
        if(drugs !=""){
          drugs <- paste0(drugs, "/", b)
          #cases$all_drugs[i] <- paste0(cases$alldrugs[i], "/", b)
        } else if(drugs==""){
          drugs <- b
          #cases$all_drugs[i] <- paste0(b)
        }
      }
    }
  }
  cases$all_drugs[i] <- drugs
}

# counts all cases with at least one specific term
sum(cases$specific_term>0)

###### Step 6: Run Text Search for specific categories of drugs #######

### Fentanyl Search

#Create a search for fentanyl, adding columns to help count number of terms and terms identified
#Not all identified terms will be counted later as this method first identifies terms and later applies logic to exclude cases where drugs may be involved but the death was not due to an overdose
cases$is_fentanyl <- 0
cases$fentanyl_term <- ""

# loops over every case
for(i in 1:nrow(cases)){
  #pulls out the words for the case
  words <- cases$nopunct_terms[i]
  # splits the words on spaces
  word_v <- str_split(words, " ")[[1]]
  drugs <- ""
  # loops over every drug
  for(b in fentanyl_words){
    # for every word (w) in the list (word_v), this looks for the drug (b)
    for(w in word_v){
      found <- w==b
      # if the drug is found, spec2 is increased and the drug is appended to "drugs"
      if(found){
        cases$is_fentanyl[i] <- cases$is_fentanyl[i] + 1
        if(drugs !=""){
          drugs <- paste0(drugs, "/", b)
          #cases$all_drugs[i] <- paste0(cases$alldrugs[i], "/", b)
        } else if(drugs==""){
          drugs <- b
          #cases$all_drugs[i] <- paste0(b)
        }
      }
    }
  }
  cases$fentanyl_term[i] <- drugs
}

### Cocaine Search

#Create a search for cocaine, adding columns to help count number of terms and terms identified
#Not all identified terms will be counted later as this method first identifies terms and later applies logic to exclude cases where drugs may be involved but the death was not due to an overdose
cases$is_cocaine <- 0
cases$cocaine_term <- ""

# loops over every case
for(i in 1:nrow(cases)){
  #pulls out the words for the case
  words <- cases$nopunct_terms[i]
  # splits the words on spaces
  word_v <- str_split(words, " ")[[1]]
  drugs <- ""
  # loops over every drug
  for(b in cocaine_words){
    # for every word (w) in the list (word_v), this looks for the drug (b)
    for(w in word_v){
      found <- w==b
      # if the drug is found, spec2 is increased and the drug is appended to "drugs"
      if(found){
        cases$is_cocaine[i] <- cases$is_cocaine[i] + 1
        if(drugs !=""){
          drugs <- paste0(drugs, "/", b)
          #cases$all_drugs[i] <- paste0(cases$alldrugs[i], "/", b)
        } else if(drugs==""){
          drugs <- b
          #cases$all_drugs[i] <- paste0(b)
        }
      }
    }
  }
  cases$cocaine_term[i] <- drugs
}


### Heroin Search

#Create a search for heroin, adding columns to help count number of terms and terms identified
#Not all identified terms will be counted later as this method first identifies terms and later applies logic to exclude cases where drugs may be involved but the death was not due to an overdose

cases$is_heroin <- 0
cases$heroin_term <- ""

# loops over every case
for(i in 1:nrow(cases)){
  #pulls out the words for the case
  words <- cases$nopunct_terms[i]
  # splits the words on spaces
  word_v <- str_split(words, " ")[[1]]
  drugs <- ""
  # loops over every drug
  for(b in heroin_words){
    # for every word (w) in the list (word_v), this looks for the drug (b)
    for(w in word_v){
      found <- w==b
      # if the drug is found, spec2 is increased and the drug is appended to "drugs"
      if(found){
        cases$is_heroin[i] <- cases$is_heroin[i] + 1
        if(drugs !=""){
          drugs <- paste0(drugs, "/", b)
          #cases$all_drugs[i] <- paste0(cases$alldrugs[i], "/", b)
        } else if(drugs==""){
          drugs <- b
          #cases$all_drugs[i] <- paste0(b)
        }
      }
    }
  }
  cases$heroin_term[i] <- drugs
}

### Any Opioid Search

#Create a search for any opioid, adding columns to help count number of terms and terms identified
#Not all identified terms will be counted later as this method first identifies terms and later applies logic to exclude cases where drugs may be involved but the death was not due to an overdose
cases$is_opioid <- 0
cases$opioid_term <- ""


# loops over every case
for(i in 1:nrow(cases)){
  #pulls out the words for the case
  words <- cases$nopunct_terms[i]
  # splits the words on spaces
  word_v <- str_split(words, " ")[[1]]
  drugs <- ""
  # loops over every drug
  for(b in opioid_words){
    # for every word (w) in the list (word_v), this looks for the drug (b)
    for(w in word_v){
      found <- w==b
      # if the drug is found, spec2 is increased and the drug is appended to "drugs"
      if(found){
        cases$is_opioid[i] <- cases$is_opioid[i] + 1
        if(drugs !=""){
          drugs <- paste0(drugs, "/", b)
          #cases$all_drugs[i] <- paste0(cases$alldrugs[i], "/", b)
        } else if(drugs==""){
          drugs <- b
          #cases$all_drugs[i] <- paste0(b)
        }
      }
    }
  }
  cases$opioid_term[i] <- drugs
}

### Pyschostimulant Search

#Create a search for psychostimulant, adding columns to help count number of terms and terms identified
#Not all identified terms will be counted later as this method first identifies terms and later applies logic to exclude cases where drugs may be involved but the death was not due to an overdose
cases$is_psychostim <- 0
cases$psychostim_term <- ""

# loops over every case
for(i in 1:nrow(cases)){
  #pulls out the words for the case
  words <- cases$nopunct_terms[i]
  # splits the words on spaces
  word_v <- str_split(words, " ")[[1]]
  drugs <- ""
  # loops over every drug
  for(b in psychostim_words){
    # for every word (w) in the list (word_v), this looks for the drug (b)
    for(w in word_v){
      found <- w==b
      # if the drug is found, spec2 is increased and the drug is appended to "drugs"
      if(found){
        cases$is_psychostim[i] <- cases$is_psychostim[i] + 1
        if(drugs !=""){
          drugs <- paste0(drugs, "/", b)
          #cases$all_drugs[i] <- paste0(cases$alldrugs[i], "/", b)
        } else if(drugs==""){
          drugs <- b
          #cases$all_drugs[i] <- paste0(b)
        }
      }
    }
  }
  cases$psychostim_term[i] <- drugs
}

###### Step 7: Apply Logic to Table #######

# Filters out cases without at least 1 broad term and one drug term
# if a case only has polysubstance abuse mentioned, it is also filtered out
# Also concerns where the broad term multidrug and specific term drug may be the only words found 
elig_cases <- cases %>%
  filter(broad_term==1,
         specific_term>0,
         !(the_term=="/multi drug" & all_drugs=="drug"),
         !(the_term=="/multidrug" & all_drugs=="drug"),
         !(all_drugs=="polysubstance" & str_detect(nopunct_terms, "polysubstance abuse"))) %>%
  select(`Unique ID for Counting`, DateOfDeath, specific_term, broad_term, 
         the_term, all_drugs, grand_terms, opioid_term, is_opioid, fentanyl_term, is_fentanyl, cocaine_term, is_cocaine, heroin_term, is_heroin, is_psychostim, psychostim_term)

# Creates a better list of broad terms
for(i in 1:nrow(elig_cases)){
  d <- unlist(strsplit(elig_cases$the_term[i], split = "/"))
  elig_cases$all_broad_terms[i] <- paste(unique(d), collapse = "/")
}


###### Step 8: Creates tables based on categories #######

all_drug <- elig_cases %>%
  filter(broad_term== 1 & specific_term > 0)

any_opioid <- elig_cases %>%
  filter(broad_term== 1 & specific_term > 0 & is_opioid>= 1)

fentanyl <- elig_cases %>%
  filter(broad_term== 1 & specific_term > 0 & is_fentanyl>= 1)

heroin <- elig_cases %>%
  filter(broad_term== 1 & specific_term > 0 & is_heroin>= 1)

cocaine <- elig_cases %.%
  filter(broad_term== 1 & specific_term > 0 & is_cocaine>= 1)

psychostimulants <- elig_cases %>%
  filter(broad_term== 1 & specific_term > 0 & is_psychostim>= 1)

###### Step 9: Count Total Number of Overdoses Per Drug Category #######

all_drug_count <- all_drug %>%
  count(`Unique ID for Counting`) %>%
  rename(Number_ODs = n) %>%
  mutate(Drug_Category == paste0("All Drug"))

any_opioid_count <- any_opioid %>%
  count(`Unique ID for Counting`) %>%
  rename(Number_ODs = n) %>%
  mutate(Drug_Category == paste0("Any Opioid"))

fentanyl_count <- fentanyl %>%
  count(`Unique ID for Counting`) %>%
  rename(Number_ODs = n) %>%
  mutate(Drug_Category == paste0("Fentanyl"))

heroin_count <- heroin %>%
  count(`Unique ID for Counting`) %>%
  rename(Number_ODs = n) %>%
  mutate(Drug_Category == paste0("Heroin"))

cocaine_count <- cocaine %>%
  count(`Unique ID for Counting`) %>%
  rename(Number_ODs = n) %>%
  mutate(Drug_Category == paste0("Cocaine"))

psychostimulants_count <- psychostimulants %>%
  count(`Unique ID for Counting`) %>%
  rename(Number_ODs = n) %>%
  mutate(Drug_Category == paste0("Pscyhostimulants"))

###### Step 10: Combine Tables to make a final Master Table #######

final_table <-rbind(all_drug_count,any_opioid_count,fentanyl_count,heroin_count,cocaine_count,psychostimulants_count)

###### Step 11: Save and Output Results to a Preferred Location #######
#TDH OIA saves as .Rdata file to be easily picked up by a tableau dashboard

save(final_table, file= "A:/Drive Folder/fatal_data_with_text_search.RData")
