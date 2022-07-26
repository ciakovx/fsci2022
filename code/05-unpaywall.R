# load the required packages
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(httr)
library(forcats)
library(usethis)
library(anytime)
library(janitor)
library(glue)
library(rorcid)
library(rcrossref)
library(roadoi)
library(inops)


# remove all objects from the environment
# to start with a clean slate
rm(list = ls())


# read in the crossref/orcid merge data
orcid_cr_merge <- read_csv("./data/results/orcid_cr_merge.csv",
                           col_types = cols(.default = "c"))

# input your email address to send with your unpaywall api requests
my_email <- "TYPE YOUR EMAIL ADDRESS HERE"

# create a slow oadoi_fetch call to be used during this class
slow_oadoi_fetch <- slowly(oadoi_fetch, rate_delay(2))

###################################################
## When you run this on your own after the class,##
############### REMOVE THE [1:20] #################
###################################################

# loop through the dois, calling oadoi_fetch and returning the results
dois_oa <- map(orcid_cr_merge$doi[1:20], function(z) {
  print(z)
  o <- slow_oadoi_fetch(dois = z, email = my_email)
  return(o)
})

# write the json, if necessary
# write_json(dois_oa, "./data/processed/dois_oa.json")

# read in the json if necessary
# dois_oa2 <- read_json("./data/processed/dois_oa.json", simplifyVector = TRUE)


# view warnings to see information on any items that weren't found in the unpaywall database
warnings()

# loop (map) through the results
# is_empty will return a TRUE or FALSE if there were no results, and the _lgl
# part of map will return that TRUE or FALSE into a single vector, which can be used 
# to subset the crossref/orcid merge

#################################################################
### DELETE THE [1:20, ] WHEN YOU ARE RUNNING THIS AFTER CLASS ###
#################################################################

dois_not_found <- orcid_cr_merge[1:20, ] %>%
  filter(map_lgl(dois_oa, is_empty))

# loop through (map) the returned results, extract (flatten) the 
# data frame, and bind (_dfr) the rows together
dois_oa_df <- dois_oa %>%
  map_dfr(., flatten) %>%
  clean_names() 

# View the data
View(dois_oa_df)

# have a look at the column names
View(as.data.frame(names(dois_oa_df)))

# have a look at how many of the results have an open access version available
tabyl(dois_oa_df$is_oa)

# filter to create a new data frame of the open access objects
oa_only <- dois_oa_df %>%
  filter(is_oa == TRUE)

# of these, have a look at the best location
best_oa <- oa_only %>%
  tidyr::unnest(best_oa_location)

# because you can't write nested lists to CSV, you must either unnest them or remove them. In this case, we remove them.
# But if you really want to explore this data, you'll want to unnest them
# here in RStudio
best_oa_merge <- best_oa %>%
  filter(!duplicated(doi)) %>%
  select_if(purrr::negate(is.list))

# now that we have the best OA location, we can merge this back to our ORCID/Crossref file
orcid_cr_oa_merge <- orcid_cr_merge %>%
  left_join(best_oa_merge, by = "doi", suffix = c("_cr", "_oa")) %>%
  rename(title = title_cr)

# write the csv
write_csv(orcid_cr_oa_merge, "./data/results/orcid_cr_oa_merge.csv")  


# exploring the data ------------------------------------------------------

#plot number of OA vs non-OA
dois_oa_df %>%
  ggplot(., aes(x = is_oa)) +
  geom_histogram(stat = "count") 

# create color palette
oa_colors <- c("bronze" = "#D55E00",
               "closed" = "#000000",
               "gold" = "#F0E442",
               "green" = "#009E73", 
               "hybrid" = "#E69F00")

# plot OA status histogram
dois_oa_df %>%
  ggplot(., aes(x = oa_status)) +
  geom_histogram(stat = "count", fill = oa_colors)


