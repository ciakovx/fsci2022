library(rorcid)
library(httr)
library(usethis)
library(anytime)
library(lubridate)
library(janitor)
library(glue)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(jsonlite)
library(ggplot2)
library(forcats)
library(rcrossref)
library(roadoi)
library(inops)
library(fulltext)

# read in the crossref/orcid merge data
orcid_cr_merge <- read_csv("./data/results/orcid_cr_merge.csv",
                           col_types = cols(.default = "c"))

# input your email address to send with your unpaywall api requests
my_email <- "TYPE YOUR EMAIL ADDRESS HERE"

# loop through the dois, calling oadoi_fetch and returning the results
dois_oa <- map(orcid_cr_merge$doi, function(z) {
  print(z)
  o <- roadoi::oadoi_fetch(dois = z, email = my_email)
  return(o)
  Sys.sleep(0.5)
})

write_json(dois_oa, "./data/processed/dois_oa.json")

# read in the json if necessary
# dois_oa2 <- read_json("./data/processed/dois_oa.json", simplifyVector = TRUE)


# view warnings to see information on any items that weren't found in the unpaywall database
warnings()

# loop (map) through the results
# is_empty will return a TRUE or FALSE if there were no results, and the _lgl
# part of map will return that TRUE or FALSE into a single vector, which can be used 
# to subset the crossref/orcid merge
dois_not_found <- orcid_cr_merge[map_lgl(dois_oa, is_empty), ]

# loop through (map) the returned results, extract (flatten) the 
# data frame, and bind (_dfr) the rows together
dois_oa_df <- dois_oa %>%
  map_dfr(., flatten) %>%
  clean_names() 



View(as.data.frame(names(dois_oa_df)))

# have a look at how many of the results have an open access version available
tabyl(dois_oa_df$is_oa)

# filter to create a new data frame of the open access objects
oa_only <- dois_oa_df %>%
  filter(is_oa == TRUE)

best_oa <- oa_only %>%
  tidyr::unnest(best_oa_location)

# because you can't write nested lists to CSV, you must either unnest them or remove them.
# We join this 


best_oa_merge <- best_oa %>%
  filter(!duplicated(doi)) %>%
  select_if(purrr::negate(is.list))

orcid_cr_oa_merge <- orcid_cr_merge %>%
  left_join(best_oa_merge, by = "doi", suffix = c("_cr", "_oa")) %>%
  rename(title = title_cr)

write_csv(orcid_cr_oa_merge, "./data/results/orcid_cr_oa_merge.csv")  





