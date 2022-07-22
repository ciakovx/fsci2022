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

# read in the crossref/orcid/unpaywall data and create a new 
# variable filename that is the first five characters of the author's last name
# and the first five characters of the title
orcid_cr <- read_csv("./data/results/orcid_cr_merge.csv") %>%
  mutate(filename = make_clean_names(paste(str_sub(family_name, 1, 5), str_sub(title, 1, 5), sep = "_")))




# wrap GET in safely in order to keep a failed search from terminating the loop
safeslowget <- slowly(safely(GET), rate_delay(2))

# create a new directory called minedpdfs, if one doesn't already exist
if(!dir.exists("minedpdfs")) {dir.create("./minedpdfs")}

# it's best to do this on campus since most of them will be paywalled
# even ones you have subscriptions to won't necessarily get picked up
# if the file size is 10KB or less, it didn't work
# see also the fulltext package in R
my_pdfs <- orcid_cr %>%
  filter(!is.na(pdf_url)) %>%
  pmap(function(...) {
    current <- tibble(...)
    print(current$pdf_url)
    n <- safeslowget(current$pdf_url,
                     add_headers(`Accept` = "application/pdf",
                                 `Content-Type` = "application/pdf",
                                 `X-Content-Type-Options` = "nosniff"),
                     write_disk(paste0("./minedpdfs/", current$filename, ".pdf")))
    return(n)
  })