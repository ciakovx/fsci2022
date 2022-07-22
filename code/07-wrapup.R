# load the required packages
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(readr)
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


orcid_cr_sherpa <- read_csv("./data/results/orcid_cr_sherpa.csv")


# look at the accepted versions a single person can deposit without paying a fee

orcid_ids <- orcid_cr_sherpa$orcid_identifier[1]

single_user_table <- orcid_cr_sherpa %>%
  filter(orcid_identifier %in% orcid_ids,
         oa_fee == "no",
         str_detect(article_version, "accepted"))

# which published versions can be deposited in the IR?
vor_ir <- orcid_cr_sherpa %>%
  filter(oa_fee == "no",
         str_detect(article_version, "published"),
         is.na(prerequisites),
         str_detect(location, "institutional"))
