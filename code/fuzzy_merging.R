# install stringdist
install.packages('fuzzyjoin')

# load other packages
library(dplyr)
library(fuzzyjoin)

# create sample tibbles
df1 <- tibble(name = c("Kara Page", 
                       "Uthman Quraishi",
                       "Bao Chiu",
                       "Ursula Jager",
                       "Goran Reisner"),
              id = 1:5)
df2 <- tibble(name = c("Kara J. Page",
                       "U. Quraishi",
                       "Bai Chiu",
                       "Bai Chi",
                       "Ursula Jager",
                       "Blasius Bezrukov"),
              id = letters[1:6])

df_merge <- df1 %>%
  stringdist_left_join(df2, by = "name",
                       max_dist = 4,
                       ignore_case = TRUE,
                       distance_col = "strdistance")
