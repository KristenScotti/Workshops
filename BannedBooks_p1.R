# ======================================================================
# BANNED BOOKS — TEXT ANALYSIS IN R
# CONTACT: Kristen Scotti (kscotti@andrew.cmu.edu)
# ======================================================================



# Clean restart
rm(list = ls())          # Clear everything in memory
cat("\014")              # Clear console
.rs.restartR()           # (RStudio only) force restart if not done already

# ======================================================================
# 0) SETUP — PACKAGES
# ======================================================================

install.packages(c(
  "tidyverse","tidytext",
  "stringr","textdata","SnowballC",
  "janitor","topicmodels"
))

library(tidyverse)
library(tidytext)
library(stringr)
library(textdata)
library(SnowballC)
library(janitor)
library(topicmodels)

# ======================================================================
# 1) LOAD DATA 
# ======================================================================

raw_url <- "https://raw.githubusercontent.com/KristenScotti/Workshops/main/BannedBooks_Texts.csv"

df <- readr::read_csv(raw_url, show_col_types = FALSE) %>%
  clean_names()

glimpse(df)   # prints column names + types
colnames(df)  # prints just the names

# --------------------------
# 1A) SIMPLE EXPLORATION
# --------------------------

df %>%
  summarise(
    n_books   = n_distinct(book_title),
    n_authors = n_distinct(author)
  )

df %>%
  count(author, sort = TRUE) %>%
  filter(n > 1)

authors_multiple <- df %>%
  count(author, sort = TRUE) %>%
  filter(n > 1)

print(authors_multiple)

df %>%
  arrange(desc(bans_pen)) %>%
  select(book_title, author, year, bans_pen) %>%
  slice_head(n = 10)

df %>%
  dplyr::filter(book_title == "A Clockwork Orange") %>%
  dplyr::pull(text_summary)

# --------------------------
# 1B) STATES WITH HIGHEST BANS
# --------------------------

# STEP 1: SELECT AND CLEAN RELEVANT COLUMNS:
states_tidy <- df %>%
  select(book_title, bans_states) %>%                 # keep only relevant columns
  mutate(bans_states = coalesce(bans_states, "")) %>% # replace NA with blank
  
  # STEP 2: SPLIT MULTIPLE STATES INTO SEPARATE ROWS
  tidyr::separate_rows(bans_states, sep = "\\s*,\\s*|\\s*;\\s*") %>%
  
  # STEP 3: EXTRACT STATE AND COUNT INFO FROM TEXT
  mutate(
    state = stringr::str_trim(stringr::str_remove(bans_states, "\\(.*\\)$")),
    count = stringr::str_extract(bans_states, "\\d+") %>% as.integer()
  ) %>%
  
  # STEP 4: FILTER OUT BLANKS AND MISSING NUMBERS 
  filter(!is.na(count), state != "")

# STEP 5: SUMMARIZE TOTALS BY STATE
states_rank <- states_tidy %>%
  group_by(state) %>%
  summarise(total_bans = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_bans))

print(states_rank)

# STEP 6: VISUALIZE THE TOP 15 STATES
states_rank %>%
  slice_max(total_bans, n = 15, with_ties = FALSE) %>%     # keep top 15 states
  ggplot(aes(x = reorder(state, total_bans), y = total_bans)) +
  geom_col(fill = "steelblue") +                            # blue bars
  coord_flip() +                                            # horizontal orientation
  labs(
    title = "States with the Highest Reported Book Bans",
    x = NULL,
    y = "Total Bans Across Listed Books"
  ) +
  theme_minimal()

# --------------------------
# 1C) BAN STATUSES
# --------------------------

# STEP 1: Select and clean the source column
status_tidy <- df %>%
  select(book_title, bans_status) %>%
  mutate(bans_status = coalesce(bans_status, "")) %>%
  
  # STEP 2: Split multiple statuses into separate rows
  tidyr::separate_rows(bans_status, sep = "\\s*,\\s*|\\s*;\\s*") %>%
  
  # STEP 3: Extract the label ('status') and the numeric 'count'
  mutate(
    status = stringr::str_trim(stringr::str_remove(bans_status, "\\(.*\\)$")),
    count  = stringr::str_extract(bans_status, "\\d+") %>% as.integer()
  ) %>%
  
  # STEP 4: Keep only valid rows
  filter(status != "", !is.na(count))

# STEP 5: Summarize totals by status
status_summary <- status_tidy %>%
  group_by(status) %>%
  summarise(total = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total))

# View the tidy summary table in the console
print(status_summary)

# STEP 6: Visualize
status_summary %>%
  ggplot(aes(x = reorder(status, total), y = total)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Frequency of Ban Statuses",
    x = NULL,
    y = "Total count reported"
  ) +
  theme_minimal()


