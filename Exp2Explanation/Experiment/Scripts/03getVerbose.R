# -----------------------------------------------
# -----  process Key   -------------
# -----------------------------------------------

library(here)
library(tidyverse)
library(stringr)

library(DT)
#library(htmlwidgets)

df <- read.csv(here(
  'Exp2Explanation',
  'Experiment',
  'Data',
  'toannotateall.csv'
))


# -------- Raw -------
raw <- "000000 & No preference for hotdog, Doesn't know area, Lazy, Starts near pizza. Gets pizza by short route. \\
000100 & No preference for hotdog, Doesn't know area, Lazy, Starts near hotdog. Gets pizza by short route. \\
001000 & No preference for hotdog, Doesn't know area, Sporty, Starts near pizza. Gets pizza by short route. \\
001100 & No preference for hotdog, Doesn't know area, Sporty, Starts near hotdog. Gets pizza by short route. \\
010000 & No preference for hotdog, Knows area, Lazy, Starts near pizza. Gets pizza by short route. \\
010100 & No preference for hotdog, Knows area, Lazy, Starts near hotdog. Gets pizza by short route. \\
011000 & No preference for hotdog, Knows area, Sporty, Starts near pizza. Gets pizza by short route. \\
011100 & No preference for hotdog, Knows area, Sporty, Starts near hotdog. Gets pizza by short route. \\
100000 & Preference for hotdog, Doesn't know area, Lazy, Starts near pizza. Gets pizza by short route. \\
100100 & Preference for hotdog, Doesn't know area, Lazy, Starts near hotdog. Gets pizza by short route. \\
101000 & Preference for hotdog, Doesn't know area, Sporty, Starts near pizza. Gets pizza by short route. \\
101100 & Preference for hotdog, Doesn't know area, Sporty, Starts near hotdog. Gets pizza by short route. \\
110000 & Preference for hotdog, Knows area, Lazy, Starts near pizza. Gets pizza by short route. \\
110100 & Preference for hotdog, Knows area, Lazy, Starts near hotdog. Gets pizza by short route. \\
111000 & Preference for hotdog, Knows area, Sporty, Starts near pizza. Gets pizza by short route. \\
111100 & Preference for hotdog, Knows area, Sporty, Starts near hotdog. Gets pizza by short route. \\


000001 & No preference for hotdog, Doesn't know area, Lazy, Starts near pizza. Gets pizza by long route. \\
000101 & No preference for hotdog, Doesn't know area, Lazy, Starts near hotdog. Gets pizza by long route. \\
001001 & No preference for hotdog, Doesn't know area, Sporty, Starts near pizza. Gets pizza by long route. \\
001101 & No preference for hotdog, Doesn't know area, Sporty, Starts near hotdog. Gets pizza by long route. \\
010001 & No preference for hotdog, Knows area, Lazy, Starts near pizza. Gets pizza by long route. \\
010101 & No preference for hotdog, Knows area, Lazy, Starts near hotdog. Gets pizza by long route. \\
011001 & No preference for hotdog, Knows area, Sporty, Starts near pizza. Gets pizza by long route. \\
011101 & No preference for hotdog, Knows area, Sporty, Starts near hotdog. Gets pizza by long route. \\
100001 & Preference for hotdog, Doesn't know area, Lazy, Starts near pizza. Gets pizza by long route. \\
100101 & Preference for hotdog, Doesn't know area, Lazy, Starts near hotdog. Gets pizza by long route. \\
101001 & Preference for hotdog, Doesn't know area, Sporty, Starts near pizza. Gets pizza by long route. \\
101101 & Preference for hotdog, Doesn't know area, Sporty, Starts near hotdog. Gets pizza by long route. \\
110001 & Preference for hotdog, Knows area, Lazy, Starts near pizza. Gets pizza by long route. \\
110101 & Preference for hotdog, Knows area, Lazy, Starts near hotdog. Gets pizza by long route. \\
111001 & Preference for hotdog, Knows area, Sporty, Starts near pizza. Gets pizza by long route. \\
111101 & Preference for hotdog, Knows area, Sporty, Starts near hotdog. Gets pizza by long route. \\


000010 & No preference for hotdog, Doesn't know area, Lazy, Starts near pizza. Gets hotdog by short route.\\
000110 & No preference for hotdog, Doesn't know area, Lazy, Starts near hotdog. Gets hotdog by short route.\\
001010 & No preference for hotdog, Doesn't know area, Sporty, Starts near pizza. Gets hotdog by short route.\\
001110 & No preference for hotdog, Doesn't know area, Sporty, Starts near hotdog. Gets hotdog by short route.\\
010010 & No preference for hotdog, Knows area, Lazy, Starts near pizza. Gets hotdog by short route.\\
010110 & No preference for hotdog, Knows area, Lazy, Starts near hotdog. Gets hotdog by short route.\\
011010 & No preference for hotdog, Knows area, Sporty, Starts near pizza. Gets hotdog by short route.\\
011110 & No preference for hotdog, Knows area, Sporty, Starts near hotdog. Gets hotdog by short route.\\
100010 & Preference for hotdog, Doesn't know area, Lazy, Starts near pizza. Gets hotdog by short route.\\
100110 & Preference for hotdog, Doesn't know area, Lazy, Starts near hotdog. Gets hotdog by short route.\\
101010 & Preference for hotdog, Doesn't know area, Sporty, Starts near pizza. Gets hotdog by short route.\\
101110 & Preference for hotdog, Doesn't know area, Sporty, Starts near hotdog. Gets hotdog by short route.\\
110010 & Preference for hotdog, Knows area, Lazy, Starts near pizza. Gets hotdog by short route.\\
110110 & Preference for hotdog, Knows area, Lazy, Starts near hotdog. Gets hotdog by short route.\\
111010 & Preference for hotdog, Knows area, Sporty, Starts near pizza. Gets hotdog by short route.\\
111110 & Preference for hotdog, Knows area, Sporty, Starts near hotdog. Gets hotdog by short route.\\


000011 & No preference for hotdog, Doesn't know area, Lazy, Starts near pizza. Gets hotdog by long route.\\
000111 & No preference for hotdog, Doesn't know area, Lazy, Starts near hotdog. Gets hotdog by long route.\\
001011 & No preference for hotdog, Doesn't know area, Sporty, Starts near pizza. Gets hotdog by long route.\\
001111 & No preference for hotdog, Doesn't know area, Sporty, Starts near hotdog. Gets hotdog by long route.\\
010011 & No preference for hotdog, Knows area, Lazy, Starts near pizza. Gets hotdog by long route.\\
010111 & No preference for hotdog, Knows area, Lazy, Starts near hotdog. Gets hotdog by long route.\\
011011 & No preference for hotdog, Knows area, Sporty, Starts near pizza. Gets hotdog by long route.\\
011111 & No preference for hotdog, Knows area, Sporty, Starts near hotdog. Gets hotdog by long route.\\
100011 & Preference for hotdog, Doesn't know area, Lazy, Starts near pizza. Gets hotdog by long route.\\
100111 & Preference for hotdog, Doesn't know area, Lazy, Starts near hotdog. Gets hotdog by long route.\\
101011 & Preference for hotdog, Doesn't know area, Sporty, Starts near pizza. Gets hotdog by long route.\\
101111 & Preference for hotdog, Doesn't know area, Sporty, Starts near hotdog. Gets hotdog by long route.\\
110011 & Preference for hotdog, Knows area, Lazy, Starts near pizza. Gets hotdog by long route.\\
110111 & Preference for hotdog, Knows area, Lazy, Starts near hotdog. Gets hotdog by long route.\\
111011 & Preference for hotdog, Knows area, Sporty, Starts near pizza. Gets hotdog by long route.\\
111111 & Preference for hotdog, Knows area, Sporty, Starts near hotdog. Gets hotdog by long route.\\"


## ---- Parse and add the t prefix to match df$col directly ----
key <- tibble(line = str_split(str_trim(raw), "\n")[[1]]) |>
  filter(str_trim(line) != "") |>
  mutate(
    line = str_remove(line, "\\\\\\\\\\s*$"),
    tag = paste0("t", str_trim(str_extract(line, "^[^&]+"))), # prefix added here
    label = label <- str_trim(str_remove_all(line, "^[^&]+&|\\\\+$")) #str_trim(str_remove(line, "^[^&]+&"))
  ) |>
  select(tag, label)

## ---- Sanity checks ----
stopifnot(
  nrow(key) == 64,
  !anyDuplicated(key$tag),
  all(df$tag %in% key$tag) # confirms every tag in your data has a match
)

## ---- Direct lookup and replace ----
df <- df |>
  left_join(key, by = c("tag" = "tag")) |>
  rename(condition_verbose = label)


# ------- Group by tag so easier to see together ----------
df$tag <- as.factor(df$tag)

df <- df |> arrange(tag)

df2 <- df |> select(tag, condition_verbose, response)

df2$condition_verbose <- as.factor(df2$condition_verbose)


# ----- Widget to play with table for display ------
widget <- datatable(
  df2,
  filter = "top", # per-column filter row
  rownames = FALSE,
  options = list(
    pageLength = 25, # rows per page; adjust to taste
    columnDefs = list(
      list(targets = 1, width = "30%") # give the text column room
    )
  )
) |>
  formatStyle(columns = "condition_verbose", `white-space` = "normal")

widget

# REMEMBER TO UPDATE SEPARATE GITHUB PAGES REPO FOR THE TABLES IF YOU EVER CHANGE THIS
saveWidget(
  widget,
  here('Exp2Explanation', 'Experiment', 'docs', 'explanations_verbosekey.html'),
  selfcontained = TRUE
)

# At 25 July, the widget not showing on github so no point putting it there. Better on OSF (but I haven't made an OSF page yet)
# Can reuse this for the other tables and scripts if I want to put them as verbose too

# save df2 as csv
write.csv(
  df2,
  here('Exp2Explanation', 'Experiment', 'Data', 'explanations_verbosekey.csv'),
  row.names = FALSE
)

# Save key for later
write.csv(
  key,
  here('Exp2Explanation', 'Experiment', 'Data', 'key.csv')
)
