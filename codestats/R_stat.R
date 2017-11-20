library(tidyverse)
library(tidytext)
library(stringr)

# purpose
# 1. get pipe counts, "<-" counts
# 2. used packages
# 3. metrics: number of comments, number of lines, number of chars, number of words, number of functions

get_stat_r <- function(file) {
  # init
  filename <- basename(file)
  
  # raw
  x1 <- readLines(file, warn = FALSE)
  x1 <- data_frame(filename, file, id = 1:length(x1), txt = x1)
  x1 <- x1 %>%
    mutate(is_fill = ifelse(grepl("^\\s*$", txt), 0, 1),
           is_comment = ifelse(grepl("^(\\s |)#", txt), 1, 0),
           cnt_char = str_count(txt, "."),
           cnt_char_real = str_count(txt, "[^\\s]"),
           cnt_assign = str_count(txt, "<-"),
           cnt_assign_rev = str_count(txt, "->"),
           cnt_pipe = str_count(txt, "%>%"),
           cnt_library = str_count(txt, "library(\\s|)\\(.*\\)"),
           cnt_function = str_count(txt, "function(\\s|)\\("),
           cnt_filter = str_count(txt, "filter(_\\w{0,}|)(\\s|)\\("),
           cnt_mutate = str_count(txt, "mutate(_\\w{0,}|)(\\s|)\\("),
           cnt_select = str_count(txt, "select(_\\w{0,}|)(\\s|)\\("),
           cnt_groupby = str_count(txt, "group_by(_\\w{0,}|)(\\s|)\\("),
           cnt_summarise = str_count(txt, "summari(s|z)e(_\\w{0,}|)(\\s|)\\("),
           used_library = str_extract_all(txt, "(?<=library(\\s|)\\()[\\sA-z0-9]+(?=\\))"))
  
  # summary
  x2 <- x1 %>%
    select(-id) %>%
    group_by(file, filename) %>%
    summarise(is_blank = sum(ifelse(is_fill == 0, 1, 0))) %>%
    ungroup() %>%
    left_join(x1 %>%
                select(-id) %>%
                group_by(file, filename) %>%
                summarise_if(is.numeric, sum)) %>%
    bind_cols(x1 %>%
                select(used_library) %>%
                unnest() %>%
                nest(used_library, .key = used_library)) %>%
    rename(cnt_blank = is_blank,
           cnt_fill = is_fill,
           cnt_comment = is_comment)
  
  # output
  return(list(raw = x1, summary = x2))
}
