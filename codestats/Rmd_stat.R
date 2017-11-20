library(tidyverse)
library(tidytext)
library(stringr)

# purpose
# 1. get pipe counts, "<-" counts
# 2. used packages
# 3. metrics: number of comments, number of lines, number of chars, number of words, number of functions
# 4. rmd: title (x), date (x), chunk, name (x)

get_stat_rmd <- function(file) {
  # init
  filename <- basename(file)
  
  # get main tbl
  x1 <- readLines(file, warn = FALSE)
  x1 <- data_frame(filename, file, id = 1:length(x1), txt = x1)
  x1 <- x1 %>%
    mutate(state = case_when(txt == "```" ~ "end",
                             grepl("```(\\s+|)\\{r.*\\}", txt) ~ "start",
                             TRUE ~ NA_character_),
           chunk = ifelse(state == "start", str_extract(txt, "(?<=```\\{r )[A-z0-9\\-]+"), NA_character_))
  
  # get start end range
  x <- x1 %>%
    filter(state %in% c("start", "end")) %>%
    select(id, state, chunk)
  x <- x %>%
    inner_join(x %>%
                 rename(state2 = state) %>%
                 select(-chunk) %>%
                 mutate(id2 = id,
                        id = lag(id)), by = c("id"="id")) %>%
    select(id, id2, state, state2, chunk) %>%
    filter(state != "end", state2 != "start") %>%
    select(id_start = id, id_end = id2, -state, -state2, chunks = chunk) %>%
    group_by(chunks) %>%
    mutate(r = row_number(),
           chunks2 = ifelse(is.na(chunks), paste0("unnamed-", r), chunks)) %>%
    ungroup() %>%
    mutate(chunks = chunks2) %>%
    select(-r, -chunks2)
  
  # get chunks grouping
  x2 <- x1 %>%
    mutate(l = 1) %>%
    inner_join(x %>%
                 mutate(l = 1), by = c("l"="l")) %>%
    select(-l) %>%
    mutate(inside = ifelse(id >= id_start & id <= id_end, 1, 0)) %>%
    filter(inside == 1) %>%
    select(id, chunks)
  
  # join main tbl with chunks grouping
  xall <- x1 %>%
    left_join(x2) %>%
    select(-chunk) %>%
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
  
  # summary code per chunks
  xall_code <- xall %>%
    filter(!is.na(chunks), is.na(state))
  xall_code <- xall_code %>%
    left_join(xall_code %>%
                distinct(chunks) %>%
                mutate(id_chunk = row_number()))
  xall_code_summary <- xall_code %>%
    select(-id) %>%
    group_by(filename, file, id_chunk, chunks) %>%
    summarise(is_blank = sum(ifelse(is_fill == 0, 1, 0))) %>%
    ungroup() %>%
    left_join(xall_code %>%
                select(-id) %>%
                group_by(filename, file, id_chunk, chunks) %>%
                summarise_if(is.numeric, sum)) %>%
    rename(cnt_blank = is_blank,
           cnt_fill = is_fill,
           cnt_comment = is_comment)
  
  # rmd words
  xall_rmd <- xall %>%
    filter(is.na(chunks), is_fill >= 1) %>%
    select(filename, file, id, txt) %>%
    unnest_tokens(words, txt, drop = FALSE)

  # summary rmd
  xall_summary <- xall %>%
    select(-id) %>%
    group_by(filename, file) %>%
    summarise(rmd_line = n()) %>%
    ungroup() %>%
    bind_cols(xall_code_summary %>%
                summarise(rmd_chunks = n())) %>%
    bind_cols(xall_rmd %>%
                filter(nchar(words) >= 2) %>%
                summarise(rmd_words = n())) %>%
    left_join(xall_code_summary %>%
                select(-id_chunk) %>%
                group_by(filename, file) %>%
                summarise_if(is.numeric, sum)) %>%
    bind_cols(xall_code %>%
                select(used_library) %>%
                unnest() %>%
                nest(used_library, .key = used_library))
  
  # output
  return(list(raw = xall, 
              summary_chunk = xall_code_summary, 
              summary = xall_summary))
}