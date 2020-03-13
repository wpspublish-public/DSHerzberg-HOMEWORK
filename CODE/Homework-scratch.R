suppressMessages(library(here))
suppressMessages(library(tidyverse))

# Problem component 1
input1 <- tribble(
  ~group, ~score, ~form,
  1, 10, 'A',
  1, 20, 'B',
  1, 30, 'C',
  1, 40, 'D',
  2, 11, 'A',
  2, 21, 'B',
  2, 31, 'C',
  2, 41, 'D',
  3, 12, 'A',
  3, 22, 'B',
  4, 13, 'A',
  4, 23, 'B',
  4, 33, 'C',
  4, 43, 'D'
)

write_csv(input1, here('INPUT-FILES/input1.csv'))

form_means <- input1 %>% 
  group_by(form) %>% 
  summarise(mean = mean(score))

output1 <- complete(input1, group, form) %>%
  mutate(score = case_when(
    group == 3 & form == 'C' ~ round(as.numeric(form_means[3,2]), 0),
    group == 3 & form == 'D' ~ round(as.numeric(form_means[4,2]), 0),
    TRUE ~ score
  ))

# Problem component 2

table_names <- c('dfA', 'dfB', 'dfC')

dfA <- tibble(a = 1:3, b = 4:6, c = 7:9)
dfB <- tibble(a = 10:12, b = 13:15, c = 16:18)
dfC <- tibble(a = 19:21, b = 22:24, c = 25:27)

write_csv(dfA, here('INPUT-FILES/dfA.csv'))
write_csv(dfB, here('INPUT-FILES/dfB.csv'))
write_csv(dfC, here('INPUT-FILES/dfC.csv'))

df_list <- list(dfA, dfB, dfC) %>% setNames(table_names)

df_out <- imap(df_list, 
               ~.x %>% 
                 mutate(name = .y) %>% 
                 select(name, everything())
               ) %>% 
  reduce(bind_rows)

# Problem component 3
input3 <- tibble(
  person = rep(101:102, each = 10),
  item = as.factor(rep(1:10, 2)),
  response = sample(1:4, 20, replace = T),
  scale = as.factor(rep(rep(1:2, each = 5), 2))
) %>% mutate(
  scale_last = case_when(
    as.integer(scale) != lead(as.integer(scale)) | is.na(lead(as.integer(scale))) ~ 1,
    TRUE ~ NA_real_
  )
)
df

write_csv(input3, here('INPUT-FILES/input3.csv'))

library(fastDummies)
dum <- input3 %>% 
  dummy_cols(select_columns = 'item') %>% 
  mutate_at(vars(starts_with("item_")), ~replace(., scale_last == 1, 0)) %>% 
  select(-scale_last)
dum

# Problem component 4
rawscore_table <- data.frame(rawscore = 10:14, SS1 = NA_integer_, SS2 = NA_integer_)

write_csv(rawscore_table, here('INPUT-FILES/input4a.csv'))

lookup_table <- tibble(CS = c('SS1', 'SS2'), x = 1:2, y = 3:4)

write_csv(lookup_table, here('INPUT-FILES/input4b.csv'))

output <- rawscore_table %>%
  gather(CS, val, -rawscore) %>%
  left_join(lookup_table, by = 'CS') %>%
  mutate(val = rawscore + x + y, x = NULL, y = NULL) %>%
  spread(CS, val)
