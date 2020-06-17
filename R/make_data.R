
library(tidyverse)

raw_ratings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv")

write_csv(raw_ratings, 'data/office_ratings.csv')

raw_info <- schrute::theoffice %>% 
  select(-index, -text_w_direction, -imdb_rating, -total_votes, -air_date)

raw_directors <- raw_info %>% 
  select(season, episode_name, director, writer) %>% 
  distinct() %>% 
  separate_rows(writer, sep = ';')

write_csv(raw_directors, 'data/office_director_and_writers.csv')



# ggdata <- raw_info %>%
#   group_by(season, episode) %>%
#   count(character) %>%
#   left_join(raw_ratings, by = c('season', 'episode'))

# 
# ungroup(ggdata) %>%
#   filter(character == 'Jim') %>%
#   ggplot() +
#   aes(x = n, y = imdb_rating, col = factor(season), label =) +
#   geom_smooth(method = 'lm', se = FALSE) +
#   geom_point() +
#   facet_wrap(~season)
