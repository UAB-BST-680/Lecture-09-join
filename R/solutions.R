
library(tidyverse)
library(gghighlight)
library(glue)


raw_directors <- read_csv('data/office_director_and_writers.csv') %>% 
  rename(title = episode_name)

raw_ratings <- read_csv('data/office_ratings.csv')


# problem 1 ---------------------------------------------------------------

# do exploratory data analysis

p1 <- raw_ratings %>% 
  distinct(season, title, imdb_rating, air_date) %>% 
  mutate(season = factor(season, labels = paste("Season", 1:9))) %>% 
  group_by(season) %>% 
  mutate(best_epi = imdb_rating == max(imdb_rating)) %>% 
  ggplot() + 
  aes(x = air_date, y = imdb_rating) + 
  geom_point(col = 'purple') +
  facet_wrap(~season, scales = 'free_x') +
  gghighlight(best_epi, label_key = title, label_params = list(force=5)) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  labs(x = 'Air date', y = 'Mean IMDB rating',
       title = 'IMDB ratings for The Office')

ggsave(filename = '01_solution.png', plot = p1, path = 'solutions',
       dpi = 300, height = 9, width = 8)

# problem 2 ---------------------------------------------------------------

# identify which episode names do not match

p2 <- list(
  titles_unmatched_directors = raw_directors %>% 
    anti_join(raw_ratings) %>% 
    pull(title),
  titles_unmatched_ratings = raw_ratings %>% 
    anti_join(raw_directors) %>% 
    pull(title)
)

write_rds(p2, 'solutions/02_solution.rds')

# problem 3 ---------------------------------------------------------------

# hints: I used the following functions: tolower, str_remove_all, and str_replace

raw_ratings <- raw_ratings %>% 
  mutate(
    title = tolower(title),
    title = str_remove_all(title, '\\.'),
    title = str_remove_all(title, '\\:'),
    title = str_remove_all(title, 'part 1'),
    title = str_remove_all(title, 'part 2'),
    title = str_replace(title, 'cover\\-up', 'cover'),
    title = trimws(title)
  )

raw_directors <- raw_directors %>% 
  mutate(
    title = tolower(title),
    title = str_replace(title, 'surveilance', 'surveillance'),
    title = str_remove_all(title, '\\.'),
    title = str_replace(title, '\\*', 'e'),
    title = str_replace(title, '\\(parts 1\\&2\\)', ''),
    title = str_replace(title, '\\(part 1\\)', ''),
    title = str_replace(title, '\\(part 2\\)', ''),
    title = str_replace(title, '\\&', 'and'),
    title = str_replace(title, 'the new guys', 'new guys'),
    title = str_replace(title, 'the michael scott paper company', 
                        'michael scott paper company'),
    title = str_replace(title, 'the manager and the salesman', 
                        'manager and salesman'),
    title = trimws(title)
  )

# check to make sure everything matches

raw_directors %>% 
  anti_join(raw_ratings) %>% 
  pull(title)

raw_ratings %>% 
  anti_join(raw_directors) %>% 
  pull(title)

data_all <- raw_directors %>% 
  left_join(raw_ratings)

write_rds(data_all, 'solutions/03_solution.rds')

# problem 4 ---------------------------------------------------------------

writer_smry <- data_all %>% 
  group_by(writer) %>% 
  summarise(
    episodes_written = n(),
    rating_mean = mean(imdb_rating)
  ) %>% 
  mutate(
    top_writer = rating_mean > 8.6 | episodes_written > 20
  )

p4 <- ggplot(writer_smry) + 
  aes(x = episodes_written, y = rating_mean, col = writer) + 
  geom_point(size = 5) + 
  gghighlight(top_writer) + 
  scale_x_continuous(breaks = c(2, 10, 13, 24)) + 
  theme_bw() + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  labs(x = 'Episodes written', y = 'Mean IMDB rating')

ggsave(filename = '04_solution.png', plot = p4, path = 'solutions',
       dpi = 300, height = 6, width = 8)

# problem 5 ---------------------------------------------------------------

library(tblStrings)
library(gt)

writer_tbl <- data_all %>%
  left_join(writer_smry) %>%
  filter(top_writer) %>% 
  group_by(writer) %>% 
  arrange(desc(imdb_rating)) %>% 
  slice(1:min(n(), 5)) %>% 
  arrange(air_date) %>% 
  mutate(
    title = str_to_title(title),
    writer = tbl_string(
      "Top {n()} episodes out of {episodes_written} written by {writer}"
    )
  ) %>% 
  select(writer, title, air_date, imdb_rating) %>% 
  gt(rowname_col = 'title') %>% 
  cols_label(air_date = 'Air date', imdb_rating = 'IMDB rating') %>% 
  fmt_date(columns = 'air_date', date_style = 5)

write_rds(writer_tbl, 'solutions/05_solution.rds')










