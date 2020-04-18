# Tidy  Tuesday ----------
# 14 April 2020 ----------
# Max Salvatore ----------

library(tidyverse)
library(here)
library(tidytuesdayR)
library(extrafont)
library(ggtext)
library(Cairo)

year    <- "2020"
tt_week <- "2020-04-14"

dir.create(here(year, tt_week), recursive = TRUE)

# data ----------
polls    <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# Top 10 songs of all time graph ---------
caption_text <- "**Source:** BBC Music<br>
                 **Twitter:** @MaxSalTweets<br>
                 **GitHub:**  github.com/maxsal"

dat <- rankings %>% 
  arrange(desc(points)) %>%
  mutate(across(is.character, as.factor)) %>%
  mutate(
    year = as.factor(year),
    label = paste0(title, " (", artist,", " , year, ")")
    )

(plt <- dat %>%
  head(10) %>%
  ggplot(aes(x = ID, y = points)) +
  geom_bar(stat = "identity", fill = "#1696d2") +
  scale_x_continuous(trans = "reverse", breaks = seq(1, 10, 1)) +
  geom_text(aes(label = label, x  = ID, y = points  - 1, hjust = 1), size = 2.4, color = "white", fontface = "bold") +
  coord_flip()  +
  labs(
    title    = "Top 10 Greatest Hip-Hop Songs of All Time",
    subtitle = "a Tidy Tuesday graph",
    x        = "rank",
    y        = "points",
    caption  = caption_text
  ) +
  theme_minimal() +
  theme(
    text             = element_text(family = "Lato"),
    plot.title       = element_text(size = 18, face = "bold"),
    plot.subtitle    = element_text(size = 14, color = "#36454f"),
    plot.caption     = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1),
    axis.text        = element_text(size = 12, color = "#36454f"),
    axis.title       = element_text(size = 12, face = "italic"),
    legend.position  = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ))

ggsave(filename = here(year, tt_week, "/best_hiphop_songs_ever.pdf"),
       plot     = plt,
       width    = 8, height = 5, device = cairo_pdf)

ggsave(filename = here(year, tt_week, "/best_hiphop_songs_ever.png"),
       plot     = plt,
       width    = 8, height = 5, dpi = 320, units = "in")

# Best song by year ----------
dat2 <- rankings %>%
  group_by(year) %>%
  summarise(points = sum(points)) %>%
  ungroup()  %>%
  arrange(year)

(plt1 <- dat2 %>%
  ggplot(aes(x = year, y = points)) +
  geom_line(size = 0.8)  +
  geom_point(size = 2) +
  geom_point(data = filter(dat2, points == max(points)), aes(x = year, y = points), size = 2, color = "#fdbf11") +
  geom_point(data = filter(dat2 %>% 
                             filter(year == dat %>% 
                                      filter(gender == "female") %>% 
                                      filter(points == max(points)) %>% .$year)),
             aes(x = year, y = points), size = 2, color = "#ec008b"
             ) +
  geom_point(data = filter(dat2, points == min(points)), 
             aes(x = year, y = points), size = 2, color = "#db2b27") +
  geom_point(data = filter(dat2, year == max(year)), 
             aes(x = year, y = points), size = 2, color = "#1696d2") +
  annotate(
    "curve",
    x         = 1999.6,
    xend      = 1994.4,
    y         = 275,
    yend      = 306,
    curvature = 0.1,
    arrow     = arrow(length = unit(2, "mm")),
    color     = "#fdbf11"
  ) +
  annotate(
    "text",
    x     = 1999.7,
    y     = 275,
    size  = 3,
    hjust = 0,
    label = "Best hip-hop song ever,\n\"Juicy\" by The Notorious B.I.G.,\ntops 1994",
    color = "#fdbf11",
    fontface = "bold"
  ) +
  annotate(
    "curve",
    x         = 1989.8,
    xend      = 1992.75,
    y         = 275,
    yend      = 257,
    curvature = -0.25,
    arrow     = arrow(length = unit(2, "mm")),
    color     = "#ec008b"
  ) +
  annotate(
    "text",
    x        = 1989.7,
    y        = 275,
    size     = 3,
    hjust    = 1,
    label    = "Queen Latifah's \"U.N.I.T.Y.\" \n leads 1993 with highest-ever scoring\nsong by a female artist",
    color    = "#ec008b",
    fontface = "bold"
  ) +
  annotate(
    "curve",
    x         = 1983,
    xend      = 1984.95,
    y         = 130,
    yend      = 11,
    curvature = -0.1,
    arrow     = arrow(length = unit(2, "mm")),
    color     = "#db2b27"
  ) +
  annotate(
    "text",
    x        = 1983,
    y        = 149,
    size     = 3,
    hjust    = .5,
    label    = "1984 is worst year\nfor hip-hop",
    color    = "#db2b27",
    fontface = "bold"
  ) +
  annotate(
    "curve",
    x         = 2014,
    xend      = 2019,
    y         = 120,
    yend      = 22,
    curvature = -0.15,
    arrow     = arrow(length = unit(2, "mm")),
    color     = "#1696d2"
  ) +
  annotate(
    "text",
    x        = 2013.5,
    y        = 120,
    size     = 3,
    hjust    = 1,
    label    = "2019 closes out decade\nwith worst year\nsince 1990",
    color    = "#1696d2",
    fontface = "bold"
  ) +
  labs(
    title    = "Total Hip Hop Scores by Year",
    subtitle = "a Tidy Tuesday graph",
    x        = "year",
    y        = "points",
    caption  = caption_text
  ) +
  theme_minimal() +
  theme(
    text               = element_text(family = "Lato"),
    plot.title         = element_text(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 14, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1),
    axis.text          = element_text(size = 12, color = "#36454f"),
    axis.title         = element_text(size = 12, face = "italic"),
    legend.position    = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ))

ggsave(filename = here(year, tt_week, "/total_score_by_year.pdf"),
       plot     = plt1,
       width    = 8, height = 5, device = cairo_pdf)

ggsave(filename = here(year, tt_week, "/total_score_by_year.png"),
       plot     = plt1,
       width    = 8, height = 5, dpi = 320, units = "in")
