# spotify data 
# data pull and visualization 
pacman::p_load(tidyverse, janitor, here, glue, spotifyr)

# token 
access_token <- get_spotify_access_token()

# get key table 
key_table <- readRDS(here("data", "key_table.rds"))

# colors 
colors <- c("dodgerblue", my_pal("btr")[c(2, 4, 8, 10)], "darkgreen")

# set width and height 
w = 8.62; h = 5.25

# list of playlists 
pids <- tibble(playlist_ids = c("6kbi8XCLWQdp2gJk0p8Jku", "0Q1pk6Fh1Pkx17whDOdc4G",
                  "37i9dQZF1Epk2cwLl6GSoW", "33VmLDOtynHe2VQj7D955L",
                  "3nkbOYgGaY0LdPO5AVVjDv", "37i9dQZF1EQqkOPvHGajmW"),
               name = c("Kevin", "Kateri McLucas", "Fausto Briosa",
                        "Paymon Parsia", "Jyoti Gill", "Maria Ragusa"))


# get tracks of the playlists 
df_list <- pids$playlist_ids %>% 
  map(get_playlist_tracks) %>% 
  set_names(pids$name)

# we did it in a list to preserve playlist owner, now we pass through the name 
df <- bind_rows(df_list, .id = "name")

# get features of each song (more than 100, so have to chunk it out)
playlist_features <- df %>%
  mutate(id_group = row_number() %/% 80) %>%
  select(id_group, track.id) %>%
  nest(data = c(track.id)) %>%
  mutate(audio_features = map(data, ~ get_track_audio_features(.$track.id)))

# unnest the features
tmp <- playlist_features %>% 
  unnest(c(data, audio_features)) %>% 
  select(-id_group)
  
# put together into data.frame
df <- df %>% 
  left_join(tmp, by = c("track.id" = "track.id")) %>%
  left_join(key_table, by = c("key" = "pitchclass"))

rm(tmp, playlist_features, df_list, pids)

# convert milliseconds to time 
ms_time <- function(x) format( as.POSIXct(Sys.Date())+x/1000, "%M:%S")

# convert year
track_year <- function(tbl) {
  tbl %>% 
    mutate(track.album.release_date = case_when(track.album.release_date_precision == "month" ~ paste0(track.album.release_date, "-01"),
                                                track.album.release_date_precision == "year" ~ paste0(track.album.release_date, "-01-01"),
                                                TRUE ~ track.album.release_date), 
    track_year = lubridate::year(as.Date(track.album.release_date)))
}

df <- df %>% 
  track_year

# average year 
df %>% group_by(name) %>% 
  summarise(mean(track_year))


# radar plot / spider chart -----------------------------------------------
library(ggradar)

scale_feat <- function(x){(x-min(x))/diff(range(x))}

sum_features <- df %>% 
  group_by(name) %>% 
  summarise(across(danceability:valence, mean)) %>%
  select(-key) %>%
  # now scale
  ungroup() %>% 
  mutate(across(danceability:valence, scale_feat)) %>%
  set_names(., str_to_title(names(.))) %>% 
  rename(Instrumental = Instrumentalness)

names <- sum_features %>% pull(Name)

for (i in 1:6) {
 nam <- paste0("p", i)
 assign(nam, ggradar(
    sum_features[i,],
    values.radar = c("0",".5", "1"),
    grid.min = 0, grid.mid = .5, grid.max = 1,
    # Polygons
    group.line.width = 1,
    group.point.size = 3,
    group.colours = colors[i],
    # Background and grid lines
    background.circle.colour = "white",
    gridline.mid.colour = "grey",
    grid.label.size = 8,
    legend.position = "bottom",
    axis.label.size = 6,
    plot.title = names[i],
    fill = TRUE,
    fill.alpha = .3
  ) + 
    theme(plot.title = element_text(size = 28,
                                    face = "bold"))) 
   
}

library(patchwork)
(p1 + p2 + p3) / (p4 + p5 + p6) +
  plot_annotation(
    caption = 'Source: Spotify'
  ) +
  theme(text = element_text('sans'))
ggsave(here("figures", "radars.png"), dpi = "print")


# valence versus energy ---------------------------------------------------

df <- df %>% 
  mutate(pop_percentile = case_when(track.popularity <= 25 ~ "<25th Percentile",
                                    track.popularity > 25 & track.popularity <= 50 ~ "26-50 Percentile",
                                    track.popularity > 50 & track.popularity <= 75 ~ "51-75th Percentile",
                                    TRUE ~ ">75th Percentile"),
         pop_percentile = factor(pop_percentile, levels = c("<25th Percentile",
                                                            "26-50 Percentile",
                                                            "51-75th Percentile",
                                                            ">75th Percentile")))

ggplot(df, aes(x = valence, y = energy, group = name, fill = name, size = pop_percentile)) + 
  geom_hline(yintercept = .5, linetype = "dashed") + 
  geom_vline(xintercept = .5, linetype = "dashed") + 
  geom_jitter(shape = 21, stroke = .2) + 
  scale_size_manual(values = c(1,2.5,4,5.5),
                    name = "Track\nPopularity") +
  scale_fill_manual(values = colors,
                    name = "Playlist\nOwner") + 
  annotate('text', x = 1, y = 1, 
           label = "HAPPY", family = "Open Sans",
           fontface = "bold.italic",
           size = 10, alpha = .5) + 
  annotate('text', x = 1, y = 0, 
           label = "CALM", family = "Open Sans",
           fontface = "bold.italic",
           size = 10, alpha = .5) +
  annotate('text', x = 0, y = 1, 
           label = "ANGRY", family = "Open Sans",
           fontface = "bold.italic",
           size = 10, alpha = .5) +
  annotate('text', x = 0, y = 0, 
           label = "SAD", family = "Open Sans",
           fontface = "bold.italic",
           size = 10, alpha = .5) +
  theme_twg(grid = FALSE, axis = TRUE) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=18),
        panel.border = element_rect(colour = "#2b2b2b", fill=NA, size=.15),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.spacing.x = unit(.1, 'cm'),
        legend.justification = "center",
        legend.title = element_text(size=20,
                                    angle = 90, 
                                    hjust = 0.5,
                                    lineheight = 0.3,
                                    face = "bold",
                                    family = "Open Sans")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE,
                           override.aes = list(size = 4))) +
  guides(size = guide_legend(nrow = 2, byrow = T)) +
  labs(x = "Valence", y = "Energy",
       caption = "Source: Spotify") 
ggsave(here("figures", "valence_x_energy.png"), 
       dpi = "print", width = 8.62, height = 5.25)


# valence across playlist  ------------------------------------------------
library(syuzhet)

tmp <- df %>% 
  select(name, valence)

dct <- function(n) {
  tmptmp <- tmp %>% 
    filter(name == n) %>% 
    pull(valence)
  
  dct_out <- get_dct_transform(
    tmptmp, 
    low_pass_size = 10, 
    x_reverse_len = 100,
    scale_vals = F,
    scale_range = T
  )
  
  out <- tibble(name = n, 
                valence_dct = dct_out)
  
}

dct_valence <- names %>% 
  map_dfr(dct)

dct_valence %>% 
  group_by(name) %>% 
  mutate(id = row_number()) %>% 
  ggplot(aes(x = id, y = valence_dct, fill = name, color = name)) + 
  geom_area(alpha = .8, size = .5) +
  scale_fill_manual(values = colors, 
                     name = "none") +
  scale_color_manual(values = colors, 
                    name = "none") +
  geom_hline(yintercept = 0, size = .1, color = "#B2B2B2") + 
  facet_wrap(~name, nrow = 3, ncol = 2) + 
  theme_twg(grid = FALSE) + 
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.background =element_rect(fill="#B2B2B2", color = NA),
        strip.text.x = element_text(size = 30, family = "Open Sans"),
        plot.title = element_text(size = 40, family = "Open Sans")) + 
  labs(x = NULL, y = NULL,
       title = "Emotional Rollercoaster of Each Playlist", 
       caption = "Source: Spotify")
ggsave(here("figures", "emotional_roller.png"), 
       dpi = "print", width = w, height = h)


# tsne ---------------------------------------------------------
library(Rtsne)
tSNE_fit <- df %>%
  select(valence, danceability, energy, loudness, acousticness) %>%
  mutate(id = row_number()) %>%
  column_to_rownames("id") %>%
  scale() %>% 
  Rtsne(check_duplicates = FALSE)

tSNE_df <- tSNE_fit$Y %>% 
  as.data.frame() %>%
  rename(tSNE1="V1",
         tSNE2="V2") %>%
  mutate(id=row_number(),
         name = df$name, 
         track = df$track.album.name)

tSNE_df %>%
  ggplot(aes(x = tSNE1, 
             y = tSNE2,
             color = name))+
  geom_point() +
  theme(legend.position="bottom")

library(ggfortify)
tmp <- df %>%
  select(valence, danceability, energy, loudness, acousticness)

pca_res <- prcomp(tmp, scale. = TRUE)

autoplot(pca_res, loadings = TRUE)





