install.packages("countrycode")

library(tidyverse)
library(ggrepel)
library(countrycode)

game_medal_tally <- read_csv("data/raw/game_medal_tally.csv.gz")
game <- read_csv("data/raw/game.csv.gz")

# Main table
medals_per_country_per_edition <- game_medal_tally |> 
  mutate(edition_id = replace(edition_id, edition_id == 48, 14)) |>
  left_join(
    game |> select(edition_id, host_country = country_noc), 
    by = "edition_id"
  ) |>
  filter(year != 1906) |>
  mutate(
    edition = case_when(
      str_detect(edition, "Summer") ~ "Olimpíadas de Verão",
      str_detect(edition, "Equestrian") ~ "Olimpíadas de Verão",
      str_detect(edition, "Winter") ~ "Olimpíadas de Inverno",
      TRUE                          ~ "other"
    ),
    is_host = country_noc == host_country,
    score_total  = (4 * gold) + (2 * silver) + bronze,
  ) |>
  group_by(country_noc, edition_id, year, edition) |>
  summarise(
    gold = sum(gold, na.rm = TRUE),
    silver = sum(silver, na.rm = TRUE),
    bronze = sum(bronze, na.rm = TRUE),
    total = sum(total, na.rm = TRUE),
    is_host = any(is_host),
    score_total = sum(score_total, na.rm = TRUE),
    .groups = "drop"
  )  |>
  mutate(
    score_summer = if_else(edition == "Olimpíadas de Verão", score_total, 0),
    score_winter = if_else(edition == "Olimpíadas de Inverno", score_total, 0),
    continent = countrycode(country_noc, "ioc", "continent"),
    continent = case_when(
      country_noc == "AHO" ~ "Americas",   # Netherlands Antilles
      country_noc == "ANZ" ~ "Oceania",    # Australasia
      country_noc == "BOH" ~ "Europe",     # Bohemia
      country_noc == "EUN" ~ "Europe",     # Equipe Unificada
      country_noc == "FRG" ~ "Europe",     # Alemanha Ocidental
      country_noc == "GDR" ~ "Europe",     # Alemanha Oriental
      country_noc == "IOA" ~ NA_character_,# Atletas Independentes
      country_noc == "KOS" ~ "Europe",     # Kosovo
      country_noc == "MIX" ~ NA_character_,# Mixed Team
      country_noc == "ROC" & year < 2020 ~ "Asia",    # Taiwan/China
      country_noc == "ROC" & year >= 2020 ~ "Europe", # Comitê Olímpico Russo
      country_noc == "SCG" ~ "Europe",     # Sérvia e Montenegro
      country_noc == "TCH" ~ "Europe",     # Tchecoslováquia
      country_noc == "UAR" ~ "Africa",     # República Árabe Unida (Egito)
      country_noc == "URS" ~ "Europe",     # União Soviética
      country_noc == "WIF" ~ "Americas",   # West Indies Federation (Caribe)
      country_noc == "YUG" ~ "Europe",     # Iugoslávia
      TRUE ~ continent
    )
  ) |>
  filter(!is.na(continent)) |>
  group_by(edition_id) |>
  mutate(
    medal_rate = total / sum(total)
  ) |>
  ungroup()

# Secondary table
score_cum <- medals_per_country_per_edition |>
  arrange(country_noc, edition_id) |>
  group_by(country_noc, edition) |>
  mutate(
    score_cum = cumsum(score_total)
  ) |>
  ungroup() |>
  select(
    country_noc,
    continent,
    year,
    edition,
    is_host,
    score = score_cum
  ) |>
  mutate(
    edition = factor(
      edition,
      levels = c("Olimpíadas de Verão",
                 "Olimpíadas de Inverno")
    )
  )

# PLOT ACCUMULATED SCORE
huge_score <- score_cum |>
  group_by(country_noc, edition) |>
  filter((edition == "Olimpíadas de Verão" & score >= 2000) | (edition == "Olimpíadas de Inverno" & score >= 450)) |>
  slice_max(year, n = 1)

# Color by continent
score_by_continent <- ggplot(
    score_cum,
    aes(x = year, y = score, color = continent, group = country_noc)
  ) +
  geom_line(size = 0.8, alpha = 0.4) +
  scale_color_manual(
    name = "Continente",
    values = c(
      "Africa"   = "#E69F00",
      "Americas" = "#CC79A7",
      "Asia"     = "#009E73",
      "Europe"   = "#0072B2",
      "Oceania"  = "#56B4E9"
    ),
    labels = c(
      "Africa"   = "África",
      "Americas" = "América",
      "Asia"     = "Ásia",
      "Europe"   = "Europa",
      "Oceania"  = "Oceania"
    )
  ) +
  geom_point(
    data = huge_score,
    size = 2,
    color = "black",
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = bind_rows(huge_score),
    aes(label = country_noc),
    size = 3,
    color = "black",
    show.legend = FALSE
  ) +
  facet_wrap(~ edition, ncol = 2, scales = "free") +
  labs(
    title = "Evolução do score acumulado por país",
    x = NULL,
    y = "Score acumulado",
    caption = "Fonte: Base dos Dados - Historical Data from the Olympics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 15
    ),
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(size = 9, color = "gray20"),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray90", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85")
  )
plot(score_by_continent)

# Top 3 by continent
top_countries <- score_cum |>
  group_by(edition, continent, country_noc) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  group_by(edition, continent) |>
  slice_max(score, n = 3) |>
  ungroup() |>
  select(edition, continent, country_noc)

top_by_continent <- score_cum |>
  semi_join(
    top_countries,
    by = c("edition", "continent", "country_noc")
  )

labels_data <- top_by_continent |>
  group_by(edition, country_noc) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    group_id = cur_group_id(),
    n = n(),
    label_position = round(n * (0.1 + 0.2 * (group_id %% 3)))
  ) |>
  filter(row_number() == label_position) |>
  ungroup()

top_score_by_continent <- ggplot(
  top_by_continent,
  aes(x = year, y = score, color = continent, group = country_noc)
) +
  geom_line(size = 0.8, alpha = 0.4) +
  scale_color_manual(
    name = "Continente",
    values = c(
      "Africa"   = "#E69F00",
      "Americas" = "#CC79A7",
      "Asia"     = "#009E73",
      "Europe"   = "#0072B2",
      "Oceania"  = "#56B4E9"
    ),
    labels = c(
      "Africa"   = "África",
      "Americas" = "América",
      "Asia"     = "Ásia",
      "Europe"   = "Europa",
      "Oceania"  = "Oceania"
    )
  ) +
  geom_text_repel(
    data = labels_data,
    aes(label = country_noc),
    size = 3,
    show.legend = FALSE
  ) +
  facet_wrap(~ edition, ncol = 2, scales = "free") +
  labs(
    title = "Evolução do score acumulado por país",
    subtitle = "Top 3 atuais por continente",
    x = NULL,
    y = "Score acumulado",
    caption = "Fonte: Base dos Dados - Historical Data from the Olympics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 15
    ),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
    plot.caption = element_text(size = 9, color = "gray20"),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray90", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85")
  )
plot(top_score_by_continent)

# Americas
labels_data <- score_cum |>
  filter(continent == "Americas") |>
  group_by(edition, country_noc) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    group_id = cur_group_id(),
    n = n(),
    label_position = round(n * (0.1 + 0.3 * (group_id %% 3)))
  ) |>
  filter(row_number() == label_position) |>
  ungroup()

score_america <- ggplot(
  score_cum |> filter(continent == "Americas"),
  aes(
    x = year,
    y = score, 
    color = country_noc,
  )
  ) +
  geom_line(size = 0.8, show.legend = FALSE) +
  geom_text_repel(
    data = labels_data,
    aes(label = country_noc),
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~ edition, ncol = 2, scales = "free") +
  labs(
    title = "Evolução do score acumulado por país",
    subtitle = "América",
    x = NULL,
    y = "Score acumulado",
    caption = "Fonte: Base dos Dados - Historical Data from the Olympics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 15
    ),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
    plot.caption = element_text(size = 9, color = "gray20"),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray90", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85")
  )
plot(score_america)

# Americas (Highlight Brasil)
score_america_brasil <- ggplot(
  score_cum |> filter(continent == "Americas"),
  aes(
    x = year,
    y = score, 
    color = country_noc, 
    alpha = country_noc == "BRA" | edition == "Olimpíadas de Inverno")
) +
  scale_alpha_manual(
    values = c(
      "TRUE" = 1,
      "FALSE" = 0.3
    ),
    guide = "none"
  ) +
  geom_line(size = 0.8, show.legend = FALSE) +
  geom_text_repel(
    data = labels_data,
    aes(label = country_noc),
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~ edition, ncol = 2, scales = "free") +
  labs(
    title = "Evolução do score acumulado por país",
    subtitle = "América - Brasil em destaque",
    x = NULL,
    y = "Score acumulado",
    caption = "Fonte: Base dos Dados - Historical Data from the Olympics"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 15
    ),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
    plot.caption = element_text(size = 9, color = "gray20"),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray90", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85")
  )
plot(score_america_brasil)

# GARBAGE
host_palette <- c(
  "Sede" = "#1f78b4",
  "Não sede" = "#bdbdbd"
)

# PLOT MEAN SCORE
performances <- medals_per_country_per_edition |>
  group_by(country_noc, edition, is_host) |>
  summarise(
    score_total = sum(score_total),
    mean_medal_rate = mean(medal_rate)
  )

g2 <- performances |>
  group_by(edition, is_host) |>
  summarise(
    mean_score = mean(score_total),
    .groups = "drop"
  ) |> 
  mutate(
    is_host = factor(
      is_host,
      levels = c(TRUE, FALSE),
      labels = c("Sede", "Não sede")
    )
  ) |>
  ggplot(aes(x = is_host, y = mean_score, fill = is_host)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = round(mean_score, 1)),
    vjust = -0.5,
    size = 4
  ) +
  facet_wrap(~ edition) +
  scale_fill_manual(values = host_palette) +
  labs(
    title = "Score médio por país",
    subtitle = "Comparação entre países sede e não sede",
    x = NULL,
    y = "Score médio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )
# Pode estar associado à escolha do país sede (São escolhidos países tradicionais?)
plot(g2)

# Plot Mean Medal Rate
g3 <- performances |>
  mutate(
    is_host = factor(
      is_host,
      levels = c(TRUE, FALSE),
      labels = c("Sede", "Não sede")
    )
  ) |>
  ggplot(aes(x = is_host, y = mean_medal_rate, fill = is_host)) +
  geom_boxplot(alpha = 0.8, width = 0.6, show.legend = FALSE) +
  facet_wrap(~ edition) +
  scale_fill_manual(values = host_palette) +
  labs(
    title = "Medal rate médio por país",
    subtitle = "Distribuição entre países sede e não sede",
    x = NULL,
    y = "Medal rate médio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )
plot(g3)


# Plot Medal Rate Distribution
g4 <- medals_per_country_per_edition |>
  mutate(
    is_host = factor(
      is_host,
      levels = c(TRUE, FALSE),
      labels = c("Sede", "Não sede")
    )
  ) |>
  ggplot(aes(x = year, y = medal_rate, color = is_host)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ edition) +
  scale_color_manual(values = host_palette) +
  labs(
    title = "Medal rate ao longo do tempo",
    subtitle = "Comparação entre países sede e não sede",
    x = "Ano",
    y = "Medal rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )
plot(g4)

# Plot Brazilian Score
g5 <- performances |>
  filter(country_noc == "BRA") |>
  group_by(edition, is_host) |>
  summarise(
    mean_score = mean(score_total),
    .groups = "drop"
  ) |>
  mutate(
    is_host = factor(
      is_host,
      levels = c(TRUE, FALSE),
      labels = c("Sede", "Não sede")
    )
  ) |>
  ggplot(aes(x = is_host, y = mean_score, fill = is_host)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = round(mean_score, 1)),
    vjust = -0.5,
    size = 4
  ) +
  facet_wrap(~ edition) +
  scale_fill_manual(values = host_palette) +
  labs(
    title = "Score médio do Brasil por edição",
    subtitle = "Comparação entre anos como sede e não sede",
    x = NULL,
    y = "Score médio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )
plot(g5)

# Plot Brazilian Medal Rate
g6 <- medals_per_country_per_edition |>
  filter(country_noc == "BRA") |>
  mutate(
    is_host = factor(
      is_host,
      levels = c(TRUE, FALSE),
      labels = c("Sede", "Não sede")
    )
  ) |>
  ggplot(aes(x = year, y = medal_rate, color = is_host)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(size = 2, alpha = 0.8) +
  facet_wrap(~ edition) +
  scale_color_manual(values = host_palette) +
  labs(
    title = "Medal rate do Brasil ao longo do tempo",
    subtitle = "Comparação entre edições como sede e não sede",
    x = "Ano",
    y = "Medal rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )
plot(g6)

