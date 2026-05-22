library(tidyverse)
library(sf)
library(ggplot2)
library(here)

sf_use_s2(FALSE)

# Find all simplified seagrass files ----------------------------------------

n_files <- list.files(
  here('data'),
  pattern = '^sgdat\\d{4}\\.RData$',
  full.names = TRUE
)
years <- gsub('.*sgdat(\\d{4})\\.RData$', '\\1', n_files)

cat('Found', length(n_files), 'files for years:', paste(years, collapse = ', '), '\n')

# Load each file and dissolve to binary presence polygon --------------------
# All categories (patchy, continuous, etc.) are collapsed to presence = 1

cat('Loading and dissolving year layers...\n')
year_presence <- map2(n_files, years, function(f, yr) {
  cat(' ', yr, '\n')
  env <- new.env()
  load(f, envir = env)
  obj <- get(ls(env)[1], envir = env)

  # Filter to seagrass only (patchy and continuous), then union to presence polygon
  obj |>
    filter(FLUCCSCODE %in% c(9113, 9116)) |>
    st_union() |>
    st_sf(year = yr, geometry = _)
})
names(year_presence) <- years

# Compute unique polygon pieces across all years ----------------------------
# Successive st_union of all year boundaries produces a fully-noded geometry;
# st_cast then splits it into the smallest unique polygon pieces.

cat('Computing unique polygon pieces across all years...\n')
all_bounds <- map(year_presence, st_geometry) |>
  do.call(what = c) |>
  st_union() |>
  st_cast('MULTIPOLYGON') |>
  st_cast('POLYGON') |>
  st_sf()

cat(nrow(all_bounds), 'unique polygon pieces found\n')

# Count years present per polygon piece ------------------------------------
# Centroids avoid shared-edge ambiguity: a centroid is unambiguously inside
# exactly one polygon piece, so intersection with each year layer is clean.

cat('Counting years of presence per polygon piece...\n')
centroids <- st_centroid(all_bounds)

count_mat <- map(year_presence, function(yp) {
  as.integer(lengths(st_intersects(centroids, yp)) > 0)
}) |>
  do.call(what = cbind)

colnames(count_mat) <- years

# Attach per-year binary columns and total count
all_bounds <- bind_cols(all_bounds, as_tibble(count_mat)) |>
  mutate(n_years = rowSums(count_mat))

# Four-panel plot: frequency bands split across the 18-survey time period ----

p <- ggplot(sgdat_freq) +
        geom_sf(aes(fill = n_years), color = NA) +
        scale_fill_gradient(
           name = 'Years\nMapped',
           low = '#D3FFBE',
           high = '#267300'
          ) +
        theme_void()

freq_breaks <- c(0, 4, 9, 13, 18)
freq_labels  <- c('1–4 years', '5–9 years', '10–13 years', '14–18 years')

sgdat_freq_plot <- sgdat_freq |>
  filter(n_years > 0) |>
  mutate(
    freq_band = cut(n_years, breaks = freq_breaks, labels = freq_labels)
  )

p2 <- ggplot(sgdat_freq_plot) +
  geom_sf(aes(fill = n_years), color = NA) +
  scale_fill_gradient(
    name = 'Years\nMapped',
    low  = '#d73027',
    high = '#1a9850',
    limits = c(1, 18)
  ) +
  facet_wrap(~freq_band, ncol = 2) +
  theme_void() +
  theme(
    strip.text = element_text(face = 'bold', margin = margin(b = 4)),
    legend.position = 'bottom',
    legend.key.width = unit(2, 'cm')
  )


# Save ---------------------------------------------------------------------

sgdat_freq <- all_bounds
save(sgdat_freq, file = here('data/sgdat_freq.RData'), compress = 'xz')

cat('\nDone. Saved to data/sgdat_freq.RData\n')
cat('Occurrence count distribution (n_years):\n')
print(table(sgdat_freq$n_years))

