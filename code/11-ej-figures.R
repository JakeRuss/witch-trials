# Title: 11-ej-figures.R
# Description: Replication code for figures 1 and 2 from "Witch Trials."
# Authors: Peter T. Leeson and Jacob W. Russ 

# Load R packages -------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(magrittr)
library(maptools)
library(rgeos)
library(ggplot2)
library(extrafont)
library(gridExtra)
library(stringr)

# Import datasets -------------------------------------------------------------

figure1 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "Figure1", na = "NA")

figure2 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "Figure2", na = "NA")

# Shapefile for Europe (Eurostat)
# Source: http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts#nuts10

euro_spdf   <- "data/raw/Eurostat/resolution_20_mil/NUTS_RG_20M_2010.shp" %>%
  readShapePoly(proj4string = CRS("+init=epsg:4326")) %>%
  # Transform the coordinates to avoid a self-intersection error
  # Will Transform back to lat/lon after subset in next step
  spTransform(CRS("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"))

euro_df     <- fortify(euro_spdf, region = "NUTS_ID") %>%
  mutate(nuts.id = str_replace_all(string      = id, 
                                   pattern     = regex("[0-9]+"), 
                                   replacement = ""))

# Figure 1: Confessional-Battle and Witch-Trial Activity Geographically -------

# Keep only the countried where there is at least one trial or battle 
# with lat/lon data 
keep_nuts_ids <- c("AT", "DE", "CH", "FR", "BE", "UK", "LU", "ES", 
                   "NL", "IT", "DK", "SE", "PT", "CZ", "IE", "PL")

subset_planar <- euro_df %>% 
  filter(id %in% keep_nuts_ids) %>%
  data.frame()

# Change planar coordinates back to lat/lon
coordinates(subset_planar) <- c("long", "lat")
proj4string(subset_planar) <- CRS("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

subset_latlon <- subset_planar %>% 
  spTransform(CRS("+init=epsg:4326")) %>% 
  data.frame()

# Create trials and battles map
fig1 <- ggplot() + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group), 
               fill    = "white", 
               colour  = "black", data = subset_latlon) +
  coord_cartesian(xlim = c(-10.5, 25), ylim = c(35, 68)) +
  geom_point(mapping = aes(x = lon, y = lat, shape = event),
             size   = 2,
             data   = figure1) +
  scale_shape_manual(values = c(16, 1)) +
  theme_bw() +
  theme(panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.border      = element_blank(),
        axis.text         = element_blank(),
        axis.ticks        = element_blank(),
        axis.title        = element_blank(),
        legend.key        = element_blank(),
        legend.position   = "bottom",
        legend.text       = element_text(size = 16, family = "Times New Roman"),
        legend.title      = element_blank())

# Print Figure 1
print(fig1)

# # Output the plot to a png
# ggsave(plot     = fig1, 
#        filename = "reports/trials_battles_map.png", 
#        width    = 14, 
#        height   = 14, 
#        dpi      = 400)

# Figure 2: Confessional-Battle and Witch-Trial Activity Temporally -----------

# Create data frames with dates for figure shading
reformation         <- data_frame(start = 1517, end = 1555)
counter_reformation <- data_frame(start = 1555, end = 1648)

# Reshape the data from wide to long for ggplot2
figure2_long <- figure2 %>%
  select(decade, trials, deaths, -battles) %>%
  gather(series, value, -decade) %>%
  mutate(series = factor(series, levels = c("trials", "deaths")))

# Extract the battles series 
battles_by_decade <- figure2 %>% select(decade, battles)

# Top Panel: Persons tried by decade
a <- ggplot() +
  # Add shading for the two Reformation periods
  geom_rect(mapping  = aes(xmin = start, xmax = end, ymax = +Inf, ymin = 0),
            data     = reformation,
            colour   = "black",
            fill     = "grey70",
            linetype = 3,
            alpha    = .4) +
  geom_rect(mapping  = aes(xmin = start, xmax = end, ymax = +Inf, ymin = 0),
            data     = counter_reformation,
            colour   = "black",
            fill     = "grey70", 
            linetype = 3,
            alpha    = .4) +
  # Plot trials and deaths
  geom_line(mapping = aes(x = decade, y = value, linetype = series),
            data    = figure2_long,
            size    = .9,
            alpha   = .8) + 
  annotate(geom   = "text", 
           x      = 1535,
           y      = +Inf, 
           label  = "Protestant\nReformation", 
           size   = 5.5, 
           family = "Times New Roman", 
           colour = "black",
           vjust  = 1.2) +
  annotate(geom   = "text", 
           x      = 1605, 
           y      = +Inf,        
           label  = "Catholic\nCounter-Reformation", 
           size   = 5.5, 
           family = "Times New Roman", 
           colour = "black", 
           vjust  = 1.2) +
  theme_bw(base_family = "Times New Roman") + 
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels = c("Persons tried", "Deaths"),
                        guide  = "legend",
                        name   = "Trial outcome") +
  scale_x_continuous(breaks = seq(1300, 1850, by = 25), 
                     limits = c(1300, 1850)) + 
  scale_y_continuous(breaks = seq(0, 6500, by = 1000), 
                     limits = c(0, 6500)) + 
  labs(y     = "Persons tried", 
       size  = 2) +
  theme(axis.text    = element_text(family = "Times New Roman", 
                                    size   = rel(1.4)), 
        axis.title   = element_text(face   = "plain", 
                                    family = "Times New Roman", 
                                    size   = rel(1.8)), 
        plot.title   = element_text(family = "Times New Roman", 
                                    size   = rel(2)),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        axis.title.y      = element_text(vjust = 1.5),
        axis.title.x      = element_blank(),
        legend.title      = element_blank(),
        legend.position   = c(.9, .9),
        legend.key.width  = grid::unit(1.5, "cm"),
        legend.background = element_blank(),
        legend.key        = element_blank(),
        legend.text       = element_text(size = rel(1.5)))

# Top Panel: Persons tried by decade
b <- ggplot() + 
  # Add shading for the two Reformation periods
  geom_rect(mapping  = aes(xmin = start, xmax = end, ymax = +Inf, ymin = 0),
            data     = reformation,
            colour   = "black",
            fill     = "grey70",
            linetype = 3,
            alpha    = .4) + 
  geom_rect(mapping  = aes(xmin = start, xmax = end, ymax = +Inf, ymin = 0),
            data     = counter_reformation,
            colour   = "black",
            fill     = "grey70", 
            linetype = 3,
            alpha    = .4) + 
  geom_line(mapping = aes(x = decade, y = battles),
            data    = battles_by_decade,
            size    = .9) + 
  theme_bw(base_family = "Times New Roman") + 
  scale_x_continuous(breaks = seq(1300, 1850, by = 25), 
                     limits = c(1300, 1850)) + 
  labs(x     = "", # Remove Decade text 
       y     = "Confessional battles", 
       size  = 2) + 
  theme(axis.text    = element_text(family = "Times New Roman", 
                                    size   = rel(1.4)), 
        axis.title   = element_text(face   = "plain", 
                                    family = "Times New Roman", 
                                    size   = rel(1.8)), 
        plot.title   = element_text(family = "Times New Roman", 
                                    size   = rel(2)),
        axis.title.y      = element_text(vjust = 1.2),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        legend.title      = element_blank(),
        legend.position   = c(.9, .9),
        legend.key.width  = grid::unit(1.5, "cm"),
        legend.background = element_blank(),
        legend.key        = element_blank(),
        legend.text       = element_text(size = rel(1.5)))

# Plot both in the same figure 
fig2 <- arrangeGrob(a, b, ncol = 1, nrow = 2)

# Print Figure 2
plot(fig2)

# # Output the plot to a png file
# ggsave(plot     = fig1, 
#        filename = "reports/trials_battles_decade.png", 
#        width    = 18, 
#        height   = 12, 
#        dpi      = 400)
