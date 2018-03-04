# libraries ----
library(data.world)
library(dplyr)
library(ggplot2)
library(grid)


# data ----
# Datasets are referenced by their URL or path
dataset_key <- "https://data.world/makeovermonday/2018w9-world-economic-freedom"
# List tables available for SQL queries
tables_qry <- data.world::qry_sql("SELECT * FROM Tables")
tables_df <- data.world::query(tables_qry, dataset = dataset_key)

if (length(tables_df$tableName) > 0) {
  data_qry <- data.world::qry_sql(sprintf("SELECT * FROM `%s`", tables_df$tableName[[1]]))
  prelim_data <- data.world::query(data_qry, dataset = dataset_key)
}

# data preparation ----

dat <- prelim_data %>% 
  filter(year %in% seq(2000,2015,15)) %>%
  arrange(countries, year) %>%
  select(year, countries, summary_index)

lost_freedom <- dat %>%
  tidyr::spread(key = year, summary_index) %>%
  filter(`2000` > `2015`) %>%
  select(countries)
#  mutate(diff = `2015` - `2000`) %>%
#  arrange(diff) %>%
#  select(countries, diff)

lf_dat <- dat %>%
  filter(countries %in% lost_freedom$countries)

order_2015 <- lf_dat %>%
  filter(year == 2015) %>%
  arrange(summary_index) %>%
  select(countries)

lf_dat$countries <- factor(lf_dat$countries, levels = order_2015$countries)
lf_dat$year <- factor(lf_dat$year, levels = c("2015", "2000"))

# lf_dat$countries <- factor(lf_dat$countries, levels = lost_freedom$countries)

# plot ----

# lineplot
p <- ggplot(lf_dat, aes(countries, summary_index, group = countries)) +
  geom_line(col = "gray95", size = 2) +
  geom_point(aes(col=year), size = 2.1, alpha = 1) +
  scale_color_manual(name = "",
                     values = c("2000" = "#9e9ac8", #"#fe9929",
                                "2015" = "#54278f")) +
  coord_flip() +
  ggtitle("Economic Freedom Decline",
          subtitle = "Countries with lower indeces of economic freedom since the turn of the century. \n  ") +
  labs(y = "Freedom Index", x = "", caption = "Source: Fraser Institute") +
  theme_minimal() +
  guides(color=FALSE)+
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(hjust = 0, size = 12))
        # legend.key.height = unit(2, "mm"),
        # legend.position = "top")

# ----
png("C:/Users/alexherseg/Downloads/makeover_mondays/26FEB2018/26FEB2018.png",
    width = 8, height= 7, units = "in", res = 1200)
p
# pushViewport(viewport(x = 0.6, y = 0.9, width=0.25, height=0.05))
# grid.rect(gp=gpar(col="white"))
# popViewport()
pushViewport(viewport(x = 0.5, y = 0.9125, width = 0.25, height = 0.05))
grid.lines(x = c(0.35, 0.85), y = c(0.5, 0.5),
           gp = gpar(col = "gray90", lwd = 7))
grid.points(x = c(0.35, 0.85), y = c(0.5, 0.5), pch = 16,
            gp = gpar(col = c("#54278f", "#9e9ac8"))) # "#fe9929"
grid.text(label = c("2015", "2000"), x = c(0.10, 0.95), y = c(0.5, 0.5), 
          just = "left", 
          gp = gpar(fontsize = 10, col = "grey20"))
popViewport()
dev.off()
# ----
ggplot(lf_dat, aes(year, summary_index)) +
  geom_line(aes(group = countries))


# ----
ggsave("C:/Users/alexherseg/Downloads/makeover_mondays/26FEB2018/26FEB2018.png", 
       q, width = 8, height = 7)


# custom legend ----
draw_pop_legend <- function(x = 10, y = 5, width = 5, height = 5, fontsize = 10) {
  
  # Finish viewport() function
  pushViewport(viewport(x = x, y = y, width = width, height = height, just = "center"))
  
  legend_labels <- c("Past record high",
                     "95% CI range",
                     "Current year",
                     "Past years",
                     "Past record low")
  
  legend_position <- c(0.9, 0.7, 0.5, 0.2, 0.1)
  
  # Finish grid.text() function
  grid.text(label = legend_labels, x = 3.12, y = legend_position, 
            just = "left", 
            gp = gpar(fontsize = fontsize, col = "grey20"))
  
  # Position dots, rectangle and line
  point_position_y <- c(0.1, 0.2, 0.9)
  point_position_x <- rep(3.06, length(point_position_y))
  grid.points(x = point_position_x, y = point_position_y, pch = 16,
              gp = gpar(col = c("#0000CD", "#EED8AE", "#CD2626")))
  grid.rect(x = 0.06, y = 0.5, width = 0.06, height = 0.4,
            gp = gpar(col = NA, fill = "#8B7E66"))
  grid.lines(x = c(0.03, 0.09), y = c(0.5, 0.5),
             gp = gpar(col = "black", lwd = 3))
  
  # Add popViewport() for bookkeeping
  popViewport()
}

library(grid)
p + draw_pop_legend()
p

# ----
dat <- prelim_data %>% 
  filter(year >= 2000)

ggplot(dat, aes(year, summary_index)) +
  geom_line(aes(group = countries)) +
  theme_minimal()
