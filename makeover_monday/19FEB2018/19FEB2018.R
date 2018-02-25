# libraries ----
library(data.world)
library(dplyr)
library(ggplot2)
library(treemapify)

# data ----

# Datasets are referenced by their URL or path
dataset_key <- "https://data.world/makeovermonday/2018w8-where-does-your-medicine-come-from"
# List tables available for SQL queries
tables_qry <- data.world::qry_sql("SELECT * FROM Tables")
tables_df <- data.world::query(tables_qry, dataset = dataset_key)

if (length(tables_df$tableName) > 0) {
  sample_qry <- data.world::qry_sql(sprintf("SELECT * FROM `%s`", tables_df$tableName[[1]]))
  sample_df <- data.world::query(sample_qry, dataset = dataset_key)
  sample_df
}

prelim_data <- sample_df %>%
  filter(exporter != "World", year == 2016) %>%
  arrange(desc(exports_usd)) 

my_top_n <- 12
#other <- data.frame(exporter = "All Other Countries Combined", year = 2016, exports_usd = sum(prelim_data$exports_usd[(my_top_n+1):219], na.rm = TRUE))
#data <- rbind(prelim_data[1:my_top_n,], other)

# eu <- read.csv("../Downloads/makeover_mondays/19FEB2018/EU.csv")
# data$eu <- ifelse(data$exporter %in% eu$Countries.in.EU, "EU", "Not")

nrows <- nrow(prelim_data)
# data$exporter <- paste0(data$exporter, ": $", round(data$exports_usd/1000000000, 1), "b")
prelim_data$top_countries <- c(rep("Top", my_top_n), rep("Not", nrows-my_top_n))
# prelim_data$country_usd <- paste0(prelim_data$exporter, " \n", "$", round(prelim_data$exports_usd/1000000000, 1), "B #(", 1:nrows, ")")
prelim_data$country <- prelim_data$exporter
prelim_data$country[(my_top_n+1):nrows] <- ""
prelim_data$usd <- paste0("$", round(prelim_data$exports_usd/1000000000, 1), "B (#", 1:nrows, ")")
prelim_data$usd[(my_top_n+1):nrows] <- ""

# ----
p <- ggplot(prelim_data, aes(area = exports_usd, label = country, subgroup = top_countries)) +
  geom_treemap(aes(fill = top_countries), col = "white") +
  scale_fill_manual(values = c("Top" = "#ce1256", "Not" = "gray70")) +
  geom_treemap_text(color = "white") +
  geom_treemap_text(aes(label = usd), size = 10, color = "white", padding.y = grid::unit(7, "mm")) +
  ggtitle("Major Players in the Global Drug Export Market", 
          subtitle = "In 2016, total drugs and medicine exports totaled $319B USD, with 12 countries responsible for just over 80% of total value.") +
  labs(caption = "Source: TradeMap") +
  guides(fill = FALSE) +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(hjust = 0, size = 12))

ggsave("C:/Users/alexherseg/Documents/blog/alexherseg.github.io/makeover_monday/19FEB2018/19FEB2018.png", 
       p, width = 10, height = 6)
# ----  
  geom_point(stat = "identity", aes(col = year)) +
  coord_flip()