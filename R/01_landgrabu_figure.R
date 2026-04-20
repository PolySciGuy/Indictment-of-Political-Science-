# 01_landgrabu_figure.R
# Project: Indictment of Political Science
# Author: David M. Gregory
# Data: Lee (2020), Morrill Act of 1862 Indigenous Land Parcels Database
# Source: https://github.com/HCN-Digital-Projects/landgrabu-data

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(readr)
  library(scales); library(forcats)
})

unis <- read_csv(paste0(
  "https://raw.githubusercontent.com/HCN-Digital-Projects/",
  "landgrabu-data/master/",
  "Morrill_Act_of_1862_Indigenous_Land_Parcels_Database/CSVs/",
  "Universities.csv"), show_col_types = FALSE)

clean_currency <- function(x) as.numeric(gsub("[\\$,\" ]", "", x))

plot_data <- unis %>%
  mutate(us_paid = clean_currency(Adjusted_US_Paid),
         endow_raised = clean_currency(Adjusted_Endow_Raised_1914)) %>%
  filter(!is.na(us_paid), !is.na(endow_raised), endow_raised > 0, us_paid > 0) %>%
  mutate(extraction_ratio = endow_raised / us_paid,
         uni_short = gsub("University of ", "U. ", University),
         uni_short = gsub("University", "U.", uni_short)) %>%
  filter(is.finite(extraction_ratio)) %>%
  slice_max(order_by = extraction_ratio, n = 20) %>%
  mutate(uni_short = fct_reorder(uni_short, extraction_ratio))

p <- ggplot(plot_data, aes(x = extraction_ratio, y = uni_short)) +
  geom_col(fill = "#8B0000", alpha = 0.85, width = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = label_number(suffix = "x", accuracy = 1)) +
  labs(title = "Extraction Ratio: Endowment Raised vs. U.S. Payment to Indigenous Nations",
       subtitle = "Top 20 Morrill Act beneficiaries by ratio",
       x = "Endowment raised per dollar paid to tribes", y = NULL,
       caption = "Data: Lee (2020), Morrill Act of 1862 Indigenous Land Parcels Database.") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())

ggsave("figures/fig_extraction_ratio.png", plot = p, width = 8, height = 7, dpi = 300)
cat("Figure saved\n")

