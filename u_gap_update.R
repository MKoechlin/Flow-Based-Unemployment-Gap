##################################################################

#' Code related to Enisse Kharroubi & Marius Koechlin 2025: Labour market floes and the Phillips curve
#' Code uuthor Marius Koechlin
#' 
#' This code file updates the unemployment gap measure introduce in the paper mentioned above.
#' 
#' Last updated: 05.06.2025



# Set-up ------------------------------------------------------------------
# Delete all variables
rm(list = ls())

# Set language to English
Sys.setenv(LANG = "en")

# Load packages
library("dplyr")         # Code structure
library("tidyr")         # Data treatment
library("ggplot2")       # Graphs
library("readxl")        # To load excel files into R
library("blsR")          # To load data from BLS
library("lubridate")     # Work with dates
library("zoo")           # Work with dates

# Set your API key (if used) (can be set-up here: https://www.bls.gov/developers/)
bls_set_key(
  # REPLACE WITH YOUR BLS API KEY !
  bls_api_key <- Sys.getenv("BLS_API")
)

# Load data ---------------------------------------------------------------

## Labour Market Flows
#' The data can be downloaded here: https://fred.stlouisfed.org/release/tables?rid=334&eid=1147#snid=1189
#' Or an API can be used (either from FRED or BLS). 

# Data on the data series names
bls_lfs_id <- read.csv("rawdata/bls_lfs_id.csv")

# Load the data using the BLS API
bls_lfs_data <- get_n_series_table(series_ids = bls_lfs_id$series_id, api_key = bls_get_key(), 
                   start_year = 1990, end_year = lubridate::year(today()),
                   parse_values = TRUE) 

# Clean the data
bls_lfs_data <- bls_lfs_data %>%
  mutate(month = as.numeric(gsub("M", "", period)),
         date = as.Date(paste0(year, "-", month, "-01"))) %>%
  relocate(year, date, month) %>% dplyr::select(-c(year, period, month)) %>%
  pivot_longer(- date, names_to = "series_id", values_to = "flow_ths_pers")

# Join data with series names
bls_lfs_data <- bls_lfs_data %>%
  left_join(bls_lfs_id, join_by("series_id"))


## Stock unemployment rate
#' The data can be downloaded from the BLS website: https://www.bls.gov/cps/lfcharacteristics.htm
#' Or an API can be used (either from FRED or BLS).

# Data on the data series names
bls_lm_id <- read.csv("rawdata/bls_lm_id.csv")

# Load the data using the BLS API
bls_lm_data <- get_n_series_table(series_ids = bls_lm_id$series_id, api_key = bls_get_key(), 
                                   start_year = 1990, end_year = lubridate::year(today()),
                                   parse_values = TRUE) 

# Clean the data
bls_lm_data <- bls_lm_data %>%
  mutate(month = as.numeric(gsub("M", "", period)),
         date = as.Date(paste0(year, "-", month, "-01"))) %>%
  relocate(year, date, month) %>% dplyr::select(-c(year, period, month)) %>%
  pivot_longer(- date, names_to = "series_id", values_to = "value")

# Join data with series names
bls_lm_data <- bls_lm_data %>%
  left_join(bls_lm_id, join_by("series_id"))


# Data treatment ----------------------------------------------------------
## Labour Market Flows
# Prepare the flows and calculate the monthly transition probabilities
bls_lfs_data <- bls_lfs_data %>%
  group_by(date, t0) %>%
  mutate(total_flow = sum(flow_ths_pers)) %>% ungroup() %>%
  mutate(trans_prob = flow_ths_pers / total_flow) %>%
  dplyr::select(date, t0, t1, transition, flow_ths_pers, total_flow, trans_prob)

## Stock unemployment rate
# Adjust dataset & calculate the unemployment rate as a share of the working age population
bls_lm_data <- bls_lm_data %>%
  dplyr::select(date, series_name_short, value) %>%
  pivot_wider(names_from = series_name_short, values_from = value) %>%
  mutate(u_rate = unemployed / population * 100)



# Calculate the flow-based unemployment rate ------------------------------
# Prepare dataframe to store results
flow_u_data <- tibble(date = numeric(), flow_u = numeric())

# Start loop
for (date_t in unique(bls_lfs_data$date)) {
  
  # Prepare p matrix
  p_matrix <- bls_lfs_data %>% 
    dplyr::select(c(date, transition, t0, t1, trans_prob)) %>%
    filter(date == date_t) %>% dplyr::select(-c(date, transition)) %>%
    pivot_wider(names_from = t1, values_from = trans_prob) %>% 
    column_to_rownames(var = "t0") %>% as.matrix()
  
  # Calculate q matrix
  q_matrix <- matrix(
    c(p_matrix["E", "E"] - p_matrix["I", "E"],
      p_matrix["E", "U"] - p_matrix["I", "U"],
      p_matrix["U", "E"] - p_matrix["I", "E"],
      p_matrix["U", "U"] - p_matrix["I", "U"]),
    nrow = 2, ncol = 2, byrow = TRUE)
  rownames(q_matrix) <- c("E", "U")
  colnames(q_matrix) <- c("E", "U")
  
  # Calculate the flow based unemployment rate
  flow_u <- ((1 - q_matrix["E", "E"]) * p_matrix["I", "U"] + p_matrix["I", "E"] * q_matrix["E", "U"]) /
    ((1 - q_matrix["E", "E"]) * (1 - q_matrix["U", "U"]) -  q_matrix["U", "E"] * q_matrix["E", "U"])
  
  # Add to main dataframe
  flow_u_data <- bind_rows(flow_u_data, tibble(date = date_t, flow_u = flow_u))
  
}

# Final adjustments
flow_u_data <- flow_u_data %>%
  mutate(date = as.Date(date),
         flow_u = flow_u * 100)


# Combine the data & calculate the gap ------------------------------------
# Combine data
final_data <- 
  bls_lm_data %>% dplyr::select(date, u_rate) %>%
  left_join(flow_u_data, by = "date") 

# Calculate the unemployment gap
final_data <- final_data %>%
  mutate(u_gap = flow_u - u_rate) %>%
  # Calculate the 6-month moving average
  mutate(across(c(u_rate, flow_u, u_gap), ~ rollmean(., 6, fill = NA, align = "right", na.rm = TRUE), .names = "{col}_ma6"))


# Save the dataset --------------------------------------------------------
# Save as CSV
write.csv(final_data %>% dplyr::select(date, u_gap_ma6) %>% rename(u_gap = u_gap_ma6),
          file = "output/Kharroubi_Koechlin_u_gap_data.csv", row.names = FALSE)


# Some analysis -----------------------------------------------------------

# Plot the stock-based & flow-based rate
final_data %>%
  ggplot(aes(x = date)) +
  geom_rect(xmin = as.Date("1990-08-01"), xmax = as.Date("1991-03-31"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2008-01-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-04-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_line(aes(y = u_rate_ma6, color = "Stock-based unemployment rate")) +
  geom_line(aes(y = flow_u_ma6, color = "Flow-based unemployment rate")) +
  scale_color_manual(values = c("Stock-based unemployment rate" = "darkblue", 
                                 "Flow-based unemployment rate" = "darkred"),
                     name = "") +
  labs(title = "Flow-, and stock-based unemployment",
       x = "Date", y = "Percentage") +
  theme_minimal() + theme(legend.position = "bottom")
# Save the plot
ggsave("output/stock_flow_u_rate.png", width = 22, height = 15, units = "cm")

# Plot the unemployment gap
final_data %>%
  ggplot(aes(x = date)) +
  geom_rect(xmin = as.Date("1990-08-01"), xmax = as.Date("1991-03-31"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2008-01-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-04-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_line(aes(y = u_gap_ma6)) +
  # Fill negative and positive area
  geom_ribbon(aes(ymin = pmin((u_gap_ma6), 0), ymax = 0), fill = "#aa332f", alpha = 0.3) +
  geom_ribbon(aes(ymin = 0, ymax = pmax((u_gap_ma6), 0)), fill = "green3", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  labs(title = "Unemployment Gap (Flow-based - Stock-based)",
       x = "Date", y = "Percentage points") +
  theme_minimal()
ggsave("output/u_gap.png", width = 22, height = 15, units = "cm")

# Summary statistics of the gap
final_data %>%
  summarise(mean_gap = mean(u_gap_ma6, na.rm = TRUE),
            sd_gap = sd(u_gap_ma6, na.rm = TRUE),
            min_gap = min(u_gap_ma6, na.rm = TRUE),
            max_gap = max(u_gap_ma6, na.rm = TRUE),
            median_gap = median(u_gap_ma6, na.rm = TRUE))

# Development in the last three month of the gap
final_data %>%
  filter(date >= max(date) - months(3)) %>%
  summarise(mean_gap = mean(u_gap_ma6, na.rm = TRUE),
            sd_gap = sd(u_gap_ma6, na.rm = TRUE),
            min_gap = min(u_gap_ma6, na.rm = TRUE),
            max_gap = max(u_gap_ma6, na.rm = TRUE),
            median_gap = median(u_gap_ma6, na.rm = TRUE))



############################### END ##############################
##################################################################
