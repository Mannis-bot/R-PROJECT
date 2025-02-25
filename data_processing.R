# data_processing.R
# Load required libraries
library(nycflights13)
library(data.table)

# Load the flights dataset and convert to data.table
data("flights", package = "nycflights13")
flights_dt <- as.data.table(flights)

# 1. Compute average departure delay for each airline
avg_delay <- flights_dt[, .(avg_dep_delay = mean(dep_delay, na.rm = TRUE)), by = carrier]

# 2. Find top 5 destinations with the most flights
top_dests <- flights_dt[, .N, by = dest][order(-N)][1:5]

# 3. Add new columns: unique ID and delay flag
flights_dt[, flight_id := .I]  # Unique ID for each row
flights_dt[, is_delayed := ifelse(dep_delay > 15, TRUE, FALSE)]

# Replace NA in is_delayed with FALSE
flights_dt[is.na(is_delayed), is_delayed := FALSE]

# Preview results
head(flights_dt)
print(avg_delay)
print(top_dests)

# 4. Save processed data as CSV in the project folder
fwrite(flights_dt, "processed_flights.csv")
