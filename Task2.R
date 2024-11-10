# Load necessary libraries
library(readr)  # Using readr for reading CSV files
library(RColorBrewer)

# Set working directory to the location of your CSV file
setwd("C:/Users/Home/OneDrive/Desktop/R Stdio")

# Read the CSV file
data_file <- "Assignment1.csv"
all_data <- read_csv(data_file)

# Check the first few rows of the data to verify it loaded correctly
head(all_data)

# Define maintenance categories and years
maintenance_categories <- c("labor", "materials", "third party", "burden")
years <- 1995:2015

# Function to extract data for a given row number (Maintenance/Load Factor)
get_data_by_row <- function(row_num) {
  return(na.omit(as.numeric(all_data[row_num, -1])))
}

get_maintenace_category <- function(row_num) {
  labor <- get_data_by_row(row_num + 1)
  materials <- get_data_by_row(row_num + 2)
  third_party <- get_data_by_row(row_num + 3)
  burden <- get_data_by_row(row_num + 5)
  return(setNames(
    c(sum(labor), sum(materials), sum(third_party), sum(burden)),
    maintenance_categories
  ))
}

# Plotting Load Factor bar plot
plot_bar <- function(data, title) {
  barplot(data,
          main = title,
          xlab = "Years",
          ylab = "Load Factor (%)",
          col = "lightblue",
          border = "red"
  )
}

# Maintenance and Load Factor row numbers
maintenance_rows <- c(16, 55, 94, 133)
load_factor_rows <- c(34, 73, 112, 151)

fleet_category <- c(
  "small narrowbodies",
  "large narrowbodies",
  "widebodies",
  "total fleet"
)

# Pie chart for maintenance
windows(width = 1920 / 100, height = 1080 / 100) # Set window size
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))

# Define a color palette
colors <- brewer.pal(4, "Set3")  # Using RColorBrewer for a set of 4 distinct colors

# Create pie charts for each maintenance category with enhancements
lapply(1:4, function(i) {
  data <- get_maintenace_category(maintenance_rows[i])
  
  # Calculate percentages
  percentages <- round(100 * data / sum(data), 1)
  
  # Create labels with category names and percentages
  labels <- paste0(names(data), ": ", percentages, "%")
  
  # Create pie chart
  pie(data, 
      labels = labels,        # Use labels with percentages
      main = paste("Maintenance Costs for", fleet_category[i]),  # Descriptive title
      col = colors,          # Set colors for slices
      border = "white")      # Add border to slices
})

# Add an outer title for all pie charts
mtext("Maintenance Cost Distribution", outer = TRUE, cex = 1.5)

# Reset plotting parameters to default
par(mfrow = c(1, 1))

# Bar chart for Load Factor
windows(width = 1920 / 100, height = 1080 / 100) # Set window size
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))

# Plot Load Factor data for each fleet category
lapply(1:4, function(i) {
  data <- setNames(get_data_by_row(load_factor_rows[i]), years)
  plot_bar(data, fleet_category[i])
})

# Add an outer title for all bar charts
mtext("Load Factor", outer = TRUE, cex = 1.5)

# Reset plotting parameters to default
par(mfrow = c(1, 1))
