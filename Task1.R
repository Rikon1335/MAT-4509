install.packages("RColorBrewer")
library(RColorBrewer)

# Load necessary libraries
library(readr)  # Using readr for reading CSV files
library(RColorBrewer)

# Set working directory to the new folder location
setwd("C:/Users/Home/OneDrive/Desktop/R Stdio")

# Verify if the file exists
if(file.exists("Assignment1.csv")) {
  cat("File exists!\n")
} else {
  cat("File does not exist!\n")
}

# Specify the correct data file path
data_file <- "Assignment1.csv"

# Read the CSV file
all_data <- read_csv(data_file)

# Display the data to verify it
cat("Data Loaded:\n")
print(all_data)

# Define categories and rows for maintenance and load factor
maintenance_categories <- c("labor", "materials", "third party", "burden")
fleet_category <- c("small narrowbodies", "large narrowbodies", "widebodies", "total fleet")

maintenance_rows <- c(16, 55, 94, 133)   # Maintenance rows (to be adjusted based on data)
load_factor_rows <- c(34, 73, 112, 151)  # Load factor rows (to be adjusted based on data)

# Function to extract data for a given row number
get_data_by_row <- function(row_num) {
  return(na.omit(as.numeric(all_data[row_num, -1])))  # Removes NA and extracts the numeric values
}

# Function to extract maintenance category data and sum them up
get_maintenance_category <- function(row_num) {
  labor <- get_data_by_row(row_num + 1)
  materials <- get_data_by_row(row_num + 2)
  third_party <- get_data_by_row(row_num + 3)
  burden <- get_data_by_row(row_num + 5)
  return(setNames(
    c(sum(labor), sum(materials), sum(third_party), sum(burden)),
    maintenance_categories
  ))
}

# Function to plot Load Factor Bar Plot
plot_bar <- function(data, title) {
  barplot(data,
          main = title,
          xlab = "Years",
          ylab = "Load Factor (%)",
          col = "lightblue",
          border = "red"
  )
}

# Function to plot pie chart for maintenance categories
plot_pie_chart <- function(data, fleet_type, title) {
  # Calculate percentages for the pie chart
  percentages <- round(100 * data / sum(data), 1)
  labels <- paste0(names(data), ": ", percentages, "%")
  
  pie(data, 
      labels = labels, 
      main = title,
      col = brewer.pal(4, "Set3"),  # Use a color palette
      border = "white"  # Add border to pie slices
  )
}

# Set up plotting window for pie charts (for maintenance categories)
windows(width = 1920 / 100, height = 1080 / 100) # Set window size
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))

# Generate pie charts for maintenance cost distribution for each fleet category
lapply(1:4, function(i) {
  data <- get_maintenance_category(maintenance_rows[i])
  plot_pie_chart(data, fleet_category[i], paste("Maintenance Costs for", fleet_category[i]))
})

# Add an outer title for all pie charts
mtext("Maintenance Cost Distribution", outer = TRUE, cex = 1.5)

# Reset plotting parameters to default
par(mfrow = c(1, 1))

# Set up plotting window for load factor bar plots
windows(width = 1920 / 100, height = 1080 / 100) # Set window size
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))

# Generate bar plots for Load Factor for each fleet category
lapply(1:4, function(i) {
  data <- setNames(get_data_by_row(load_factor_rows[i]), 1995:2015)  # Assign years as column names
  plot_bar(data, fleet_category[i])
})

# Add an outer title for all bar plots
mtext("Load Factor", outer = TRUE, cex = 1.5)

# Reset plotting parameters to default
par(mfrow = c(1, 1))
