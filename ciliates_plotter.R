library(ggplot2)
library(tidyverse)

# Read the data from the CSV file
temperature <- 30
data <- read.csv(paste(temperature,".csv",sep=''))

num_data = data[,1:5]
colnames(num_data)<-c(0,5,10,20,30)

size_data = data[,6:10]
colnames(size_data)<-c(0,5,10,20,30)

# Reshape the data for ggplot for number of vacuoles
num_data_long <- num_data %>%
  gather(key = "Time", value = "Number_of_Vacuoles") %>%
  mutate(Time = factor(Time, levels = c("0", "5", "10", "20", "30")))

# Create a box plot for number of vacuoles
ggplot(num_data_long, aes(x = Time, y = Number_of_Vacuoles)) +
  geom_boxplot() +
  labs(title = paste("Number of Vacuoles Over Time at",temperature,"°C"),
       x = "Time (minutes)",
       y = "Number of Vacuoles")

# Reshape the data for ggplot for size of vacuoles
size_data_long <- size_data %>%
  gather(key = "Time", value = "Size_of_Vacuoles") %>%
  mutate(Time = factor(Time, levels = c("0", "5", "10", "20", "30")))

# Create a box plot for size of vacuoles
ggplot(size_data_long, aes(x = Time, y = Size_of_Vacuoles)) +
  geom_boxplot() +
  labs(title = paste("Evolution of Vacuole Size at",temperature,"°C"),
       x = "Time (minutes)",
       y = "Size of Vacuoles (μm)")
# Dot plot
ggplot(size_data_long, aes(x = Time, y = Size_of_Vacuoles)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  labs(title = paste("Evolution of Vacuole Size at",temperature,"°C"),
       x = "Time (minutes)",
       y = "Size of Vacuoles")

# ------------------------------------------
#Speed

# Create a data frame with temperature and speed values
temperature <- c(rep("5°C", 5), rep("20°C", 5), rep("30°C", 5))
speed <- c(
  65.54494766, 149.9839762, 220.7115535, 192.7940457, 165.6804299,
  228.9890923, 222.5978796, 284.5970756, 200.2508421, 286.8885903,
  628.3564553, 440.7826098, 587.3381716, 514.1040217, 681.5467876
)

speed_data <- data.frame(Temperature = factor(temperature, levels = c("5°C", "20°C", "30°C")), Speed = speed)

# Create a box plot
ggplot(speed_data, aes(x = Temperature, y = Speed)) +
  geom_boxplot() +
  labs(title = "Mean Ciliate Speed vs Temperature",
       x = "Temperature (°C)",
       y = "Speed (µm/s)")
  
# ------------------------------------
# Phagocytosis plot superposition

library(ggplot2)
library(tidyverse)

# List all the temperature values you have (e.g., 5, 10, 15, 20, 25, 30)
temperatures <- c(5, 20, 30)

# Create empty lists to store data frames
num_data_list <- list()
size_data_list <- list()

# Loop through each temperature
for (temperature in temperatures) {
  # Read the data from the CSV file
  data <- read.csv(paste(temperature, ".csv", sep=''))
  
  # Extract relevant columns for number of vacuoles
  num_data <- data[, 1:5]
  colnames(num_data) <- c(0, 5, 10, 20, 30)
  
  # Extract relevant columns for size of vacuoles
  size_data <- data[, 6:10]
  colnames(size_data) <- c(0, 5, 10, 20, 30)
  
  # Reshape data for number of vacuoles
  num_data_long <- num_data %>%
    gather(key = "Time", value = "Number_of_Vacuoles") %>%
    mutate(Time = factor(Time, levels = c("0", "5", "10", "20", "30")))
  
  # Reshape data for size of vacuoles
  size_data_long <- size_data %>%
    gather(key = "Time", value = "Size_of_Vacuoles") %>%
    mutate(Time = factor(Time, levels = c("0", "5", "10", "20", "30")))
  
  # Append data frames to lists
  num_data_list[[as.character(temperature)]] <- num_data_long
  size_data_list[[as.character(temperature)]] <- size_data_long
}

# Combine data frames from the lists
combined_num_data <- bind_rows(num_data_list, .id = "Temperature")
combined_size_data <- bind_rows(size_data_list, .id = "Temperature")

# Manually set the order of the factor levels for Temperature
temperature_levels <- as.character(temperatures)

combined_num_data$Temperature <- factor(
  combined_num_data$Temperature,
  levels = temperature_levels
)

combined_size_data$Temperature <- factor(
  combined_size_data$Temperature,
  levels = temperature_levels
)

# Define the color palette for each temperature
temperature_colors <- colorRampPalette(c("darkgreen", "lightgreen"))(length(temperatures))

# Create a box plot for number of vacuoles with manual color palette and legend label
ggplot(combined_num_data, aes(x = Time, y = Number_of_Vacuoles, fill = Temperature)) +
  geom_boxplot() +
  labs(title = "Number of Vacuoles Over Time Under Various Temperatures",
       x = "Time (minutes)",
       y = "Number of Vacuoles",
       fill = "Temperature (°Celsius)") +  # Include the unit in the legend label
  scale_fill_manual(values = temperature_colors)

# Create a box plot for size of vacuoles with manual color palette and legend label
ggplot(combined_size_data, aes(x = Time, y = Size_of_Vacuoles, fill = Temperature)) +
  geom_boxplot() +
  labs(title = "Evolution of Vacuole Size Under Various Temperatures",
       x = "Time (minutes)",
       y = "Size of Vacuoles (μm)",
       fill = "Temperature (°Celsius)") +  # Include the unit in the legend label
  scale_fill_manual(values = temperature_colors)
