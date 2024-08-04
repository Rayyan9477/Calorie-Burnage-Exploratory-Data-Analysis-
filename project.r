# Load the dataset
calorie_burnage <- read.csv("C:/Users/rehan/Downloads/Documents/Calorie_Burnage.csv")


# --------------------------------Task 2-------------------------------
# Calculate summary statistics for each variable
summary_stats <- lapply(calorie_burnage, function(x) {
  stats <- c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mode = as.numeric(names(sort(table(x), decreasing = TRUE)[1])),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE)
  )
  return(stats)
})

# Convert the summary statistics to a data frame for better visualization
summary_stats_df <- as.data.frame(t(sapply(summary_stats, unlist)))

# Add variable names to the data frame
colnames(summary_stats_df) <- c("Mean", "Median", "Mode", "Q1", "Q3")

# Display the summary statistics for each variable
summary_stats_df

# ---------------------------------- Task 3 --------------------------------

# Create a function to identify outliers
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_fence <- Q1 - 1.5 * IQR
  upper_fence <- Q3 + 1.5 * IQR
  outliers <- x[x < lower_fence | x > upper_fence]
  return(outliers)
}

# Create a box and whisker plot with different colors for each variable
boxplot(calorie_burnage$Duration, calorie_burnage$Average_Pulse, calorie_burnage$Max_Pulse, calorie_burnage$Calorie_Burnage, calorie_burnage$Hours_Work, calorie_burnage$Hours_Sleep,
        main = "Box and Whisker Plots of Calorie Burnage Dataset",
        names = c("Duration", "Average Pulse", "Max Pulse", "Calorie Burnage", "Hours Work", "Hours Sleep"),
        col = c("red", "blue", "green", "orange", "purple", "brown"),
        outline = TRUE)

# Identify outliers for each variable
outliers <- list(
  Duration = identify_outliers(calorie_burnage$Duration),
  Average_Pulse = identify_outliers(calorie_burnage$Average_Pulse),
  Max_Pulse = identify_outliers(calorie_burnage$Max_Pulse),
  Calorie_Burnage = identify_outliers(calorie_burnage$Calorie_Burnage),
  Hours_Work = identify_outliers(calorie_burnage$Hours_Work),
  Hours_Sleep = identify_outliers(calorie_burnage$Hours_Sleep)
)

# Print the outliers for each variable
print("Outliers:")
print(outliers)

# ------------------------------- Task 4 ------------------------------
# Install and load the GGally package for scatter plot matrix
install.packages("GGally")
library(GGally)

# Create a scatter plot matrix for all variables
ggpairs(calorie_burnage, title = "Scatter Plot Matrix of Calorie Burnage Dataset")

# ----------------------------- Task 5 -----------------------------
library(readr)
Calorie_Burnage <- read.csv("C:/Users/rehan/Downloads/Documents/Calorie_Burnage.csv")
model <- lm(`Calorie_Burnage` ~ `Average_Pulse` + `Hours_Work` + `Hours_Sleep`, data = Calorie_Burnage)
summary(model)