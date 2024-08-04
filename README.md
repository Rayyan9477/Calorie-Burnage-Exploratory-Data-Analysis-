# Calorie Burnage Data Analysis

## Overview
This project involves analyzing the Calorie Burnage dataset. It covers several key data science concepts, including data cleaning, summary statistics calculation, outlier detection, data visualization, and linear regression modeling. This README provides an extensive overview of the techniques and methods used in this analysis.

## Table of Contents
1. [Loading the Dataset](#loading-the-dataset)
2. [Summary Statistics](#summary-statistics)
3. [Outlier Detection](#outlier-detection)
4. [Data Visualization](#data-visualization)
5. [Linear Regression Model](#linear-regression-model)
6. [Conclusion](#conclusion)
7. [Author](#author)

## Loading the Dataset
The dataset is loaded using the `read.csv` function in R.

```R
calorie_burnage <- read.csv("C:/Users/rehan/Downloads/Documents/Calorie_Burnage.csv")
```

## Summary Statistics
Summary statistics provide a quick overview of the dataset's central tendency and variability. We calculate the mean, median, mode, and quartiles (Q1 and Q3) for each variable.

```R
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

summary_stats_df <- as.data.frame(t(sapply(summary_stats, unlist)))
colnames(summary_stats_df) <- c("Mean", "Median", "Mode", "Q1", "Q3")
print(summary_stats_df)
```

## Outlier Detection
Outliers can significantly affect the analysis and modeling of data. We use the interquartile range (IQR) method to detect outliers.

```R
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_fence <- Q1 - 1.5 * IQR
  upper_fence <- Q3 + 1.5 * IQR
  outliers <- x[x < lower_fence | x > upper_fence]
  return(outliers)
}

outliers <- list(
  Duration = identify_outliers(calorie_burnage$Duration),
  Average_Pulse = identify_outliers(calorie_burnage$Average_Pulse),
  Max_Pulse = identify_outliers(calorie_burnage$Max_Pulse),
  Calorie_Burnage = identify_outliers(calorie_burnage$Calorie_Burnage),
  Hours_Work = identify_outliers(calorie_burnage$Hours_Work),
  Hours_Sleep = identify_outliers(calorie_burnage$Hours_Sleep)
)

print("Outliers:")
print(outliers)
```

## Data Visualization
Data visualization helps in understanding the distribution and relationships within the dataset. We use box plots to visualize the distribution of each variable and scatter plot matrices to explore relationships between variables.

### Box and Whisker Plots
```R
boxplot(calorie_burnage$Duration, calorie_burnage$Average_Pulse, calorie_burnage$Max_Pulse, calorie_burnage$Calorie_Burnage, calorie_burnage$Hours_Work, calorie_burnage$Hours_Sleep,
        main = "Box and Whisker Plots of Calorie Burnage Dataset",
        names = c("Duration", "Average Pulse", "Max Pulse", "Calorie Burnage", "Hours Work", "Hours Sleep"),
        col = c("red", "blue", "green", "orange", "purple", "brown"),
        outline = TRUE)
```

### Scatter Plot Matrix
```R
install.packages("GGally")
library(GGally)
ggpairs(calorie_burnage, title = "Scatter Plot Matrix of Calorie Burnage Dataset")
```

## Linear Regression Model
Linear regression is used to understand the relationship between the dependent variable (Calorie Burnage) and independent variables (Average Pulse, Hours Work, and Hours Sleep).

```R
library(readr)
Calorie_Burnage <- read.csv("C:/Users/rehan/Downloads/Documents/Calorie_Burnage.csv")
model <- lm(Calorie_Burnage ~ Average_Pulse + Hours_Work + Hours_Sleep, data = Calorie_Burnage)
summary(model)
```

## Conclusion
This analysis covered the essential steps in data science, from data loading and summary statistics to outlier detection, data visualization, and building a linear regression model. These techniques provide a solid foundation for understanding and analyzing datasets.

## Author
Rayyan Ahmed  
[LinkedIn](https://www.linkedin.com/in/rayyan-ahmed9477/)

Feel free to connect with me on LinkedIn for any questions or collaborations.
