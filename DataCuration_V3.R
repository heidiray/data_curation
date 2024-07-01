rm(list = ls())

library(readstata13)
library(readxl)
library(tidyverse)
library(tidyverse)
library(directlabels)
library(maps)
library(rnaturalearthdata)
library(sf)
library(ggridges)
library(ggthemes)
library(scales)
getwd()
setwd("/Users/heidiray/Downloads")
list.files()

arrests_csv <- read.csv("NYPDArrests.csv")

arrests_csv$ARREST_BORO[arrests_csv$ARREST_BORO == 'B'] <- 'BRONX'
arrests_csv$ARREST_BORO[arrests_csv$ARREST_BORO == 'S'] <- 'STATEN ISLAND'
arrests_csv$ARREST_BORO[arrests_csv$ARREST_BORO == 'K'] <- 'BROOKLYN'
arrests_csv$ARREST_BORO[arrests_csv$ARREST_BORO == 'M'] <- 'MANHATTAN'
arrests_csv$ARREST_BORO[arrests_csv$ARREST_BORO == 'Q'] <- 'QUEENS'


head(arrests_csv)


# Rename common variables to a consistent name in each dataset
colnames(arrests_csv)[which(names(arrests_csv) == "ARREST_BORO")] <- "NY_Borough"



arrests_csv <- arrests_csv[, -which(names(arrests_csv) %in% c("ARREST_KEY", "PC_CD", "PD_DESC", "KY_CD", "OFNS_DESC", "LAW_CODE", "LAW_CAT_CD", "ARREST_PRECINCT", "JURISDICTION_CODE", "X_COORD_CD", "Y_COORD_CD", "Latitude", "Longitude", "New Georeferenced Column"))]


subset_arrests <- arrests_csv[1:20000, ]


# Renaming columns

colnames(subset_arrests)[which(names(subset_arrests) == "ARREST_BORO")] <- "NY_Borough"


View(subset_arrests)

# Import required library
library(dplyr)


subset_arrests <- subset_arrests %>%
  mutate(month = as.integer(substring(ARREST_DATE, 1, 2)),
         day = as.integer(substring(ARREST_DATE, 4, 5)),
         year = as.integer(substring(ARREST_DATE, 7, 10))) %>%
  select(-ARREST_DATE)

# Group by required columns and the new month, day, and year columns
grouped_arrests4 <- subset_arrests %>%
  group_by(NY_Borough, PERP_SEX, AGE_GROUP, PERP_RACE, month, day, year)

# Summarize data within each group
arrests_summary <- grouped_arrests4 %>%
  summarise(arrest_count = n())  # Count the number of rows in each group

# View the summarized data
View(arrests_summary)

# Create a sample dataset for demonstration
borough_population <- data.frame(
  NY_Borough = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"),
  Population = c(1472654, 2736074, 1694251, 2405464, 495747))

names(borough_population)[names(borough_population) == "NY_Borough"] <- "NY_Borough"


merged_data <- merge(arrests_summary, borough_population, by = "NY_Borough")

# View the merged data
View(merged_data)



borough_race_populationWhite <- data.frame(
  NY_Borough = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
  Percent_White = c(10.9, 34.7, 45.8, 32.9, 71.3))
borough_race_populationBlack <- data.frame(
  NY_Borough = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
  Percent_Black = c(31.2, 34.4, 15.3, 19, 8.9))
borough_race_populationAPI <- data.frame(
  NY_Borough = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
  Percent_API = c(2.9, 7.5, 9.4, 17.5, 5.6))
borough_race_populationHispanic <- data.frame(
  NY_Borough = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
  Percent_Hispanic = c(48.4, 19.8, 27.2, 25, 12.1))
borough_race_populationOther <- data.frame(
  NY_Borough = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
  Percent_Other = c(2.9, 3.6, 2.4, 5.6, 2.1))

# Convert borough names in borough_population to uppercase
borough_race_populationWhite$NY_Borough <- toupper(borough_race_populationWhite$NY_Borough)
names(borough_race_populationWhite)[names(borough_race_populationWhite) == "NY_Borough"] <- "NY_Borough"
# Merge the arrests_summary with borough_population
merged_data <- merge(merged_data, borough_race_populationWhite, by = "NY_Borough")

View(merged_data)

library(dplyr)

# Convert 'NY Borough' values to uppercase in both datasets

merged_data$NY_Borough <- toupper(merged_data$NY_Borough)
borough_race_populationBlack$NY_Borough <- toupper(borough_race_populationBlack$NY_Borough)

merged_data <- merge(merged_data, borough_race_populationBlack, by = "NY_Borough")


merged_data$NY_Borough <- toupper(merged_data$NY_Borough)
borough_race_populationAPI$NY_Borough <- toupper(borough_race_populationAPI$NY_Borough)

merged_data <- merge(merged_data, borough_race_populationAPI, by = "NY_Borough")



borough_race_populationHispanic$NY_Borough <- toupper(borough_race_populationHispanic$NY_Borough)

merged_data <- merge(merged_data, borough_race_populationHispanic, by = "NY_Borough")

borough_race_populationOther$NY_Borough <- toupper(borough_race_populationOther$NY_Borough)

Final_merged_data <- merge(merged_data, borough_race_populationOther, by = "NY_Borough")

View(Final_merged_data)
head(Final_merged_data)

write.csv(Final_merged_data, "DataCurationProj.csv", row.names = FALSE)

#Data Visualization
library(ggplot2)

# Assuming your dataset is named merged_data2
# If your dataset name is different, replace merged_data2 with your dataset name

# Convert 'NY Borough' and 'PERP_RACE' to factors
Final_merged_data$NY_Borough <- as.factor(Final_merged_data$NY_Borough)
Final_merged_data$PERP_RACE <- as.factor(Final_merged_data$PERP_RACE)

custom_colors <- c("darkorchid", "lightpink", "plum2", "palevioletred1", "violetred", "lightslateblue", "maroon1")

# Create the bar plot with custom colors
ggplot(Final_merged_data, aes(x = NY_Borough, fill = PERP_RACE)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = custom_colors) +  # Specify custom colors
  labs(title = "Arrest Counts by NY Borough and Perpetrator Race",
       x = "NY Borough", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Merge all data frames into a single data frame
merged_data <- merge(borough_race_populationWhite, borough_race_populationBlack, by = "NY_Borough")
merged_data <- merge(merged_data, borough_race_populationAPI, by = "NY_Borough")
merged_data <- merge(merged_data, borough_race_populationHispanic, by = "NY_Borough")
merged_data <- merge(merged_data, borough_race_populationOther, by = "NY_Borough")

# Set row names to NY_Borough column
rownames(merged_data) <- merged_data$NY_Borough
merged_data$NY_Borough <- NULL

# Transpose the data frame
merged_data <- t(merged_data)

# Plot
barplot(as.matrix(merged_data), beside = TRUE, 
        col = rainbow(ncol(merged_data)), 
        legend.text = rownames(merged_data),
        args.legend = list(x = "topright", bty = "n", cex = 0.7),
        main = "Distribution of Races in Each Borough",
        xlab = "Borough",
        ylab = "Percentage")
# Merge all data frames into a single data frame
merged_data <- merge(borough_race_populationWhite, borough_race_populationBlack, by = "NY_Borough")
merged_data <- merge(merged_data, borough_race_populationAPI, by = "NY_Borough")
merged_data <- merge(merged_data, borough_race_populationHispanic, by = "NY_Borough")
merged_data <- merge(merged_data, borough_race_populationOther, by = "NY_Borough")

# Set row names to NY_Borough column
rownames(merged_data) <- merged_data$NY_Borough
merged_data$NY_Borough <- NULL

library(ggplot2)

# Merge all data frames into a single data frame
merged_data <- merge(borough_race_populationWhite, borough_race_populationBlack, by = "NY_Borough")
merged_data <- merge(merged_data, borough_race_populationAPI, by = "NY_Borough")
merged_data <- merge(merged_data, borough_race_populationHispanic, by = "NY_Borough")
merged_data <- merge(merged_data, borough_race_populationOther, by = "NY_Borough")

# Plot the barplot
barplot(as.matrix(merged_data), 
        beside = FALSE, 
        col = rainbow(ncol(merged_data)),
        main = "Distribution of Races in Each Borough",
        xlab = "Borough",
        ylab = "Percentage",
        ylim = c(0, 100),
        border = NA,
        space = 0.1)

# Move legend to the upper right corner inside the plot
legend("topright", 
       legend = rownames(merged_data), 
       fill = rainbow(ncol(merged_data)), 
       bty = "n",
       cex = 0.7,
       x.intersp = 0.5,
       y.intersp = 0.5)

ggplot(merged_data, aes(x = NY_Borough, fill = PERP_RACE)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = custom_colors) +  # Specify custom colors
  labs(title = "Arrest Counts by NY Borough and Perpetrator Race",
       x = "NY Borough", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
head(merged_df)

# Load required library
library(ggplot2)
library(reshape2)

# Create data frame
df <- data.frame(
  Borough = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"),
  Percent_White = c(11.64, 34.7, 45.8, 32.9, 71.3),
  Percent_Black = c(31.94, 34.4, 15.3, 19.0, 8.9),
  Percent_API = c(3.64, 7.5, 9.4, 17.5, 5.6),
  Percent_Hispanic = c(49.14, 19.8, 27.2, 25.0, 12.1),
  Percent_Other = c(3.64, 3.6, 2.4, 5.6, 2.1)
)

# Melt the data frame for easier plotting
df_melted <- melt(df, id.vars = "Borough")

# Plot stacked bar graph
ggplot(df_melted, aes(x = Borough, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Races by Borough",
       x = "Borough",
       y = "Percentage",
       fill = "Race") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("darkorchid", "lightpink", "plum2", "palevioletred1", "violetred"), name = "Race")
# Calculate total arrest count for each NY borough
total_arrest_count <- aggregate(arrest_count ~ NY_Borough, data = Final_merged_data, FUN = sum)

# Merge total_arrest_count with merged_data
merged_data <- merge(Final_merged_data, total_arrest_count, by = "NY_Borough", all.x = TRUE)

custom_colors <- c("ASIAN / PACIFIC ISLANDER" = "darkorchid",
                   "BLACK" = "lightpink", "BLACK HISPANIC" = "plum2", "WHITE" = "palevioletred1", "WHITE HISPANIC" = "violet", "UNKNOWN" = "violetred", "AMERICAN INDIAN/ALASKAN NATIVE" = "pink")

# Plotting
ggplot(merged_data, aes(x = NY_Borough, y = arrest_count.y, fill = PERP_RACE)) +
  geom_bar(position = "stack", alpha = 0.5, stat = "identity") +
  scale_fill_manual(values = custom_colors) +  # Specify custom colors
  labs(title = "Total Arrest Count by NY Borough and Perpetrator Race",
       x = "NY Borough", y = "Total Arrest Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Load required library
library(ggplot2)

# Total arrest count for each data set
total_arrest_count <- aggregate(arrest_count ~ NY_Borough, data = Final_merged_data, FUN = sum)

# Plotting
ggplot(total_arrest_count, aes(x = NY_Borough, y = arrest_count)) +
  geom_bar(stat = "identity", fill = "violet", alpha = 0.5) + # Adjust alpha here
  labs(title = "Total Arrest Count for Each Data Set", x = "NY Borough", y = "Total Arrest Count") +
  theme_minimal()

# Calculate total arrest count for each NY borough
total_arrest_count <- aggregate(arrest_count ~ NY_Borough, data = merged_data, FUN = sum)

# Merge total_arrest_count with merged_data
merged_data <- merge(merged_data, total_arrest_count, by = "NY_Borough", all.x = TRUE)

# Plotting
ggplot(merged_data, aes(x = NY_Borough, y = arrest_count.y, fill = PERP_RACE)) +
  geom_bar(position = "stack", alpha = 0.5, stat = "identity") +
  labs(title = "Total Arrest Count by NY Borough and Perpetrator Race",
       x = "NY Borough", y = "Total Arrest Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Define the parameters
observed_proportion <- proportion_black_combined / 100
expected_proportion <- 0.312  # actual percentage of Black people in the population

# Perform the one-sample proportion test
prop.test(x = black_combined_count, n = total_individuals, p = expected_proportion, alternative = "two.sided")
# Load the ggplot2 package
library(ggplot2)

# Define custom colors
custom_colors <- c("BRONX" = "pink",
                   "BROOKLYN" = "violetred",
                   "MANHATTAN" = "darkorchid",
                   "QUEENS" = "palevioletred1",
                   "STATEN ISLAND" = "purple")

# Plot the barplot using ggplot2
ggplot(merged_data, aes(x = NY_Borough, fill = Race)) +
  geom_bar(position = "stack", alpha = 0.5) +  # Make bars transparent
  scale_fill_manual(values = custom_colors) +  # Specify custom colors
  labs(title = "Distribution of Races in Each Borough",
       x = "Borough",
       y = "Percentage") +
  ylim(0, 100) +  # Set y-axis limit
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Race"))  # Move legend to the upper right corner inside the plot


# Define custom colors
custom_colors <- c("pink", "orchid", "plum2", "violetred", "purple")

# Plot the barplot with custom colors and transparent bars
barplot(as.matrix(merged_data), 
        beside = TRUE, 
        col = custom_colors,  # Use custom colors
        legend.text = rownames(merged_data),
        args.legend = list(x = "topright", bty = "n", cex = 0.7),
        main = "Distribution of Races in Each Borough",
        xlab = "Borough",
        ylab = "Percentage",
        border = NA,  # Remove border
        density = NULL,  # Remove density
        space = 0,  # Remove space between bars
        args = list(col.axis = "black", lwd = 1, cex.axis = 1, font = 2),  # Adjust axis color, width, and font
        alpha = 0.5)  # Make bars transparent

# Define custom colors
custom_colors <- c("darkorchid", "lightpink", "plum2", "palevioletred1", "violetred")

# Plot the barplot with white background
ggplot(df_melted, aes(x = Borough, y = value, fill = variable)) +
  geom_bar(stat = "identity", alpha = 0.5) +  # Make bars transparent
  labs(title = "Percentage of Races by Borough",
       x = "Borough",
       y = "Percentage",
       fill = "Race") +
  theme_minimal() +  # Use minimal theme
  theme(panel.background = element_rect(fill = "white")) +  # Set background to white
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_colors, name = "Race")
# Create a list with the specified elements
borough_list <- list(
  Bronx = 37397,
  `Brooklyn (Kings)` = 56942,
  Manhattan = 85071,
  Queens = 64509,
  `Staten Island (Richmond)` = 79201
)

# Convert the list to a data frame
borough_df <- data.frame(Borough = names(borough_list), Income = unlist(borough_list))

# Plot the bar graph with custom colors and transparency
library(ggplot2)
ggplot(borough_df, aes(x = Borough, y = Income)) +
  geom_bar(stat = "identity", fill = "violet", alpha = 0.5) +  # Make bars transparent
  labs(title = "Income by Borough",
       x = "Borough",
       y = "Income") +
  theme_minimal()  # Use minimal theme

# Total number of individuals in the dataset
total_individuals <- nrow(Final_merged_data)

# Number of individuals who are "Black" or "Black Hispanic"
black_combined_count <- sum(Final_merged_data$PERP_RACE %in% c("BLACK", "BLACK HISPANIC"))

# Calculate the proportion
proportion_black_combined <- (black_combined_count / total_individuals) * 100

proportion_black_combined

# Define the parameters
observed_proportion <- proportion_black_combined / 100
expected_proportion <- 0.312  # actual percentage of Black people in the population

# Perform the one-sample proportion test
prop.test(x = black_combined_count, n = total_individuals, p = expected_proportion, alternative = "two.sided")

# Create a list with the specified elements
borough_list <- list(
  Bronx = 37397,
  `Brooklyn (Kings)` = 56942,
  Manhattan = 85071,
  Queens = 64509,
  `Staten Island (Richmond)` = 79201
)

# Convert the list to a data frame
borough_df <- data.frame(Borough = names(borough_list), Income = unlist(borough_list))

# Create a list with the specified elements
borough_list <- list(
  Bronx = 37397,
  `Brooklyn (Kings)` = 56942,
  Manhattan = 85071,
  Queens = 64509,
  `Staten Island (Richmond)` = 79201
)

# Convert the list to a data frame
borough_df <- data.frame(Borough = names(borough_list), Income = unlist(borough_list))

# Plot the bar graph with custom colors and transparency
library(ggplot2)
ggplot(borough_df, aes(x = Borough, y = Income)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.5) +  # Make bars transparent
  labs(title = "Income by Borough",
       x = "Borough",
       y = "Income") +
  theme_minimal()  # Use minimal theme






































