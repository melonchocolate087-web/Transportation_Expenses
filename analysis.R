
# PROJECT: Transportation Habits and Daily Expenses
# STEP 4: Data Analysis and Graph Generation

# 1. Load required package
library(ggplot2)

# 2. Load the dataset
transportation <- read.csv("transportation_expenses.csv")

# 3. Display the dataset
View(transportation)

# 4. Calculate descriptive statistics
summary_statistics <- data.frame(
  Average_Age = mean(transportation$Age),
  Average_Trips_Per_Day = mean(transportation$Trips_Per_Day),
  Average_Daily_Travel_Hours = mean(transportation$Daily_Travel_Hours),
  Average_Daily_Fare_PHP = mean(transportation$Daily_Fare_PHP),
  Average_Monthly_Budget_PHP = mean(transportation$Monthly_Transport_Budget_PHP),
  Average_Distance_KM = mean(transportation$Distance_KM),
  Average_Satisfaction = mean(transportation$Satisfaction_Rating)
)

print("SUMMARY STATISTICS")
print(summary_statistics)

# 5. Analyze main transportation methods
transport_summary <- as.data.frame(
  table(transportation$Main_Transport)
)

colnames(transport_summary) <- c(
  "Main_Transport",
  "Number_of_Respondents"
)

print("TRANSPORTATION METHOD SUMMARY")
print(transport_summary)

# 6. Analyze travel purposes
purpose_summary <- as.data.frame(
  table(transportation$Travel_Purpose)
)

colnames(purpose_summary) <- c(
  "Travel_Purpose",
  "Number_of_Respondents"
)

print("TRAVEL PURPOSE SUMMARY")
print(purpose_summary)

# 7. Analyze average daily fare by transportation method
fare_summary <- aggregate(
  Daily_Fare_PHP ~ Main_Transport,
  data = transportation,
  FUN = mean
)

print("AVERAGE DAILY FARE BY TRANSPORTATION")
print(fare_summary)

# 8. Create a folder for graphs
if (!dir.exists("figures")) {
  dir.create("figures")
}

# GRAPH 1: Transportation Method Distribution
graph1 <- ggplot(
  transportation,
  aes(x = Main_Transport)
) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Most Common Transportation Methods",
    x = "Transportation Method",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 30, hjust = 1
  ))

ggsave(
  "figures/transportation_methods.png",
  graph1,
  width = 8,
  height = 5
)

# GRAPH 2: Average Daily Fare by Transportation Method
graph2 <- ggplot(
  fare_summary,
  aes(
    x = Main_Transport,
    y = Daily_Fare_PHP
  )
) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Average Daily Transportation Fare",
    x = "Transportation Method",
    y = "Average Daily Fare (PHP)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 30, hjust = 1
  ))

ggsave(
  "figures/average_daily_fare.png",
  graph2,
  width = 8,
  height = 5
)

# GRAPH 3: Daily Travel Hours vs Daily Fare
graph3 <- ggplot(
  transportation,
  aes(
    x = Daily_Travel_Hours,
    y = Daily_Fare_PHP
  )
) +
  geom_point(
    color = "darkgreen",
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red"
  ) +
  labs(
    title = "Daily Travel Time vs Transportation Fare",
    x = "Daily Travel Hours",
    y = "Daily Fare (PHP)"
  ) +
  theme_minimal()

ggsave(
  "figures/travel_time_vs_fare.png",
  graph3,
  width = 8,
  height = 5
)

# 9. Export summary files
write.csv(
  summary_statistics,
  "summary_statistics.csv",
  row.names = FALSE
)

write.csv(
  transport_summary,
  "transportation_summary.csv",
  row.names = FALSE
)

write.csv(
  purpose_summary,
  "travel_purpose_summary.csv",
  row.names = FALSE
)

write.csv(
  fare_summary,
  "fare_summary.csv",
  row.names = FALSE
)

# 10. Display graphs
print(graph1)
print(graph2)
print(graph3)

# 11. Completion message
print("Analysis and graph generation completed successfully!")


