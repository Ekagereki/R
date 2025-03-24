library(dplyr)
library(ggplot2)
library(tidyr)


# know the working directory
getwd()

# change the directory to where data is located
setwd("/Users/edwinkagereki/Downloads")

# Load the data specifying an encoding to handle multibyte strings
data <- read.csv('task_data(in).csv', stringsAsFactors = FALSE, 
                 fileEncoding = 'latin1')

# Confirm header names and preview the data
print(head(data))

# Check how many rows have missing values
sum(complete.cases(data))  # Count of complete cases
sum(!complete.cases(data)) # Count of incomplete cases

# Remove rows with missing values
data <- na.omit(data)

# Rename columns for better readability
colnames(data) <- gsub("[[:punct:]]", "", colnames(data))  # Remove special characters
colnames(data) <- gsub("\\s+", "_", colnames(data)) # Replace spaces with underscores

# Response mapping: Create a function to map responses to numeric values
map_response_to_score <- function(response) {
  # Remove extra spaces
  response <- trimws(response)
  if (grepl('Not at all', response, ignore.case = TRUE)) {
    return(0)
  } else if (grepl('Several days', response, ignore.case = TRUE)) {
    return(1)
  } else if (grepl('Over half the days', response, ignore.case = TRUE)) {
    return(2)
  } else if (grepl('Nearly/almost every day', response, ignore.case = TRUE)) {
    return(3)
  } else {
    return(NA)
  }
}

# Define depression and anxiety item columns using column numbers
# Depression items: columns 4 to 11
# Anxiety items: columns 12 to 18
depression_cols <- 4:11
anxiety_cols <- 12:18

## Apply the mapping function to each of the target columns to create numeric columns
for (col in depression_cols) {
  new_col <- paste0('dep_', col)
  data[[new_col]] <- sapply(data[[col]], map_response_to_score)
}

for (col in anxiety_cols) {
  new_col <- paste0('anx_', col)
  data[[new_col]] <- sapply(data[[col]], map_response_to_score)
}

# Compute composite scores for depression and anxiety by summing the scores
data <- data %>%
  rowwise() %>%
  mutate(depression_score = sum(c_across(starts_with('dep_')), na.rm = TRUE),
         anxiety_score = sum(c_across(starts_with('anx_')), na.rm = TRUE)) %>%
  ungroup()

# Convert Condition to factor for grouping
condition <- as.factor(data$Condition)


# Descriptive statistics by condition for depression and anxiety scores
stat_dep <- data %>% 
  group_by(data$Condition) %>% 
  summarise(mean_dep = mean(depression_score, na.rm = TRUE),
            sd_dep = sd(depression_score, na.rm = TRUE),
            n = n())

stat_anx <- data %>% 
  group_by(data$Condition) %>% 
  summarise(mean_anx = mean(anxiety_score, na.rm = TRUE),
            sd_anx = sd(anxiety_score, na.rm = TRUE),
            n = n())

print(stat_dep)
print(stat_anx)

# Boxplots for visual inspection - depression score
p1 <- ggplot(data, aes(x = condition, y = depression_score, fill = condition)) +
  geom_boxplot() +
  labs(title = 'Depression Scores by Group', y = 'Depression Score', x = 'Condition') +
  theme_minimal()
print(p1)

# Boxplots for anxiety scores
p2 <- ggplot(data, aes(x = condition, y = anxiety_score, fill = condition)) +
  geom_boxplot() +
  labs(title = 'Anxiety Scores by Group', y = 'Anxiety Score', x = 'Condition') +
  theme_minimal()
print(p2)

# Perform t-tests to compare depression scores between conditions
t_test_dep <- t.test(depression_score ~ condition, data = data)
print(t_test_dep)

# Perform t-tests to compare anxiety scores between conditions
t_test_anx <- t.test(anxiety_score ~ condition, data = data)
print(t_test_anx)

# Save the modified data with the computed composite scores to a new CSV file
write.csv(data, 'task_data_with_scores.csv', row.names = FALSE)