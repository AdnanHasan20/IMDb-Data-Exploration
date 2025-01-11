
# ----------- IDS Questions ---------------

# 1. Which directors and writers consistently produce highly-rated or popular movies?
# First, filter out the data where the 'directors' and 'writers' are not missing, and then group by 'directors' and 'writers' 
# to find the average rating and total votes


# Join with ratings to retain 'averageRating' and 'numVotes'
director_writer_ratings <- sample_title_crew %>%
  inner_join(sample_title_ratings, by = "tconst") %>%
  filter(!is.na(directors) & !is.na(writers))

director_writer_summary <- director_writer_ratings %>%
  group_by(directors, writers) %>%
  dplyr:: summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),
    total_votes = sum(numVotes, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_rating))




top_directors_writers <- sample_title_crew %>%
  filter(!is.na(directors)) %>%  # Exclude rows with missing directors
  left_join(sample_title_ratings, by = "tconst") %>%
  filter(!is.na(averageRating) & !is.na(numVotes)) %>%  # Filter out rows with missing ratings/votes
  separate_rows(directors, sep = ",") %>%  # Separate multiple directors if present
  group_by(directors) %>%  # Group by directors
  dplyr::summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),  # Average rating
    total_votes = sum(numVotes, na.rm = TRUE),      # Total votes
    movie_count = n(),  # Count number of movies for each director
    .groups = "drop"
  ) %>%
  arrange(desc(total_votes)) %>%
  filter(movie_count > 5)  # Only consider those who worked on more than 5 movies

# View the result
print(top_directors_writers)




# Reshape the data: We will gather the directors and writers and calculate their movie success
directors_writers_data <- movie_data %>%
  # Expand the lists of directors and writers into separate rows
  gather(key = "role", value = "nconst", directors, writers) %>%
  # Remove rows where nconst is NA (i.e., titles with no directors or writers)
  filter(!is.na(nconst)) %>%
  # Group by person (nconst) and calculate average rating, total votes, and number of movies
  group_by(nconst) %>%
  dplyr::summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),
    total_votes = sum(numVotes, na.rm = TRUE),
    movie_count = n(),
    .groups = "drop"
  ) %>%
  # Filter for people with at least 5 movies for relevance
  filter(movie_count >= 5) %>%
  arrange(desc(total_votes))


str(directors_writers_data)




# Step: Correlation Analysis

# Selecting relevant numeric columns for correlation
correlation_data <- movie_data %>%
  select(averageRating, numVotes)

# Calculating the correlation matrix
correlation_matrix <- cor(correlation_data, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)







library(dplyr)
library(ggplot2)

# Step 1: Identify top directors from `movie_data`
top_directors <- movie_data %>%
  filter(averageRating > 8) %>%  # Filter movies with high average ratings
  select(tconst, directors, averageRating, numVotes) %>%
  distinct(directors)  # Keep only distinct directors

# Step 2: Link directors with their movie titles
top_director_movies <- movie_data %>%
  filter(averageRating > 8) %>%  # Filter movies with high ratings
  inner_join(title_basics, by = "tconst") %>%  # Add movie titles
  select(directors, primaryTitle.x, averageRating, numVotes) %>%  # Select relevant columns
  arrange(desc(averageRating))  # Sort by rating

# Step 3: Simplify data for visualization
# Top 5 directors with their top 3 movies
top_director_movies_filtered <- top_director_movies %>%
  group_by(directors) %>%
  slice_max(averageRating, n = 1) %>%  # Keep top 3 movies per director
  ungroup() %>%
  filter(directors %in% head(unique(directors), 20))  







library(dplyr)
library(ggplot2)

# Step 1: Identify top directors from `movie_data`
top_directors <- movie_data %>%
  filter(averageRating > 8) %>%  # Filter movies with high average ratings
  select(tconst, directors, averageRating, numVotes) %>%
  distinct(directors)  # Keep only distinct directors

# Step 2: Link directors with their movie titles and names
top_director_movies <- movie_data %>%
  filter(averageRating > 8) %>%  # Filter movies with high ratings
  inner_join(title_basics, by = "tconst") %>%  # Add movie titles
  inner_join(names_basics, by = c("directors" = "nconst")) %>%  # Add director names
  select(primaryName, primaryTitle.x, averageRating, numVotes) %>%  # Select relevant columns
  arrange(desc(averageRating))  # Sort by rating



excluded_titles <- c("Episode 20", "Episode #1.1")  # Replace with actual titles

top_director_movies_filtered <- top_director_movies %>%
  filter(!primaryTitle.x %in% excluded_titles) %>%  # Exclude specified titles
  group_by(primaryName) %>%
  slice_max(averageRating, n = 3) %>%  # Keep top 3 movies per director
  ungroup() %>%
  filter(primaryName %in% head(unique(primaryName), 20))  # Limit to top 5 directors




# Step 4: Visualize the data
ggplot(top_director_movies_filtered, aes(x = averageRating, y = primaryName, fill = primaryTitle.x)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Top Directors and Their High-Rated Works",
    x = "Average Rating",
    y = "Director",
    fill = "Titles"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))





# 2. Can the success of a movie (high votes and ratings) be predicted based on the crew (directors and writers)?
# Using total votes and directors/writers as predictors


director_writer_avg_rating <- movie_data %>%
  group_by(tconst) %>%
  dplyr::summarise(avg_rating = mean(averageRating, na.rm = TRUE), 
                   total_votes = sum(numVotes, na.rm = TRUE)) %>%
  arrange(desc(avg_rating)) # Sort by average rating




# Sort by average rating

# plot 2 ----
# Plotting the average rating for each director/writer
ggplot(director_writer_avg_rating, aes(x =  avg_rating, y = avg_rating, fill = avg_rating)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates to make the names readable
  labs(
    title = "Directors and Writers with the Highest Average Ratings",
    x = "Director/Writer",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "none"
  )




director_writer_model <- lm(avg_rating ~ total_votes + directors + writers, data = director_writer_summary)
summary(director_writer_model)


# Summary of the model to get R-squared


# Predicted values from the model
predictions <- predict(director_writer_model)

# Calculate RMSE
rmse <- sqrt(mean((predictions - director_writer_summary$avg_rating)^2))
rmse

# Residuals
residuals <- director_writer_model$residuals

coefficients <- coef(director_writer_model)
importance <- data.frame(
  Predictor = names(coefficients),
  Coefficient = coefficients
)









# Plotting residuals
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Residuals Distribution", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Alternatively, you can create a residual vs fitted plot to check for patterns
fitted_values <- director_writer_model$fitted.values
ggplot(data.frame(fitted_values, residuals), aes(x = fitted_values, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()


