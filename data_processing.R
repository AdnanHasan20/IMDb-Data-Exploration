movie_data <- sample_title_crew %>%
  left_join(sample_title_ratings, by = "tconst") %>%
  left_join(sample_title_basics, by = "tconst") %>%
  filter(!is.na(averageRating) & !is.na(numVotes)) # Filter for non-missing ratings and votes

sample_title_crew <- sample_title_crew %>%
  mutate(
    directors = ifelse(directors == "\\N", NA, directors),
    writers = ifelse(writers == "\\N", NA, writers),
    directors = str_trim(directors),
    writers = str_trim(writers)
  ) %>%
  separate_rows(directors, sep = ",") %>%
  separate_rows(writers, sep = ",") 


director_summary <- movie_data %>%
  separate_rows(directors) %>%
  group_by(directors) %>%
  dplyr::summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),
    total_votes = sum(numVotes, na.rm = TRUE),
    num_movies = n()
  ) %>%
  filter(num_movies > 10) %>%  # Limit to directors with more than 10 movies
  arrange(desc(avg_rating))


# Calculate the IQR for total_votes to filter out outliers
Q1 <- quantile(director_summary$total_votes, 0.25)
Q3 <- quantile(director_summary$total_votes, 0.75)
IQR_value <- Q3 - Q1


# Define the outlier thresholds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value



# Filter out directors based on the number of votes
director_summary_filtered <- director_summary %>%
  filter(total_votes >= lower_bound & total_votes <= upper_bound) %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n = 10)  # Adjust the 'n' value to show top N directors


# Remove rows with missing values
director_summary <- director_summary %>%
  filter(!is.na(avg_rating) & !is.na(directors))  # Remove any rows with missing values



# Count the number of movies per director and filter directors with >= 5 movies
director_summary_filtered <- director_summary %>%
  group_by(directors) %>%
  filter(n() >= 5) %>%
  ungroup()  # Ungroup after filtering to avoid issues



director_writer_avg_rating <- movie_data %>%
  group_by(tconst) %>%
  dplyr::summarise(avg_rating = mean(averageRating, na.rm = TRUE), 
                   total_votes = sum(numVotes, na.rm = TRUE)) %>%
  arrange(desc(avg_rating)) 



