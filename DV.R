# ----------- Data Visualization ---------------

## Q1 ----
# 1. What are the most popular genres in movies over the decades?

# Load the treemapify library for treemap visualizations
library(treemapify)

# Prepare the dataset for creating a treemap visualization
treemap_data <- sample_title_basics %>%
  filter(!is.na(startYear) & startYear != "\\N" & !is.na(genres)) %>%  # Remove invalid or missing year and genre data
  separate_rows(genres, sep = ",") %>%  # Split rows containing multiple genres into separate rows
  mutate(
    decade = floor(as.numeric(startYear) / 10) * 10,  # Calculate the decade for each movie
    genres = ifelse(genres == "\\N", "Others", genres)  # Replace missing genres with "Others"
  ) %>%
  group_by(decade, genres) %>%
  dplyr::summarise(total_movies = n(), .groups = "drop")  # Count the number of movies by genre and decade

# Plot the treemap visualization
ggplot(treemap_data, aes(
  area = total_movies,  # Area of each block proportional to the total number of movies
  fill = genres,  # Fill color representing the genre
  label = paste(genres, "Movies:", total_movies)  # Add labels for genres and counts
)) +
  geom_treemap() +  # Create the treemap layout
  geom_treemap_text(
    fontface = "italic",  # Use italic font for the labels
    colour = "white",  # Label color
    place = "center",  # Center the labels
    grow = TRUE  # Adjust label size to fit blocks
  ) +
  facet_wrap(~decade, ncol = 2) +  # Split the visualization by decade
  labs(
    title = "Treemap of Movies Produced by Genre Over Decades",  # Title for the treemap
    fill = "Genre"  # Legend label
  ) +
  theme_minimal() +  # Minimal theme for clean visuals
  theme(legend.position = "right")  # Position the legend on the right

## DATA VIZ 2 ----
# 2. What are the top-rated genres and how do their ratings vary with the number of votes?

# Merge the movie details dataset with the ratings dataset
sample_title_basics_ratings <- sample_title_basics %>%
  left_join(sample_title_ratings, by = "tconst")  # Join datasets on the unique movie identifier

# Process the dataset to calculate top-rated genres
top_genres_ratings <- sample_title_basics_ratings %>%
  separate_rows(genres, sep = ",") %>%  # Split rows containing multiple genres into separate rows
  group_by(genres) %>%
  dplyr::summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),  # Calculate the average rating for each genre
    total_votes = sum(numVotes, na.rm = TRUE),  # Calculate the total votes for each genre
    .groups = 'drop'
  ) %>%
  filter(total_votes > 1000)  # Filter out genres with fewer than 1000 total votes

top_genres_ratings$total_votes = top_genres_ratings$total_votes / 1000  # Scale down total votes for easier visualization

# Plot the relationship between genres and ratings
ggplot(top_genres_ratings, aes(x = genres, y = "Average Rating")) +
  geom_tile(aes(fill = avg_rating), color = "white") +  # Use a tile plot with fill color based on average rating
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg. Rating") +  # Gradient for average ratings
  labs(
    title = "Top-Rated Genres and Their Ratings",  # Title of the plot
    x = "Genre",  # X-axis label
    y = ""  # Remove Y-axis label
  ) +
  theme_minimal() +  # Minimal theme for clean visuals
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate X-axis labels for readability
    legend.position = "right"  # Position the legend on the right
  )



## RQ 3 ----
# 3. How do the number of movies produced in different genres vary over the decades?

# Filter and process the dataset for genre production analysis
sample_title_basics <- sample_title_basics %>%
  filter(!is.na(startYear) & startYear != "\\N" & !is.na(genres))  # Filter out invalid year and genre data

genre_production <- sample_title_basics %>%
  separate_rows(genres, sep = ",") %>%  # Split rows containing multiple genres into separate rows
  group_by(startYear, genres) %>%
  dplyr::summarise(total_movies = n(), .groups = "drop")  # Count the number of movies per genre per year

# Prepare the dataset for an area plot
movies_by_genre_decade <- genre_production %>%
  filter(!is.na(startYear), genres != "", startYear >= 1900) %>%  # Filter valid years and genres
  mutate(
    startYear = as.numeric(startYear),  # Convert year to numeric
    decade = (startYear %/% 10) * 10  # Calculate the decade for each movie
  ) %>%
  separate_rows(genres, sep = ",") %>%
  mutate(genres = ifelse(genres == "\\N", "Others", genres)) %>%  # Replace missing genres with "Others"
  group_by(decade, genres) %>%
  dplyr::summarise(movie_count = n(), .groups = "drop") %>%  # Count the number of movies by genre and decade
  arrange(decade)

# Plot an area chart to visualize the production trends of genres over decades
ggplot(movies_by_genre_decade, aes(x = decade, y = movie_count, fill = genres)) +
  geom_area(alpha = 0.8, color = "grey", size = 0.3) +  # Use an area plot to show trends
  scale_fill_viridis_d(option = "plasma") +  # Apply the plasma color scheme for gradients
  labs(
    title = "Number of Movies Produced in Different Genres Over the Decades",  # Title of the plot
    x = "Decade",  # X-axis label
    y = "Number of Movies",  # Y-axis label
    fill = "Genre"  # Legend label
  ) +
  theme_minimal() +  # Minimal theme for clean visuals
  theme(
    legend.position = "right",  # Position the legend on the right
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate X-axis labels for readability
  )


## DATA VIZ 4 ----
# 4. How do genres contribute to the success of movies in terms of both ratings and votes?

# Process the dataset to calculate the contribution of each genre to movie success
genre_success <- sample_title_basics_ratings %>%
  separate_rows(genres, sep = ",") %>% # Split rows containing multiple genres into separate rows
  
  mutate(
    genres = ifelse(genres == "\\N", "Others", genres)  # Replace missing genres with "Others"
  ) %>%
  
  group_by(genres) %>%
  dplyr::summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),  # Calculate the average rating for each genre
    total_votes = sum(numVotes, na.rm = TRUE),  # Calculate the total votes for each genre
    .groups = 'drop'
  )

genre_success$total_votes_scaled <- genre_success$total_votes / 10000  # Scale down total votes for better visualization

# Plot a bubble chart to show genre success based on ratings and votes
ggplot(genre_success, aes(x = genres, y = avg_rating, size = total_votes_scaled, color = genres)) +
  geom_point() +  # Use points to represent each genre
  scale_size_continuous(range = c(2, 12), name = "Total Votes\n(Scaled)") +  # Scale bubble sizes for votes
  labs(
    title = "Genres Contribution to Movie Success",  # Title of the chart
    x = "Genre",  # X-axis label
    y = "Average Rating"  # Y-axis label
  ) +
  theme_minimal() +  # Minimal theme for clean visuals
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate X-axis labels for readability

