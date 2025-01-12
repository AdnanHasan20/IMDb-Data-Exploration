
# ----------- Data Visualization Questions ---------------

## Q1 ----
# 1. What are the most popular genres in movies over the decades?
# We can group by 'startYear' and 'genres' to find the number of movies per genre per year.
# Load additional library for treemap visualization
library(treemapify)


# Data preparation for the treemap
treemap_data <- sample_title_basics %>%
  filter(!is.na(startYear) & startYear != "\\N" & !is.na(genres)) %>%  # Filter valid data
  separate_rows(genres, sep = ",") %>%  # Split multiple genres into separate rows
  mutate(
    decade = floor(as.numeric(startYear) / 10) * 10,  # Create a decade column
    genres = ifelse(genres == "\\N", "Others", genres)  # Replace missing genres with "Others"
  ) %>%
  group_by(decade, genres) %>%
  dplyr::summarise(total_movies = n(), .groups = "drop")  # Count number of movies per genre per decade



ggplot(treemap_data, aes(
  area = total_movies,
  fill = genres,
  label = paste(genres, "Movies:", total_movies)
)) +
  geom_treemap() +
  geom_treemap_text(
    fontface = "italic",
    colour = "white",
    place = "center",
    grow = TRUE
  ) +
  facet_wrap(~decade, ncol = 2) +  # Facet by decade
  labs(
    title = "Treemap of Movies Produced by Genre Over Decades",
    fill = "Genre"
  ) +
  theme_minimal() +
  theme(legend.position = "right")



  
  

  




##DATA VIZ 2 ----
# 2. What are the top-rated genres and how do their ratings vary with the number of votes?
# We will group by 'genres' and calculate the average rating and total votes, filtering for genres with a sufficient number of votes

sample_title_basics_ratings <- sample_title_basics %>%
  left_join(sample_title_ratings, by = "tconst")

# 2. What are the top-rated genres and how do their ratings vary with the number of votes?
top_genres_ratings <- sample_title_basics_ratings %>%
  separate_rows(genres, sep = ",") %>%
  group_by(genres) %>%
  dplyr::summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),
    total_votes = sum(numVotes, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(total_votes > 1000)  

top_genres_ratings$total_votes = top_genres_ratings$total_votes / 1000

ggplot(top_genres_ratings, aes(x = genres, y = "Average Rating")) +
  geom_tile(aes(fill = avg_rating ), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Rating") +
  labs(
    title = "Top-Rated Genres and Their Ratings vs. Votes",
    x = "Genre",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "right"
  )


##DATA VIZ 3 ----
# 3. How do genres contribute to the success of movies in terms of both ratings and votes?
# Group by genres and plot how total votes relate to average ratings for each genre
genre_success <- sample_title_basics_ratings %>%
  separate_rows(genres, sep = ",") %>%
 mutate(
    genres = ifelse(genres == "\\N", "Others", genres)  # Replace missing genres with "Others"
  ) %>%
  group_by(genres) %>%
  dplyr::summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),
    total_votes = sum(numVotes, na.rm = TRUE),
    .groups = 'drop'
  )

genre_success$total_votes_scaled <- genre_success$total_votes / 10000  # Dividing by 10,000

ggplot(genre_success, aes(x = genres, y = avg_rating, size = total_votes_scaled, color = genres)) +
  geom_point() +
  scale_size_continuous(range = c(2, 12), name = "Total Votes\n(Scaled)") +  # Adjust bubble size range
  labs(title = "Genres' Contribution to Movie Success", x = "Genre", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##RQ 4 ----
# 4. How do the number of movies produced in different genres vary over the decades?
# Group by 'startYear' and 'genres' to see the trend of production
sample_title_basics <- sample_title_basics %>%
  filter(!is.na(startYear) & startYear != "\\N" & !is.na(genres))

# Group by startYear and genres, and count the number of movies for each combination
#Group by startYear and genres, and count the number of movies for each combination
genre_production <- sample_title_basics %>%
  separate_rows(genres, sep = ",") %>%  # Split multiple genres into separate rows
  group_by(startYear, genres) %>%
  dplyr::summarise(total_movies = n(), .groups = "drop")  # Count number of movies per genre per year

# Alternative for the fourth plot: Stacked Bar Chart

# Pl





# Load additional library for treemap visualization
library(treemapify)

# Data preparation for the treemap
treemap_data <- sample_title_basics %>%
  filter(!is.na(startYear) & startYear != "\\N" & !is.na(genres)) %>%  # Filter valid data
  separate_rows(genres, sep = ",") %>%  # Split multiple genres into separate rows
  mutate(
    decade = floor(as.numeric(startYear) / 10) * 10,  # Create a decade column
    genres = ifelse(genres == "\\N", "Others", genres)  # Replace missing genres with "Others"
  ) %>%
  group_by(decade, genres) %>%
  dplyr::summarise(total_movies = n(), .groups = "drop")  # Count number of movies per genre per decade






#rq 4 : refine ----

library(ggplot2)
library(dplyr)
library(tidyr)

# Preparing data: Extract decade and group by genres and decades
movies_by_genre_decade <- genre_production %>%
  filter(!is.na(startYear), genres != "", startYear >= 1900) %>%  # Filter valid years and genres
  mutate(
    startYear = as.numeric(startYear),  # Ensure startYear is numeric
    decade = (startYear %/% 10) * 10    # Convert year to decade
  ) %>%
  separate_rows(genres, sep = ",") %>%  # Split multiple genres
  mutate(genres = ifelse(genres == "\\N", "Others", genres)) %>%  # Replace /N with "Others"
  group_by(decade, genres) %>%
  dplyr::summarise(movie_count = n(), .groups = "drop") %>%
  arrange(decade)



# Plot: Stacked Area Chart
ggplot(movies_by_genre_decade, aes(x = decade, y = movie_count, fill = genres)) +
  geom_area(alpha = 0.8, color = "grey", size = 0.3) +
  scale_fill_viridis_d(option = "plasma") + # Use viridis for a beautiful gradient
  labs(
    title = "Number of Movies Produced in Different Genres Over the Decades",
    x = "Decade",
    y = "Number of Movies",
    fill = "Genre"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )












