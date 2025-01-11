
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(plyr)

# Read in the data files (only relevant columns for memory efficiency)
title_basics <- fread("title.basics.tsv", select = c("tconst", "startYear", "genres", "numVotes", "averageRating", "primaryTitle", "titleType"))
title_crew <- fread("title.crew.tsv", select = c("tconst", "directors", "writers"))
title_ratings <- fread("title.ratings.tsv", select = c("tconst", "averageRating", "numVotes"))
names_basics <- fread("name.basics.tsv", select = c("nconst", "primaryName", "knownForTitles"))





# ----------- Sampling Step ---------------
set.seed(123)  # For reproducibility

# Calculate .05% of the total rows for a smaller sample
sample_size <- floor(0.0005 * nrow(title_basics))  

# Sample 1% of the data
sample_title_basics <- title_basics[sample(1:nrow(title_basics), sample_size), ]
sample_title_crew <- title_crew[tconst %in% sample_title_basics$tconst, ]
sample_title_ratings <- title_ratings[tconst %in% sample_title_basics$tconst, ]




