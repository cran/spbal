## -----------------------------------------------------------------------------
set.seed(2026)

# Define the population.  
pop <- matrix(runif(2000 * 2), ncol = 2)

# Define the number of sample points to draw from the population.
n <- 250

# The maximum number of candidate samples.
J1 <- 200

## -----------------------------------------------------------------------------
# Use all logical CPU's
n_threads <- 0

# Display step timings
verbose <- FALSE

# Build single inverse-distance matrix
cache_W <- TRUE

## -----------------------------------------------------------------------------
library(spbal)

sampleMatrix <- DAS(pop = pop,
                    n = n, 
                    J1 = J1, 
                    n_threads = n_threads, 
                    verbose = verbose,
                    cache_W = cache_W)

## -----------------------------------------------------------------------------
# first candidate sample
s <- sampleMatrix[1,]
s

## -----------------------------------------------------------------------------
library(reshape2)
library(ggplot2)

# plotting...
index_matrix <- sampleMatrix # leave sampleMatrix intact
df <- pop

# Melt the matrix into long format to map indices to colors
index_long <- melt(index_matrix)
colnames(index_long) <- c("row", "col", "index")

# Merge the matrix data with the dataframe coordinates
plot_data <- merge(index_long, df, by.x = "index", by.y = "row.names")

# Create a color column for the row groups of the matrix
plot_data$color_group <- ifelse(plot_data$row == 1, "red", "black")

# Dynamic title using the actual values of n and number of locations
n_value <- n
n_locations <- length(pop) / 2

# Plot the points - BLACK drawn first, RED on top (guaranteed layering)
ggplot(plot_data, aes(x = V1, y = V2, color = color_group)) +
  # Black points (bottom layer)
  geom_point(data = plot_data[plot_data$color_group == "black", ],
             size = 3) +
  # Red points (top layer - drawn after black)
  geom_point(data = plot_data[plot_data$color_group == "red", ],
             size = 3) +
  scale_color_manual(values = c("black" = "black", "red" = "red")) +
  theme_minimal() +
  labs(color = "Matrix Row Group",
       title = paste0("DAS Sample of n = ", n_value,
                      " from ", n_locations, " Locations, J1=", J1),
       caption = "First row colored in RED.") +
  xlab("X Coordinate") +
  ylab("Y Coordinate")

## -----------------------------------------------------------------------------
set.seed(2026)

# Numeric matrix of population coordinates, one unit per row.
# x points uniformly in [0,1)^2. pop must be a numeric matrix with one unit per row.
# pop must have at least one coordinate column.
pop <- matrix(runif(2000 * 2), ncol = 2)

# Numeric matrix of auxiliary variables, one row per unit.
# aux must be a numeric matrix with one row per unit.
# aux must have the same number of rows as pop.
# aux must have at least one column.
aux <- matrix(runif(2000 * 2), ncol = 2)
aux <- aux / sqrt(rowSums(aux^2)) 

# The number of sample points to draw from the population.
# n must satisfy 1 <= n < nrow(pop).
# There is no default.
n <- 150

# Mix in [0, 1]. The default 0.5.
# alpha must be a single number in [0, 1].
alpha <- 0.5

# Maximum number of candidate samples, \code{2 <= J1 <= floor(N/2)}.
# Default is \code{min(500, floor(N/2))}. Values above 500 are allowed but
# trigger a warning (large assignment problems dominate run time).
J1 <- 200

## -----------------------------------------------------------------------------
# Use all logical CPU's
n_threads <- 0

# Display step timings
verbose <- FALSE

# Build single inverse-distance matrix
cache_W <- TRUE

## -----------------------------------------------------------------------------
library(spbal)

sampleMatrix <- doubleDAS(pop = pop,
                          aux = aux,
                          n = n,
                          alpha = alpha,
                          J1 = J1, 
                          n_threads = n_threads, 
                          verbose = verbose,
                          cache_W = cache_W)

## -----------------------------------------------------------------------------
# first candidate sample
s <- sampleMatrix[1,]
s

## -----------------------------------------------------------------------------
library(reshape2)
library(ggplot2)

# plotting...
index_matrix <- sampleMatrix # leave sampleMatrix intact
df <- pop

# Melt the matrix into long format to map indices to colors
index_long <- melt(index_matrix)
colnames(index_long) <- c("row", "col", "index")

# Merge the matrix data with the dataframe coordinates
plot_data <- merge(index_long, df, by.x = "index", by.y = "row.names")

# Create a color column for the row groups of the matrix
plot_data$color_group <- ifelse(plot_data$row == 1, "red", "black")

# Dynamic title using the actual values of n and number of locations
n_value <- n
n_locations <- length(pop) / 2

# Plot the points, colored by the row group of the matrix
ggplot(plot_data, aes(x = V1, y = V2, color = color_group)) +
  # Black points (bottom layer)
  geom_point(data = plot_data[plot_data$color_group == "black", ],
             size = 3) +
  # Red points (top layer - drawn after black)
  geom_point(data = plot_data[plot_data$color_group == "red", ],
             size = 3) +
  scale_color_manual(values = c("black" = "black", "red" = "red")) +
  theme_minimal() +
  labs(color = "Matrix Row Group",
       title = paste0("doubleDAS Sample of n = ", n_value,
                      " from ", n_locations, " Locations, J1=", J1),
       subtitle = paste0("alpha=", alpha, "."),
       caption = "First row colored in RED.") +
  xlab("X Coordinate") +
  ylab("Y Coordinate")

