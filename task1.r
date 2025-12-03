
library(cluster)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)


generate_hypercube_clusters <- function(n, k, side_length, noise_sd = 1.0) {
  # n dimensions → n clusters
  # cluster centers are the positive corners of the hypercube
  centers <- diag(side_length, nrow = n, ncol = n)

  # generate points around each center
  data <- do.call(rbind, lapply(1:n, function(i) {
    matrix(rnorm(k * n, mean = centers[i, ], sd = noise_sd),
           nrow = k, ncol = n, byrow = TRUE)
  }))

  return(data)
}
estimate_clusters <- function(dat, maxK) {
  gap <- clusGap(
    dat,
    FUN = function(x, k) kmeans(x, centers = k, nstart = 20, iter.max = 50),
    K.max = maxK,
    B = 50
  )
  best_k <- maxSE(gap$Tab[,"gap"], gap$Tab[,"SE.sim"])
  return(list(gap = gap, best_k = best_k))
}

#### simulation parameters
dims <- c(6, 5, 4, 3, 2)
side_lengths <- 10:1
k <- 100 # points per cluster
noise_sd <- 1


results <- expand.grid(
  dim = dims,
  side_length = side_lengths
) %>%
  arrange(dim, desc(side_length)) %>%
  mutate(estimated_k = NA_integer_)

for (i in seq_len(nrow(results))) {
  n <- results$dim[i]
  L <- results$side_length[i]

  message("Running dim = ", n, ", side_length = ", L)

  dat <- generate_hypercube_clusters(
    n = n, k = k, side_length = L, noise_sd = noise_sd
  )

  est <- estimate_clusters(dat, maxK = n)
  results$estimated_k[i] <- est$best_k
}

p <- ggplot(results, aes(x = side_length, y = estimated_k,
                    color = factor(dim))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_hline(aes(yintercept = dim), linetype = "dashed") +
  facet_wrap(~dim, scales = "free_y") +
  scale_x_reverse(breaks = 10:1) +
  labs(
    title = "Cluster Detectability vs. Hypercube Side Length",
    x = "Side Length (clusters get closer as this decreases)",
    y = "Estimated Number of Clusters",
    color = "Dimension"
  ) +
  theme_classic()
ggsave("figures/gap_estimated_k_vs_side_length.png", p,
             width = 10, height = 6, dpi = 150)

