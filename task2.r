
library(dplyr)
library(ggplot2)
library(cluster)
library(plotly)

generate_shell_clusters <- function(n_shells, k_per_shell, max_radius,
                                    noise_sd = 0.1) {
  
  # Smallest radius scales with max_radius so it never exceeds it
  shell_radii <- seq(max_radius / n_shells, max_radius, length.out = n_shells)
  
  coords_list <- vector("list", n_shells)
  
  for (s in seq_len(n_shells)) {
    r_mean <- shell_radii[s]
    # radial component with Gaussian noise
    r <- rnorm(k_per_shell, mean = r_mean, sd = noise_sd)
    
    # sample random directions on the unit sphere
    dir_mat <- matrix(rnorm(3 * k_per_shell), ncol = 3)
    dir_norm <- sqrt(rowSums(dir_mat^2))
    dir_unit <- dir_mat / dir_norm
    
    points <- dir_unit * r  
    coords_list[[s]] <- data.frame(
      x1 = points[, 1],
      x2 = points[, 2],
      x3 = points[, 3],
      shell = s
    )
  }
  do.call(rbind, coords_list)
}

set.seed(123)
df_shells <- generate_shell_clusters(
  n_shells     = 4,
  k_per_shell  = 100,
  max_radius   = 10,
  noise_sd     = 0.1
)

p <- plot_ly(
  df_shells,
  x = ~x1, y = ~x2, z = ~x3,
  color = ~factor(shell),
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, opacity = 0.7)
)
# Save the plot as an HTML file
htmlwidgets::saveWidget(
  as_widget(p),
  "figures/shell_clusters_3d_plot.html"
)   


spectral_kmeans <- function(x, k, d_threshold = 1) {
  x <- as.matrix(x)
  n <- nrow(x)
  if (n < k) stop("Not enough points to form k clusters.")
  
  # 1. Adjacency matrix A based on distance threshold
  dist_mat <- as.matrix(dist(x))
  A <- ifelse(dist_mat <= d_threshold, 1, 0)
  diag(A) <- 0   # no self-loops
  
  # 2. Degree matrix and (symmetric) normalized Laplacian L_sym
  degree_vec <- rowSums(A)
  D <- diag(degree_vec)
  L <- D - A
  
  inv_sqrt_deg <- 1 / sqrt(degree_vec)
  inv_sqrt_deg[!is.finite(inv_sqrt_deg)] <- 0  # handle isolated nodes
  D_inv_sqrt <- diag(inv_sqrt_deg)
  
  L_sym <- D_inv_sqrt %*% L %*% D_inv_sqrt
  
  # 3. Eigen-decomposition of L_sym
  eig <- eigen(L_sym, symmetric = TRUE)
  # Smallest eigenvalues first
  idx <- order(eig$values)
  U <- eig$vectors[, idx[1:k], drop = FALSE]
  
  # 4. K-means on rows of U
  set.seed(123)
  km <- kmeans(U, centers = k, nstart = 10)
  km
}
spectral_clust_wrapper <- function(x, k, d_threshold = 1) {
  spectral_kmeans(x, k = k, d_threshold = d_threshold)
}
library(cluster)

set.seed(123)
X <- df_shells[, c("x1", "x2", "x3")]

gap_test <- clusGap(
  X,
  FUNcluster = spectral_clust_wrapper,
  K.max      = 8,
  B          = 20,
  d_threshold = 1
)
n_shells    <- 4
k_per_shell <- 100
noise_sd    <- 0.1
d_threshold <- 1

# Varying max_radius to see its effect on estimated k
max_radius_values <- seq(10, 1, by = -1)

get_k_hat <- function(X, K.max = 8, d_threshold = 1, B = 20) {
  gap_res <- clusGap(
    X,
    FUNcluster = spectral_clust_wrapper,
    K.max      = K.max,
    B          = B,
    d_threshold = d_threshold
  )
  
  k_vals <- 1:K.max
  gap_vals <- gap_res$Tab[, "gap"]
  se_vals  <- gap_res$Tab[, "SE.sim"]
  
  k_hat <- maxSE(f = gap_vals, SE.f = se_vals, method = "Tibs2001SEmax")
  k_hat
}


for (d_threshold in c(0.8, 1.0, 1.2)) { 
    results <- lapply(max_radius_values, function(Rmax) {
    df <- generate_shell_clusters(
        n_shells    = n_shells,
        k_per_shell = k_per_shell,
        max_radius  = Rmax,
        noise_sd    = noise_sd
    )
    X <- df[, c("x1", "x2", "x3")]
    
    k_hat <- get_k_hat(X, K.max = 8, d_threshold = d_threshold, B = 20)
    
    data.frame(
        max_radius = Rmax,
        k_hat      = k_hat
    )
    })

    results_df <- bind_rows(results)

    p1 <- ggplot(results_df, aes(x = max_radius, y = k_hat)) +
    geom_line(color = "#1c78e9") +
    geom_point(size = 2, color = "#1c78e9") +
    geom_hline(yintercept = n_shells, linetype = "dashed") +
    scale_x_reverse(breaks = max_radius_values) +   # view from large to small radius
    labs(
        x = "Maximum radius of outer shell",
        y = "Estimated number of clusters (k)",
        title = "Spectral clustering on concentric shells",
        subtitle = paste("n_shells =", n_shells,
                        ", k_per_shell =", k_per_shell,
                        ", d_threshold =", d_threshold)
    ) +
    theme_classic(base_size = 14)
    ggsave(paste0("figures/spectral_clustering_gap_statistic_d_threshold_", d_threshold, ".png"),
        plot = p1,
        width = 8, height = 6, dpi = 150)
}