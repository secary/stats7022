pacman::p_load("import")
import::from("ds_a3_q3.R", get_kmeans)

x <- rep(2:4, each = 3)
get_kmeans(x = x, 3)
