season_fun <- function(x, .names) {
  .year <- x[[1]]
  .x <- x[, -1]
  for (i in 1:length(.names)) {
    .x[[.names[i]]] <- rowMeans(cbind(.x[[i]], .x[[i + 1]]))
  }
  .x$year <- .year
  .x <- .x %>% select(year, !!.names)
  .x
}

cor_fun <- function(x) {
  cor(x$mei, x$precip, method = "kendall")
}
