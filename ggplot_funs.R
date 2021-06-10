Statmean <- ggproto("Statmean", Stat,
                    compute_group = function(data, scales) {
                      mean <- mean(data$x)
                      data.frame(xintercept=mean)
                    },
                    required_aes = c("x")
)

stat_mean <- function(mapping = NULL, data = NULL, geom = "vline",
                      position = "identity", na.rm = FALSE,
                      show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = Statmean, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


StatPercentileX <- ggproto("StatPercentileX", Stat,
                           compute_group = function(data, scales, probs) {
                             percentiles <- Hmisc::wtd.quantile(data$x, probs=probs, weights = data$P001)
                             data.frame(xintercept=percentiles)
                           },
                           required_aes = c("x")
)

stat_percentile_x <- function(mapping = NULL, data = NULL, geom = "vline",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatPercentileX, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatPercentileXLabels <- ggproto("StatPercentileXLabels", Stat,
                                 compute_group = function(data, scales, probs) {
                                   percentiles <- Hmisc::wtd.quantile(data$x, probs=probs, weights = data$P001)
                                   data.frame(x=percentiles, y=Inf,
                                              # label=paste0("P", probs*100),
                                              label = "Mediana")
                                   # label=paste0("p", probs*100, ": ",
                                   #              round(percentiles, digits=3)))
                                 },
                                 required_aes = c("x")
)

stat_percentile_xlab <- function(mapping = NULL, data = NULL, geom = "text",
                                 position = "identity", na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatPercentileXLabels, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
