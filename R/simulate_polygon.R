#' Simulate polygon without any voids
#'
#' Used to check correctness of calculation
#'
#' @export
simulate_polygon <- function(n) {
    if(!requireNamespace('sf', quietly = TRUE)) {
        stop("package `sf` required to use this function")
    }

    u <- runif(n, 0, 2*pi) |> sort()
    d <- runif(n, 100, 200)
    df <- data.frame(x = cos(u) * d,
                     y = sin(u) * d)
    df <- rbind(df, df[1,])
    poly <- df |>
        as.matrix() |>
        list() |>
        sf::st_polygon(dim = 'XY') |>
        sf::st_sfc() |>
        sf::st_set_crs('epsg:32630') # UTM 30 (or anything in units m)

    df_offsets <- apply(df, 2, function(x) x[2:length(x)] - x[1:(length(x)-1)]) |>
        as.data.frame()
    angles <- atan2(df_offsets$y, df_offsets$x)
    angles <- ifelse(angles < 0,  2*pi + angles, angles)
    angles_deg <- angles/pi*180

    lengths <- df_offsets$y/sin(angles)
    bearings <- dec_to_hms(angle_to_bearing(angles_deg))

    df2 <- data.frame(X=NA, Y=NA, BEARING=bearings, LENGTH=lengths)
    df2$X[1] <- df$x[1]
    df2$Y[1] <- df$y[1]

    list(poly = poly,
         bearings_and_lengths = df2)
}
