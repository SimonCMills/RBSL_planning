#' Convert bearing to angle
#'
#' @noRd
bearing_to_angle <- function(bearing) {
    angle <- 90 - bearing
    angle[angle < 0] <- angle[angle < 0] + 360
    angle
}

#' Convert bearing to angle
#'
#' @noRd
angle_to_bearing <- function(angle) {
    bearing <- 90-angle
    bearing[angle > 90] <- 360 - (bearing[angle > 90] - 90)
    bearing
}

#' Clean bearings
#'
#' @noRd
clean_bearings <- function(x) {
    clean_coords <- gsub("[^0-9]", " ", x) |>
        gsub(pattern="^ *| *$", replacement = "") |>
        gsub(pattern=" {2,}", replacement=" ")
    return(clean_coords)
}

#' Convert HMS bearing to decimal
#'
#' @noRd
hms_to_dec <- function(clean_bearings) {
    pt1 <- gsub("(^[0-9]*) .*", "\\1", clean_bearings) |> as.numeric()
    pt2 <- gsub("^[0-9]* ([0-9]*) .*", "\\1", clean_bearings) |> as.numeric()
    pt3 <- gsub("^.* ([0-9]*)$", "\\1", clean_bearings) |> as.numeric()

    return(pt1 + pt2/60 + pt3/3600)
}

dec_to_hms <- function(dec) {
    h <- floor(dec)
    s <- dec %% 1 * 3600
    m <- floor(s/60)
    s <- round(s %% 60, 0)

    paste0(paste0(rep(0, 3 - nchar(h)), h), '°',
           paste0(rep(0, 2-nchar(m)), m), "'",
           paste0(rep(0, 2-nchar(s)), s), "''")
}
