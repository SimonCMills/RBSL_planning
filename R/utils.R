#' Convert bearing to angle
#'
#' @noRd
bearing_to_angle <- function(bearing) {
    angle <- 90 - bearing
    angle[angle < 0] <- angle[angle < 0] + 360
    angle
}

#' Clean bearings
#'
#' @noRd
clean_bearings <- function(x) {
    clean_coords <- gsub("[^0-9]", " ", df$BEARING) |>
        gsub(pattern="^ *| *$", replacement = "") |>
        gsub(pattern="  ", replacement=" ")
    return(clean_coords)
}

#' Convert HMS bearing to decimal
#'
#' @noRd
dec_to_deg <- function(clean_bearings) {
    pt1 <- gsub("(^[0-9]*) .*", "\\1", clean_bearings) |> as.numeric()
    pt2 <- gsub("^[0-9]* ([0-9]*) .*", "\\1", clean_bearings) |> as.numeric()
    pt3 <- gsub("^.* ([0-9]*)$", "\\1", clean_bearings) |> as.numeric()

    return(pt1 + pt2/60 + pt3/3600)
}
