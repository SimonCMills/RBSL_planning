#' Calculate area
#'
#' Function to calculate area from a dataframe that contains 4 columns:
#'  1.  An X column: this should have units metres and be empty apart from the
#'      first entry.
#'  2.  A Y column: this should have units metres and be empty apart from the
#'      first entry.
#'  3.  A BEARING column, formatted as: HHH°MM'SS"
#'  4.  A LENGTH column
#'
#' @export
calculate_area <- function(df) {
    # check names
    df_names <- names(df)
    target_names <- c('X', 'Y', 'BEARING', 'LENGTH')
    for (name in target_names) {
        if(!any(df_names == name)) {
            paste0(fname, ' does not include a column labelled: ', name, '. Check
            that the column headings are correct.')  |>
                strwrap() |>
                paste(collapse=' \n') |>
                stop()
        }
    }
    aberrant_names <- length(!(df_names %in% target_names))
    if(aberrant_names != 0) {
        paste0(fname, ' includes additional columns beyond the required X, Y,
        BEARING, and LENGTH. Remove these and rerun.')  |>
            strwrap() |>
            paste(collapse=' \n') |>
            stop()
    }

    # format bearings
    df$bearing_clean <- clean_bearings(df$BEARING)
    bearing_check <- grepl('[0-9]{1-3} [0-9]{2} [0-9]{2}', df$bearing_clean)
    if(any(bearing_check == FALSE)) {
        c(paste0(fname, ' contains bearings that are not correctly formatted.
        Offending entries are: '),
          df$BEARING[!bearing_check]) |>
            strwrap() |>
            paste(collapse=' \n') |>
            stop()
    }

    df$bearing_deg <- dec_to_deg(df$bearing_clean)

    if(any(df$bearing_deg < 0)) {
        paste0(fname, ' contains bearings that are less than 0. Check that numbers
               are correct and that they all follow the correct formatting rules')  |>
            strwrap() |>
            paste(collapse=' \n') |>
            stop()
    }

    if(any(df$bearing_deg >= 360)) {
        paste0(fname, ' contains bearings that are greater than or equal to 360.
           Check that numbers are correct and that they all follow the correct
           formatting rules') |>
            strwrap() |>
            paste(collapse=' \n') |>
            stop()
    }

    # Check length
    len_num <- as.numeric(df$LENGTH)
    check_numeric_len <- tryCatch(expr = as.numeric(df$LENGTH),
                                  warning=function(w) w)
    if(check_numeric_len$message == 'NAs introduced by coercion') {
        paste0(fname, ' contains entries in the LENGTH column that are not
        coercible to a number (e.g. are character or string entries). Check the
        LENGTH column entries.') |>
            strwrap() |>
            paste(collapse=' \n') |>
            stop()
    }

    # compute angles etc.
    df$angle_deg <- bearing_to_angle(df$bearing_deg)
    df$angle_rad <- df$angle_deg/180*pi
    df$LENGTH <- as.numeric(df$LENGTH)
    df$disp_lat <- sin(df$angle_rad) * df$LENGTH
    df$disp_lon <- cos(df$angle_rad) * df$LENGTH
    df$coord_x <- df$X[1] + cumsum(df$disp_lon)
    df$coord_y <- df$Y[1] + cumsum(df$disp_lat)

    # area calculation
    df$area_latitude <- cumsum(df$disp_lat)
    df$area_departure <- 0
    for(i in 1:(nrow(df)-1)) {
        df$area_departure[i] <- df$disp_lon[i+1] + df$disp_lon[i]
    }

    cum_lat <- df$area_latitude[nrow(df)]
    cum_lon <- df$area_departure[nrow(df)]

    if(cum_lat != 0) {
        paste0('The bearings and lengths in', fname, ' does not produce a closed
        loop (i.e. the bearings and lengths do not return to the starting point).
        There is an accumulated discrepancy of ', round(cum_lat, 3), ' metres.
               Check all entries.') |>
            strwrap() |>
            paste(collapse=' \n') |>
            stop()
    }
    if(cum_lon != 0) {
        paste0('The bearings and lengths in', fname, ' does not produce a closed
        loop, and have an accumulated discrepancy of ', round(cum_lon, 3),
        ' metres. Check all entries.') |>
            strwrap() |>
            paste(collapse=' \n') |>
            stop()
    }

    area_ha <- sum(df$area_latitude * df$area_departure)/2e4
    area_acre <- area_ha * 2.47105

    list(df = df,
         area_ha = area_ha,
         area_acre = area_acre)
}
