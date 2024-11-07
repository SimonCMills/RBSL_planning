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
calculate_area <- function(fname) {
    if(!file.exists(fname)) {
        paste0(fname, ' does not exist')  |>
            strwrap() |>
            paste(collapse=' \n') |>
            stop()
    }
    df <- read.csv(fname, fileEncoding = 'latin1')

    # check names
    df_names <- colnames(df)
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
    if(ncol(df) != 4) {
        paste0(fname, ' includes additional columns beyond the required X, Y,
        BEARING, and LENGTH. Remove these and rerun.')  |>
            strwrap() |>
            paste(collapse=' \n') |>
            stop()
    }

    # format bearings
    df$bearing_clean <- clean_bearings(df$BEARING)
    bearing_check <- grepl('^[0-9]{1,3} [0-9]{1,2} [0-9]{1,2}$', df$bearing_clean)
    if(any(bearing_check == FALSE)) {
        c(paste0(fname, ' contains bearings that are not correctly formatted.
        Offending entries are: '),
          df$BEARING[!bearing_check]) |>
            strwrap() |>
            paste(collapse=' \n') |>
            stop()
    }

    df$bearing_deg <- hms_to_dec(df$bearing_clean)

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
    if(class(check_numeric_len) != 'numeric') {
        if(check_numeric_len$message == 'NAs introduced by coercion') {
            paste0(fname, ' contains entries in the LENGTH column that are not
        coercible to a number (e.g. are character or string entries). Check the
        LENGTH column entries.') |>
                strwrap() |>
                paste(collapse=' \n') |>
                stop()
        }
    }

    # compute angles etc.
    df$angle_deg <- bearing_to_angle(df$bearing_deg)
    df$angle_rad <- df$angle_deg/180*pi
    df$LENGTH <- as.numeric(df$LENGTH)
    df$disp_lat <- sin(df$angle_rad) * df$LENGTH
    df$disp_lon <- cos(df$angle_rad) * df$LENGTH
    df$coord_x <- df$X[1] + cumsum(df$disp_lon)
    df$coord_y <- df$Y[1] + cumsum(df$disp_lat)

    cum_lat <- cumsum(df$disp_lat)[nrow(df)]
    cum_lon <- cumsum(df$disp_lon)[nrow(df)]

    if(cum_lat != 0 | cum_lon != 0) {
        paste0('The bearings and lengths in "', fname, '" do not produce a closed
        loop (i.e. the bearings and lengths do not return to the starting point),
        with a discrepancy of ', round(cum_lat, 2) , ' metres latitude and ',
               round(cum_lon, 2), ' metres longitude. Final bearing and length
               has been adjusted to achieve closure') |>
            strwrap() |>
            paste(collapse=' \n') |>
            writeLines()

        x_delta <- df$X[1] - df$coord_x[nrow(df)-1]
        y_delta <- df$Y[1] - df$coord_y[nrow(df)-1]
        final_angle <- atan2(y_delta, x_delta)
        if(final_angle < 0) final_angle <- 2*pi + final_angle
        final_angle_deg <- final_angle/pi * 180
        final_bearing <- dec_to_hms(angle_to_bearing(final_angle_deg))
        final_length <- y_delta/sin(final_angle)

        # recompute angles etc.
        df$angle_rad[nrow(df)] <- final_angle
        df$BEARING[nrow(df)] <- final_bearing
        df$LENGTH[nrow(df)] <- final_length # do not round until final step!
        df$disp_lat <- sin(df$angle_rad) * df$LENGTH
        df$disp_lon <- cos(df$angle_rad) * df$LENGTH
        df$coord_x <- df$X[1] + cumsum(df$disp_lon)
        df$coord_y <- df$Y[1] + cumsum(df$disp_lat)
    }

    # area calculation
    df$area_latitude <- cumsum(df$disp_lat)
    df$area_departure <- NA
    for(i in 1:(nrow(df)-1)) {
        df$area_departure[i] <- df$disp_lon[i+1] + df$disp_lon[i]
    }

    df$area_product <- df$area_latitude * df$area_departure
    area_ha <- sum(df$area_product, na.rm=TRUE)/2e4
    area_acre <- area_ha * 2.47105

    df$LENGTH[nrow(df)] <- round(final_length, 2)

    list(df = df,
         area_ha = area_ha,
         area_acre = area_acre)
}
