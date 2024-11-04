#' Format the dataframe output from calculate_area()
#'
#' @export
format_df <- function(input) {
    df_fmt <- with(input$df,
                   data.frame(FROM_STATION = '',
                              INCLUDED_ANGLE = '',
                              BEARING = BEARING,
                              LENGTH = LENGTH,
                              `LOG-COS LOG-SIN` = '',
                              LATITUDE_PLUS = ifelse(disp_lat >= 0, disp_lat, ''),
                              LATITUDE_MINUS = ifelse(disp_lat < 0, abs(disp_lat), ''),
                              ` ` = '',
                              DEPARTURE_PLUS = ifelse(disp_lon >= 0, disp_lon, ''),
                              DEPARTURE_MINUS = ifelse(disp_lon < 0, abs(disp_lon), ''),
                              COORDINATES_X = coord_x,
                              COORDINATES_Y = coord_y,
                              BEACON_POINTS = '',
                              ` ` = '',
                              TOTAL = ifelse(area_latitude >= 0, '(+)', '(-)'),
                              LATITUDE = abs(area_latitude),
                              ADJ = ifelse(df$area_longitude >= 0, '(+)', '(-)'),
                              DEPARTURE = abs(area_longitude),
                              ` ` = '',
                              ` ` = '',
                              `(+) PRODUCT` = ifelse(area_product >= 0, area_product, ''),
                              `(-) PRODUCT` = ifelse(area_product < 0, abs(area_product), ''),
                              START_X = X[1],
                              START_Y = Y[1],
                              AREA_HA = '',
                              AREA_ACRE = ''))
    df$AREA_HA[1] <- input$area_ha
    df$AREA_ACRE[1] <- input$area_acre

    return(df)
}
