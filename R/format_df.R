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
                              LATITUDE_PLUS = ifelse(disp_lat >= 0, round(disp_lat, 1), ''),
                              LATITUDE_MINUS = ifelse(disp_lat < 0, abs(round(disp_lat, 1)), ''),
                              ` ` = '',
                              DEPARTURE_PLUS = ifelse(disp_lon >= 0, round(disp_lon, 1), ''),
                              DEPARTURE_MINUS = ifelse(disp_lon < 0, abs(round(disp_lon, 1)), ''),
                              COORDINATES_X = coord_x,
                              COORDINATES_Y = coord_y,
                              BEACON_POINTS = '',
                              ` ` = '',
                              TOTAL = ifelse(area_latitude >= 0, '(+)', '(-)'),
                              LATITUDE = abs(round(area_latitude, 1)),
                              ADJ = ifelse(area_departure >= 0, '(+)', '(-)'),
                              DEPARTURE = abs(round(area_departure, 1)),
                              ` ` = '',
                              ` ` = '',
                              `PRODUCT_PLUS` = ifelse(area_product >= 0, round(area_product, 1), ''),
                              `PRODUCT_MINUS` = ifelse(area_product < 0, abs(round(area_product, 1)), ''),
                              START_X = '',
                              START_Y = '',
                              AREA_HA = '',
                              AREA_ACRE = ''))

    df_fmt$START_X[1] = input$df$X[1]
    df_fmt$START_Y[1] = input$df$Y[1]
    df_fmt$AREA_HA[1] <- input$area_ha
    df_fmt$AREA_ACRE[1] <- input$area_acre

    return(df_fmt)
}
