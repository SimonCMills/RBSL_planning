#' Process list of input files
#'
#' @export
process_files <- function(file_list) {

    for(file_i in file_list) {
        outname_i <- gsub('^.*/', '', file_i)
        print(paste0('~~~~~~~ Processing ', file_i, ' ~~~~~~~'))
        if(!file.exists(file_i)) {
            paste0(file_i, ' is not found. Is your working directory set
                   properly?') |>
                strwrap() |>
                paste(collapse=' \n') |>
                stop()
        }
        output_i <- calculate_area(file_i)
        df_i <- format_df(output_i)

        if(!dir.exists('outputs')) {
            print('Creating outputs/ directory to write output files to')
            dir.create('outputs')
        }
        write.csv(output_i, paste0('outputs/', outname_i))
    }
}
