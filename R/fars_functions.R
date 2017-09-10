#' Read data from a file into a tbl dataframe
#'
#' This function reads data from a specified file. This operation is based on the
#' read_csv function from the readr package. It is useful for reading the most
#' common types of flat file data, comma separated values and tab separated values.
#'
#' @param filename A character string which specifies path to a file.
#' Files ending in .gz, .bz2, .xz, or .zip will be automatically uncompressed.
#'
#' @return This function return a tbl data frame (using the dplyr package).
#' An error will be thrown if the specified file dosen't exist.
#'
#' @examples
#' \dontrun{
#' fars_read("data/accident_2013.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make file names corresponding to given years.
#'
#' This function creates the file names corresponding to a given vector of years.
#'
#' @param year A numeric or a character vector that specifies the years
#' for which we want to build the file names.
#'
#' @return A character vector containing the file names
#'
#' @examples
#' make_filename(2013)
#' make_filename("2013")
#' make_filename(2013:2015)
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}
#' Read Month and year columns from data files of given years.
#'
#' This function reads the data from files for a set of given years. For each year
#' it reads the data, adds a "year" column and selects the two columns "MONTH" and
#' "year".
#'
#' @param years A numeric or character vector containing years for which, the data
#' will be read.
#'
#' @return A list of tbl data frames with columns "MONTH" and "year" corresponding
#' to the specified years. In case of wrong year specification an error will be
#' thrown.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(2013:2014)
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, year = ~year) %>%
                                dplyr::select_("MONTH", "year")
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize data by year and by month for a given set of years.
#'
#' This function reads years and months from data for a given set of years.
#' The number of accidents by year and by month.
#'
#' @param years A numeric or character vector containing years for which, the data
#' will be read.
#'
#' @return A tbl data frame containing the number of accidents summarized by year
#' and by month.Bad year argument or missing files will produce an error.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years("2013")
#' fars_summarize_years(2013:2015)
#' }
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr bind_rows
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_(~year, ~MONTH) %>%
                dplyr::summarize_(n = ~n()) %>%
                tidyr::spread_(key_col = 'year', value_col = 'n')
}
#' Plots accidents locations on a map for a given year and a given state.
#'
#' Plots accidents locations on a map for a given year and a given state.
#'
#' @param state.num A numeric value used to specify a state number.
#' @param year A numeric value to specify the year for which the graphic
#' is plotted. An error will be thrown if there are no records for the given
#' state and the specified year. Wrong year or state number specification will
#' produce an error.
#'
#' @return A map plot of accidents location is returned.
#'
#' @examples
#' \dontrun{
#'  fars_map_state(1,2013)
#'  }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~ STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
