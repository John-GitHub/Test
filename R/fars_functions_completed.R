#' Read a csv file in as a tbl for a particular year of data from the Fatality
#' Analysis Reporting System.
#'
#' @param filename The name of a csv file.
#'
#' @return A tbl representation of the data contained in the csv file.
#' @include readr
#' @details An error will result if the file name does not exist.
#' @example
#' fars_read("accident_2013.csv.bz2")
#' @export

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make a file name for a paticular year's data file from the Fatality Analysis
#' Reporting System.
#'
#' @param year A numeric or interter; should be the full 4 digit year.
#'
#' @return The name of the file from the Fatality Analysis Reporting System for
#'   that year.
#' @include readr
#' @examples
#' foo<-fars_read(make_filename(2013))
#' @details An error will result if the year is outside the scope of years
#'   available as data sets.
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read a csv file in as a tbl for multiple years of data from the Fatality
#' Analysis Reporting System.
#'
#' @param years A numeric or integer vector of years; should be the full 4 digit
#'   years.
#'
#' @return A list of tbls representing the year, Month data extracted from the
#'   given set of years from the Fatality Analysis Reporting System.
#' @include readr, dplyr
#' @details An error will result if the indicated packages are not attached.
#' @examples
#' foo<-fars_read_years(2013:2015)
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Creates a summary table of the number of fatal accidents by month by year for
#' a given set of years.
#'
#' @param years A numeric or integer vector of years; should be the full 4 digit
#'   years.
#'
#' @return A table of the number of fatal accidents by year by month for the
#'   given set of years.
#'
#' @include readr, dplyr
#' @details An error will result if the years are not given as full 4 digit
#'   representions or if the indicated packages are not attached.
#' @examples
#' fars_summarize_years(2013:2015)
#' @export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Creates a summary table of the number of fatal accidents by month by year for
#' a given set of years for a given state.
#'
#' @param state.num A number indicating the state.
#' @param year A numeric or interter; should be the full 4 digit year.
#'
#' @return A map of the fatal accidents for a state in a given year.
#'
#' @examples
#' fars_map_state(1,2013)
#' @include  dplyr, maps, readr
#' @details An error will result if the years are not given as full 4 digit
#'   representions or if the indicated packages are not attached.
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
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
