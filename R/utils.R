#' Format percentages
#'
#' Fix scales::percent() for NA values
#'
#' @param x A numeric vector to format
#' @inheritDotParams scales::number_format
#' @return formatted character vector
#' @importFrom scales percent
#' @importFrom data.table fifelse
#' @export
pct <- function(x, ...) fifelse(is.na(x) | !is.numeric(x), "--", percent(x, ...))


#' Format currencies
#'
#' Fix scales::comma() for NA values
#'
#' @param x A numeric vector to format
#' @inheritDotParams scales::number_format
#' @return formatted character vector
#' @importFrom scales comma
#' @importFrom data.table fifelse
#' @export
curr <- function(x, ...) fifelse(is.na(x) | !is.numeric(x), "--", comma(x, ...))

#' Format yday dates
#'
#' @param x A numeric vector to format
#' @inheritParams base::format.Date
#' @return formatted character vector
#' @export
yday_to_date <- function(x, format="%m-%d") format(as.Date(as.character(x), format="%j"), format)

