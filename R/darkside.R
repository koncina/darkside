#' @import xml2
#' @importFrom utils type.convert

NULL

get_column_titles <- function(x) {
  xml_titles <- xml_find_all(x, "./d1:Title")
  col_idx <- as.numeric(gsub("^.*\\[(\\d+)\\]\\/\\*\\[\\d+\\]$", "\\1", xml_path(xml_titles)))
  col_idx <- col_idx - as.numeric(gsub(".*\\[(\\d+)\\]$", "\\1", xml_path(x[1]))) + 1
  out <- character(length(x))
  out[col_idx] <- xml_text(xml_titles)
  out[out == ""]  <- NA_character_
  out
}

# Most of the time subcolumns contain different observations or timepoints
# But prism allows also to enter already calculated error values (SD, SEM...)
# We need to provide this information as subcolumn_names

get_subcolumn_titles <- function(xml_table) {
  switch(xml_attr(xml_table, "YFormat"),
         SDN = c("mean", "sd", "n"),
         SEM = c("mean", "sem", "n"),
         CVN = c("mean", "percent_cv", "n"),
         SD = c("mean", "sd"),
         SE = c("mean", "sem"),
         CV = c("mean", "percent_cv"),
         `low-high` = c("mean", "error_high", "error_low"),
         `upper-lower-limits` = c("mean", "limit_upper", "limit_lower")
  )
}

get_subcolumn_values <- function(x, subcolumn = "*") {
  values <- xml_text(xml_find_all(x, paste0("./d1:Subcolumn[", subcolumn, "]/d1:d")))
  type.convert(values, na.strings = c("NA", ""), as.is = TRUE, dec = ",") # Is the decimal point always "," in pzfx files?
}

get_x_columns <- function(node_table) {

  x_columns <- list()

  # We could first test if the node is present but this seems to be faster...
  row_names <- xml_text(xml_find_all(node_table, "./d1:RowTitlesColumn/d1:Subcolumn/d1:d"))
  if (length(row_names) > 0) x_columns <- list(row_name = row_names)

  if (xml_attr(node_table, "XFormat") != "none") {
    x_column <- xml_find_first(node_table, "./d1:XColumn")
    x_name <- xml_text(xml_find_all(x_column, "./d1:Title"))

    if (nchar(x_name) == 0 || length(x_name) == 0) x_name <- "x_value"

    x_column <- get_subcolumn_values(x_column)
    if (length(x_column) > 0) x_columns[[x_name]] <- x_column
  }

  if (xml_attr(node_table, "XFormat") == "startenddate") {
    x_advanced_columns <- xml_find_first(node_table, "./d1:XAdvancedColumn")
    x_advanced_columns <- lapply(c(start_date = 1, end_data = 2), function(x) get_subcolumn_values(x_advanced_columns, x))
    x_columns <- c(x_columns, x_advanced_columns)
  }

  x_columns
}

standardise_data_table <- function(path, data_table) {
  if (length(data_table) != 1) {
    stop("`data_table` must have length 1", call. = FALSE)
  }

  pzfx_dt <- pzfx_tables(path)

  if (is.numeric(data_table)) {
    if (data_table < 1 || data_table > length(pzfx_dt)) stop("`data_table` index out of range", call. = FALSE)
    data_table_out <- data_table
  } else if (is.character(data_table)) {
    data_table_out <- which(pzfx_dt == data_table)

    if (length(data_table_out) == 0) stop(sprintf("data table '%s' not found", data_table), call. = FALSE)
    else if (length(data_table_out) > 1) {
      stop(sprintf("multiple data tables are named '%s': select the data table by index", data_table), call. = FALSE)
    }
  } else stop("`data_table` must be either an integer or a string.", call. = FALSE)
  data_table_out
}

fake_tibble <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE, check.rows = FALSE)
  class(x) <- c("tbl_df", "tbl", "data.frame") # Setting tibble class to allow pretty printing without tibble dependency
  x
}

#' List all data tables in a pzfx file
#'
#' List all data tables in a pzfx file
#'
#' @param path Path to the pzfx file.
#'
#' @return A character vector with the name of the data tables
#'
#' @export
pzfx_tables <- function(path) {
  xml <- read_xml(path)
  xml <- xml_find_all(xml, "//d1:GraphPadPrismFile/d1:Table/d1:Title")
  xml_text(xml_contents(xml))
}

#' Read Graphpad prism pzfx files
#'
#' `read_pzfx()` generates a tidy data frame out of the data table contained in the pzfx file.
#'
#' @param path Path to the pzfx file.
#'
#' @param data_table data table to read. Either a string (the name of a data table), or an integer (the position of the data table). Defaults to the first data table.
#'
#' @return A tibble / data frame
#'
#' @export
read_pzfx <- function(path, data_table = 1) {

  data_table <- standardise_data_table(path, data_table)

  xml <- read_xml(path)
  xml_dt <- xml_find_all(xml, paste0("//d1:GraphPadPrismFile/d1:Table[", data_table, "]"))
  y_columns <- xml_find_all(xml_dt, "./d1:YColumn")

  n_subcolumns <- as.numeric(xml_attr(y_columns, "Subcolumns"))
  n_cells <- xml_length(xml_find_all(y_columns, "./d1:Subcolumn"))

  # Check if data table is empty...
  if (sum(n_cells) == 0) return(fake_tibble())

  #column_idx <- unlist(mapply(rep_len, seq_along(n_subcolumns), n_subcolumns, SIMPLIFY = FALSE))
  #column_idx <- unlist(mapply(rep_len, column_idx, n_cells, SIMPLIFY = FALSE))
  column_idx <- unlist(mapply(rep_len, rep(seq_along(n_subcolumns), n_subcolumns), n_cells, SIMPLIFY = FALSE))

  # Y Columns
  column_names <- rep(rep(get_column_titles(y_columns), n_subcolumns), n_cells)

  subcolumn_idx <- unlist(lapply(n_subcolumns, seq_len))
  subcolumn_idx <- unlist(mapply(rep_len, subcolumn_idx, n_cells, SIMPLIFY = FALSE))

  subcolumn_names <- get_subcolumn_titles(xml_dt)

  if (is.null(subcolumn_names)) {
    subcolumn_names <- NA_character_
  } else {
    subcolumn_names <- rep(rep(subcolumn_names, length(y_columns)), n_cells)
  }

  y_values <- get_subcolumn_values(y_columns)

  x_columns <- get_x_columns(xml_dt)

  # Using the number of rows in each subcolumn, we generate the row ids and appropriate x_columns
  x_columns <- c(list(row_id = unlist(lapply(n_cells, seq_len))),
                 lapply(x_columns, function(x) unlist(lapply(n_cells, function(y) x[0:y]))))

  out_df <- fake_tibble(x_columns,
                        column = column_idx,
                        column_name = column_names,
                        subcolumn = subcolumn_idx,
                        subcolumn_name = subcolumn_names,
                        y_value = y_values)

  out_df[, colSums(is.na(out_df)) < nrow(out_df)]
}
