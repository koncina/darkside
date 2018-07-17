#' @import xml2
#' @importFrom utils type.convert

NULL

get_column_titles <- function(x) {
  xml_text(xml_find_all(x, "./d1:Title"))
}

get_subcolumn_values <- function(x) {
  values <- xml_text(xml_find_all(x, "./d1:Subcolumn/d1:d"))
  type.convert(values, na.strings = c("NA", ""), as.is = TRUE, dec = ",") # Is the decimal point always "," in pzfx files?
}

standardise_data_table <- function(path, data_table) {
  if (length(data_table) != 1) {
    stop("`data_table` must have length 1", call. = FALSE)
  }

  pzfx_dt <- pzfx_tables(path)

  if (is.numeric(data_table)) {
    if (data_table < 1 || data_table > length(pzfx_dt)) stop("`data_table` index out of range", call. = FALSE)
  } else if (is.character(data_table)) {
    if (data_table %in% pzfx_dt) data_table <- which(pzfx_dt == data_table)
    else stop(sprintf("data table '%s' not found", data_table), call. = FALSE)
  } else stop("`data_table` must be either an integer or a string.", call. = FALSE)
  data_table
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

  row_idx <- unlist(lapply(n_cells, seq_len))

  y_values <- get_subcolumn_values(y_columns)

  # X Column
  x_column <- xml_find_all(xml_dt, "./d1:XColumn")
  x_values <- get_subcolumn_values(x_column)
  x_values <- unlist(lapply(n_cells, function(x) x_values[0:x]))

  x_name <- xml_text(xml_find_all(x_column, "./d1:Title"))

  # Rownames
  row_names <- xml_text(xml_find_all(xml_dt, "./d1:RowTitlesColumn/d1:Subcolumn/d1:d"))
  row_names <- unlist(lapply(n_cells, function(x) row_names[0:x]))

  out_df <- fake_tibble(row = row_idx,
                        row_name = row_names,
                        x_value = x_values,
                        column = column_idx,
                        column_name = column_names,
                        subcolumn = subcolumn_idx,
                        y_value = y_values)

  if (isTRUE(nchar(x_name) > 0)) colnames(out_df)[colnames(out_df) == "x_value"] <- x_name

  out_df[, colSums(is.na(out_df)) < nrow(out_df)]
}
