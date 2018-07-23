#' @import xml2
#' @importFrom utils type.convert
#' @importFrom stats setNames

NULL

get_column_titles <- function(column_nodes) {
  title_nodes <- xml_find_all(column_nodes, "./d1:Title")
  column_ids <- as.numeric(gsub("^.*\\[(\\d+)\\]\\/\\*\\[\\d+\\]$", "\\1", xml_path(title_nodes)))
  column_ids <- column_ids - as.numeric(gsub(".*\\[(\\d+)\\]$", "\\1", xml_path(column_nodes[1]))) + 1
  column_titles <- character(length(column_nodes))
  column_titles[column_ids] <- xml_text(title_nodes)
  column_titles[column_titles == ""]  <- NA_character_
  column_titles
}

# Most of the time subcolumns contain different observations or timepoints
# But prism allows also to enter already calculated error values (SD, SEM...)
# We need to provide this information as subcolumn_names

get_subcolumn_titles <- function(table_node) {
  switch(xml_attr(table_node, "YFormat"),
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

get_subcolumn_values <- function(column_nodes, subcolumn = "*") {
  cell_values <- xml_text(xml_find_all(column_nodes, paste0("./d1:Subcolumn[", subcolumn, "]/d1:d")))
  type.convert(cell_values, na.strings = c("NA", ""), as.is = TRUE, dec = ",") # Is the decimal point always "," in pzfx files?
}

get_x_columns <- function(table_node) {

  x_columns <- list()

  # We could first test if the node is present but this seems to be faster...
  row_names <- xml_text(xml_find_all(table_node, "./d1:RowTitlesColumn/d1:Subcolumn/d1:d"))
  if (length(row_names) > 0) x_columns <- list(row_name = row_names)

  if (xml_attr(table_node, "XFormat") != "none") {
    x_column_node <- xml_find_first(table_node, "./d1:XColumn")
    x_name <- xml_text(xml_find_all(x_column_node, "./d1:Title"))

    if (nchar(x_name) == 0 || length(x_name) == 0) x_name <- "x_value"

    x_values <- get_subcolumn_values(x_column_node)
    if (length(x_values) > 0) x_columns[[x_name]] <- x_values
  }

  if (xml_attr(table_node, "XFormat") == "startenddate") {
    x_advanced_column_node <- xml_find_first(table_node, "./d1:XAdvancedColumn")
    x_advanced_columns <- lapply(c(start_date = 1, end_data = 2), function(x) get_subcolumn_values(x_advanced_column_node, x))
    x_columns <- c(x_columns, x_advanced_columns)
  }

  x_columns
}

standardise_data_table <- function(path, data_table) {
  if (length(data_table) != 1) {
    stop("`data_table` must have length 1", call. = FALSE)
  }

  table_names <- pzfx_tables(path)

  if (is.numeric(data_table)) {
    if (data_table < 1 || data_table > length(table_names)) stop("`data_table` index out of range", call. = FALSE)
    data_table_index <- data_table
  } else if (is.character(data_table)) {
    data_table_index <- which(table_names == data_table)

    if (length(data_table_index) == 0) stop(sprintf("data table '%s' not found", data_table), call. = FALSE)
    else if (length(data_table_index) > 1) {
      stop(sprintf("multiple data tables are named '%s': select the data table by index", data_table), call. = FALSE)
    }
  } else stop("`data_table` must be either an integer or a string.", call. = FALSE)
  data_table_index
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
  table_name_nodes <- xml_find_all(xml, "//d1:GraphPadPrismFile/d1:Table/d1:Title")
  xml_text(xml_contents(table_name_nodes))
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
  table_node <- xml_find_all(xml, paste0("//d1:GraphPadPrismFile/d1:Table[", data_table, "]"))

  y_column_nodes <- xml_find_all(table_node, "./d1:YColumn")

  # vector with the number of subcolumns in each column
  n_subcolumns <- as.numeric(xml_attr(y_column_nodes, "Subcolumns"))

  # vector with the number of rows / cells in each subcolumn
  n_rows <- xml_length(xml_find_all(y_column_nodes, "./d1:Subcolumn"))

  # Check if data table is empty...
  if (sum(n_rows) == 0) return(fake_tibble())

  # Y Columns
  column_ids <- unlist(mapply(rep_len, rep(seq_along(n_subcolumns), n_subcolumns), n_rows, SIMPLIFY = FALSE))
  column_names <- rep(rep(get_column_titles(y_column_nodes), n_subcolumns), n_rows)

  subcolumn_ids <- unlist(lapply(n_subcolumns, seq_len))
  subcolumn_ids <- unlist(mapply(rep_len, subcolumn_ids, n_rows, SIMPLIFY = FALSE))

  subcolumn_names <- get_subcolumn_titles(table_node)

  if (is.null(subcolumn_names)) {
    subcolumn_names <- NA_character_
  } else {
    subcolumn_names <- rep(rep(subcolumn_names, length(y_column_nodes)), n_rows)
  }

  y_values <- get_subcolumn_values(y_column_nodes)

  x_columns <- get_x_columns(table_node)

  # We use the number of rows in each subcolumn to generate the row ids and finalise the x_columns
  x_columns <- c(list(row_id = unlist(lapply(n_rows, seq_len))),
                 lapply(x_columns, function(x) unlist(lapply(n_rows, function(y) x[0:y]))))

  out_df <- fake_tibble(x_columns,
                        column = column_ids,
                        column_name = column_names,
                        subcolumn = subcolumn_ids,
                        subcolumn_name = subcolumn_names,
                        y_value = y_values)

  out_df[, colSums(is.na(out_df)) < nrow(out_df)]
}
