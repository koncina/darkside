#' @import xml2
#' @importFrom utils type.convert
#' @importFrom stats setNames

NULL

extract_subcolumn <- function(x) {
  cells <- xml_find_all(x, "./d1:d")
  cells <- xml_text(cells)
  type.convert(cells, na.strings = c("NA", ""), as.is = TRUE, dec = ",") # Is the decimal point always "," in pzfx files?
}

extract_column <- function(x) {
  column <- xml_find_all(x, "./d1:Subcolumn")
  column <- lapply(column, extract_subcolumn)

  group_name <- xml_text(xml_find_all(x, "./d1:Title"))

  if (isTRUE(nchar(group_name) > 0)) {
    attr(column, "group_name") <- group_name
  }

  column
}

column_to_data_frame <- function(x, n_rows, x_columns) {
  # using seq_len instead of seq_along to create homogene number of rows
  # Doing it here will preserve attributes and makes the life easier...

  rows <- data.frame(row = seq_len(n_rows), stringsAsFactors = FALSE)
  if(nrow(x_columns) > 0) rows <- cbind(rows, x_columns, stringsAsFactors = FALSE)

  column <- data.frame(column = NA_integer_, stringsAsFactors = FALSE)
  if (!is.null(attr(x, "group_name"))) column <- cbind(column, group_name = attr(x, "group_name"), stringsAsFactors = FALSE)

  lapply(seq_along(x), function(i) data.frame(rows,
                                              column,
                                              subcolumn = i,
                                              y_value =  (`length<-`(x[[i]], n_rows)),
                                              stringsAsFactors = FALSE
  )
  )

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
  xml <- read_xml(path)

  pzfx_dt <- pzfx_tables(path)

  if (is.character(data_table)) data_table <- which(pzfx_dt == data_table)
  if (is.numeric(data_table) && !isTRUE(data_table %in% seq_along(pzfx_dt))) stop("data_table index out of range")

  xml_dt <- xml_find_all(xml, "//d1:GraphPadPrismFile/d1:Table")[[data_table]]

  # get the number of rows to fill each vector with NA if required
  n_rows <- max(xml_length(xml_find_all(xml_dt,  "//d1:Subcolumn")))


  xml <- xml_find_all(xml_dt, "./d1:RowTitlesColumn|./d1:XColumn|./d1:YColumn")

  column_types <- lapply(xml, xml_name)
  xml <- setNames(xml, column_types)

  columns <- lapply(xml, extract_column)
  # We are not keeping the group_name attrtibute doing it here...
  #columns <- lapply(columns, lapply, `length<-`, n_rows)

  x_columns <- lapply(columns[names(columns) != "YColumn"], unlist)
  #if (length(x_columns) == 0) x_columns <- list(RowTitlesColumn = NA_character_)

  x_columns <- lapply(x_columns, `length<-`, n_rows)
  #x_columns <- do.call("cbind", x_columns)
  x_columns <- as.data.frame(x_columns, stringsAsFactors = FALSE)

  y_columns <- columns[names(columns) == "YColumn"]

  out <- lapply(y_columns, column_to_data_frame, n_rows, x_columns)
  out <- lapply(out, function(x) do.call("rbind", x))
  #out <- lapply(seq_along(out), function(x) cbind(out[[x]], column = x))
  out <- lapply(seq_along(out), function(x) {out[[x]]["column"] <- x; out[[x]]})
  out <- do.call("rbind", out)

  colnames(out)[colnames(out) == "RowTitlesColumn"] <- "row_name"
  x_colname <- attr(columns[["XColumn"]], "group_name")
  if (!is.null(x_colname)) {
    colnames(out)[colnames(out) == "XColumn"] <- x_colname
  } else {
    colnames(out)[colnames(out) == "XColumn"] <- "x_value"
  }

  class(out) <- c("tbl_df", "tbl", "data.frame") # Setting tibble class to allow pretty printing without tibble dependency
  out
}
