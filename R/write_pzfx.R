NULL

#' Convert the tibble to a pzfx table XML node
#'
#' Convert a **wide** tibble to a pzfx table XML node.
#'
#' @param data tibble to be converted
#'
#' @param row_names if `NULL` (the default), converts the data frame row names to Prism row titles. If `character`, uses the provided column to generate the row titles.
#'
#' @param table_name `character` to specify the Prism table title. if `NULL` (the default), will use the name of `data`.
#'
#' @return A xml2 xml document
#'
as_column_table <- function(data, row_names = NULL, table_name = NULL) {

  row_names <- check_argument(row_names, c("character", "NULL"), 1)
  table_name <- check_argument(table_name, c("character", "NULL"), 1)

  row_name_v <- NULL
  if (isTRUE(row_names %in% colnames(data))) {
    row_name_v <- data[[row_names]]
    data <- data[-which(names(data) == row_names)]
  }
  if (is.null(row_names) && .row_names_info(data) > 0) {
    row_name_v <- rownames(data)
  }

  columns <- lapply(data,
                    function(i) list(
                      Subcolumn = setNames(
                        lapply(i, function(j) list(j)),
                        rep("d", length(i))
                      )
                    )
  )
  columns <- lapply(seq_along(columns),
                    function(i) structure(
                      c(list(
                        Title = list(names(columns[i]))
                      ),
                      columns[[i]]),
                      Subcolumns = "1"
                    )
  )
  columns <- setNames(columns,
                      rep("YColumn", length(columns))
  )




  if (!is.null(row_name_v)) {
    row_titles <- setNames(
      lapply(row_name_v,
             function(i) list(i)),
      rep("d", nrow(data))
    )
    row_titles <- list(
      RowTitlesColumn = list(Subcolumn = row_titles)
    )
    columns <- c(row_titles, columns)
  }

  pzfx_list <- list(
    Table = structure(
      c(list(Title = list(ifelse(is.null(table_name) || !is.character(table_name), deparse(substitute(x)), table_name))), columns),
      XFormat = "none",
      TableType = "OneWay")
  )

  as_xml_document(pzfx_list)
}

#' Write a data frame to a Graphpad prism pzfx file
#'
#' Writes out a `pzfx` file which can be opened using Graphpad Prism.
#'
#' @param data tibble to be converted
#'
#' @param path to write to
#'
#' @param row_names Name of column to use for rownames. If `NULL` (the default), uses row names if they exist.
#'
#' @param table_type Prism table type (column)
#'
#' @param table_name Name of the Prism table in the output document. if `NULL` (the default) the name of `data` is used.
#'
#' @return A xml2 xml document
#'
#' @export
write_pzfx <- function(data, path, table_type = "column", table_name = NULL, row_names = NULL) {
  pzfx_list <- list(
    Created = list(
      OriginalVersion = structure(
        list(),
        CreatedByProgram = "GraphPad Prism",
        CreatedByVersion="5.0")
    )
  )

  pzfx_list <- list(
    GraphPadPrismFile = structure(
      pzfx_list,
      xmlns = "http://graphpad.com/prism/Prism.htm",
      PrismXMLVersion = "5.00")
  )

  pzfx_file <- as_xml_document(pzfx_list)
  if (is.null(table_name))
    xml_table <- switch(table_type,
                        column = as_column_table(data,
                                                 table_name = ifelse(is.null(table_name), deparse(substitute(data)), table_name),
                                                 row_names = row_names))
  if (!is.null(xml_table)) xml_add_child(pzfx_file, xml_table)
  write_xml(pzfx_file, path)
}
