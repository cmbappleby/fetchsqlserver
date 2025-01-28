#' @title Update data list data frame
#'
#' @description
#' Use on the large list returned from
#' \code{\link[fetchsqlserver]{fetch_sql_server_data}}
#' . After a data table or attribute table has been pulled from the large list
#' and modified, this function replaces the table(s) with the modified tables.
#'  There is no need to use this function if you work with the data or attribute
#'  table in the large list.
#'
#'
#' @param table_name A string of the table name associated with the data table
#' data frame or attribute table data frame as it appears in the dataFile
#' column of the table names and descriptions data frame (data_list[[3]]).
#' @param data_list A large list from
#' \code{\link[fetchsqlserver]{fetch_sql_server_data}}.
#' @param data_df A data table data frame extracted from the data_list using
#' \code{\link{get_tbl_as_df}} that has been modified and is to replace the
#' data table data frame in the data_list.
#' @param attr_df An attribute table data frame extracted from the data_list
#' using \code{\link{get_tbl_as_df}} that has been modified and is to replace
#' the attribute table data frame in the data_list.
#'
#' @returns A large list (data_list) with the updated data frame(s).
#' 
#' @export
#'
update_data_list <-
  function(table_name, data_list, data_df = NULL, attr_df = NULL) {
    # Need the index of the table to update the list
    i <- match(table_name, data_list[[3]]$dataFile)

    if (!is.null(data_df)) {
      data_list[[1]][[i]] <- data_df
    }
    if (!is.null(attr_df)) {
      data_list[[2]][[i]] <- attr_df
    }

    return(data_list)
  }

#' @title Remove table from data list
#'
#' @description
#' Use on the large list returned from
#' \code{\link[fetchsqlserver]{fetch_sql_server_data}}
#' . If a data table and it's associated attribute table and table description
#' are no longer needed and will not be published with the data package, this
#' function will remove the data table
#' (data_list[[1]]), attribute table (data_list[[2]]), and table
#' description (data_list[[3]]) from the large list.
#'
#'
#' @param table_name A string of the table name associated with the data table
#' data frame or attribute table data frame as it appears in the dataFile
#' column of the table names and descriptions data frame (data_list[[3]]).
#' @param data_list A large list from
#' \code{\link[fetchsqlserver]{fetch_sql_server_data}}.
#'
#' @returns A large list (data_list) with the specified table data removed.
#' 
#' @export
#'
remove_from_data_list <- function(table_name, data_list) {
  # Need the index of the table to update the list
  i <- match(table_name, data_list[[3]]$dataFile)

  data_list[[1]] <- data_list[[1]][-i]
  data_list[[2]] <- data_list[[2]][-i]
  data_list[[3]] <- data_list[[3]][drop = FALSE, -i, ]

  return(data_list)
}

#' @title Get table from data_list as a data frame
#'
#' @description
#' Gets either a data table or attribute table from the large list returned from
#'  \code{\link[fetchsqlserver]{fetch_sql_server_data}}. This allows you to
#' work with either or both tables more easily than working with them while they
#'  are in the large list.
#'
#'
#' @param table_name A string of the table name associated with the data table
#' data frame or attribute table data frame as it appears in the dataFile
#' column of the table names and descriptions data frame (data_list[[3]]).
#' @param table_type A string with the type of table you wish to retrieve from
#' the data_list, either \code{'data'} for a data table or \code{'attr'} for an
#' attribute table. The default is \code{'data'}.
#' @param data_list A large list from
#' \code{\link[fetchsqlserver]{fetch_sql_server_data}}.
#'
#' @returns A data frame with either the specified data or attribute table
#' pulled from the large list (data_list).
#' 
#' @export
#'
get_tbl_as_df <- function(table_name, table_type = "data", data_list) {
  table_names_list <- data_list[[3]]$dataFile
  # Need to grab the right table type
  if (table_type == "data") {
    tables_list <- data_list[[1]]
  } else if (table_type == "attr") { # or get the list of attribute tables
    tables_list <- data_list[[2]]
  } else {
    stop("Table type must be 'data' or 'attr'.")
  }

  # Need the index of the table to update the list
  i <- match(table_name, table_names_list)
  df <- tables_list[[i]]

  return(df)
}

#' @title Rearrange attribute table to match data table
#'
#' @description
#' After deleting and/or rearranging columns in the data table,
#' \code{rearrange_attr_tbl} will update the attribute table to have the same
#' columns in the same order as the data table.
#'
#'
#' @param data_df A data table data frame extracted from the data_list using
#' \code{\link{get_tbl_as_df}} that has been modified.
#' @param attr_df An attribute table data frame extracted from the data_list
#' using \code{\link{get_tbl_as_df}}.
#'
#' @returns An attribute table data frame with attributeNames matching the input
#' data table data frame.
#' 
#' @export
#'
rearrange_attr_tbl <- function(data_df, attr_df) {
  # Hold col desc as we rearrange them
  new_attr_df <-
    data.frame(matrix(nrow = ncol(data_df), ncol = ncol(attr_df)))
  colnames(new_attr_df) <- colnames(attr_df)

  for (i in seq_along(colnames(data_df))) {
    rnum <- match(colnames(data_df)[i], attr_df$attributeName)
    new_attr_df[i, ] <- attr_df[rnum, ]
  }

  return(new_attr_df)
}
