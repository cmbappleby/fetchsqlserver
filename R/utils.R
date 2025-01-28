#' @title Save data pkg info Excel spreadsheet template
#'
#' @description
#' Saves the data pkg info spreadsheet that is to be
#' filled out by the user containing information needed by many functions in the
#'  \code{fetchsqlserver} package. The first sheet, overall, is for information
#' such as server and database names, park units, date format, etc. The second
#' sheet, custom_units, is for any custom units that are used in the
#' attribute tables for metadata creation. The template already contains four
#' custom units, which can be deleted at the user's discretion. These are
#' provided as an example of how to fill out the sheet. Any units not
#' found in the EMLassemblyline unit dictionary (also available in the third
#' sheet, units_list) must be added to custom_units_table.csv.
#'
#'
#' @export
#'
save_data_info_template <- function() {
  data_pkg_path <- get_pkg_wd()
  message("This function will save the template for ",
          "data_pkg_info.xlsx, in the working directory.\n",
          "Is this the correct working directory?\n", data_pkg_path)

  correct_wd <- get_user_response()

  if (correct_wd == "No") {
    set_pkg_wd()
    data_pkg_path <- get_pkg_wd()
  }

  src_folder <- system.file("data_pkg_info_template",
                            package = 'fetchsqlserver')

  if (src_folder == "") {
    stop("The data pkg info file was not found!")
  }

  files <- list.files(src_folder, full.names = TRUE)
  file.copy(files, data_pkg_path, overwrite = TRUE)

  cat("The data pkg info file saved to", data_pkg_path, "\n")
}

#' @title Save core metadata templates
#'
#' @description
#' Core metadata files (abstract,
#' additional info, methods, intellectual rights, personnel, keywords, and
#' possibly custom units) are saved to the data package folder for EML metadata
#' creation. You should
#' check the core metadata files to ensure they filled out properly and contain
#' all the information necessary for the data package before running
#' \code{\link{create_eml_metadata}}.
#'
#'
#' @param data_pkg_folder (optional) A string of the folder path for the data
#' package where data tables, attribute tables, categorical variable tables,
#' and metadata files will be saved. If not provided, the user will be prompted
#' to select a folder.
#'
#' @export
#'
save_metadata_templates <- function(data_pkg_folder=NULL) {
  # make sure data_pkg_folder is a directory
  if (is.null(data_pkg_folder)) {
    data_pkg_folder <- svDialogs::dlg_dir(
      default = getwd(),
      title = "Select folder to save core metadata files")$res
  }

  while (length(data_pkg_folder) == 0) {
    data_pkg_folder <- svDialogs::dlg_dir(
      default = getwd(),
      title = "Select folder to save core metadata files")$res
  }

  .data_pkg_folder <<- data_pkg_folder

  cat(paste0("WARNING, any existing core metadata files in this folder, ",
             data_pkg_folder, " will be overwritten! Proceed?\n"))
  # get the response
  proceed <- get_user_response()

  if (proceed == "No") {
    stop("`save_metadata_templates` aborted by user.")
  }

  # get the protocol-specific core metadata folder path
  src_folder <- system.file("core_metadata_templates",
                            package = "fetchsqlserver")

  # if it doesn't exist, then we got a problem, like Christina sucks
  if (src_folder == "") {
    stop("The core metadata templates are missing. Try using ",
         "`EMLasseblyline::template_core_metadata()` instead.")
  }

  # get a list of the core metadata files
  files <- list.files(src_folder, full.names = TRUE)

  # copy each file to the destination folder
  file.copy(files, data_pkg_folder, overwrite = TRUE)

  cat(paste0("Core metadata template files",
             " have been successfully copied to ", data_pkg_folder,
             "! Ensure these files are filled out before running ",
             "`create_eml_metadata()`.",
             " Core metadata files include: ",
             "abstract.txt, additional_info.txt, custom_units.txt, ",
             "intellectual_rights.txt, keywords.xlsx, methods.txt, ",
             "and personnel.xlsx.\n"))
}

#' @title Read data pkg info Excel spreadsheet
#'
#' @description
#' Once the data pkg info spreadsheet has been filled out by the user (see
#' \code{\link{save_data_info_template()}}), this function reads both sheets in
#' the spreadsheet and saves the information in hidden hash dictionaries and
#' data frames. If either of the sheets are not saved/set, a message will appear
#'  prompting the user to run the function for that individual sheet.
#'
#'
#' @export
#'
read_data_pkg_info <- function() {
  # The Excel file contains information that will be read and
  # store for use by many functions in the package.
  data_pkg_info_path <- svDialogs::dlg_open(
    default = get_pkg_wd(),
    title = "Select data pkg info Excel spreadsheet",
    multiple = FALSE)$res

  read_overall_sheet(data_pkg_info_path)
  read_cust_units_sheet(data_pkg_info_path)
}

#' @title Read overall sheet in data pkg info Excel Spreadsheet
#'
#' @description
#' Once the data pkg info spreadsheet has been filled out by the user (see
#' \code{\link{save_templates()}}), the function
#' \code{\link{read_data_pkg_info()}} should be ran to read the contents. If
#' changes were made to the overall info after running that function, this
#' function needs to be ran to update the overall info.
#' This function reads the XLSX file
#' and saves the overall information in a hidden hash dictionary. The overall
#' information is used by many functions in the \code{fetchsqlserver} package.
#'
#'
#' @export
#'
read_overall_sheet <- function(data_pkg_info_path=NULL) {
  # The Excel file contains information that will be read and
  # store for use by many functions in the package.
  if (is.null(data_pkg_info_path)) {
    data_pkg_info_path <- svDialogs::dlg_open(
      default = get_pkg_wd(),
      title = "Select data pkg info Excel spreadsheet",
      multiple = FALSE)$res
  }

  # The first sheet contains the overall info
  overall_info_sheet <- readxl::read_xlsx(data_pkg_info_path,
                                          sheet = 1,
                                          col_names = TRUE,
                                          trim_ws = TRUE)

  # A hash dictionary is used to store the overall info for the data package
  overall_info <- hash::hash()

  # There should only be one 'value' per column with the exception of ParkUnits
  # and ProducingUnits.
  overall_info[["server_name"]] <- overall_info_sheet$ServerName[1]
  overall_info[["db_name"]] <- overall_info_sheet$DBName[1]
  overall_info[["project_name"]] <- overall_info_sheet$ProjectName[1]
  overall_info[["project_ref"]] <- overall_info_sheet$ProjectReference[1]
  overall_info[["collection_status"]] <- overall_info_sheet$CollectionStatus[1]
  overall_info[["network_code"]] <- overall_info_sheet$NetworkCode[1]
  overall_info[["producing_units"]] <-
    na.omit(overall_info_sheet$ProducingUnits)
  overall_info[["park_units"]] <- na.omit(overall_info_sheet$ParkUnits)
  overall_info[["date_format"]] <- overall_info_sheet$DateFormat[1]
  overall_info[["time_format"]] <- overall_info_sheet$TimeFormat[1]
  overall_info[["date_time_format"]] <- overall_info_sheet$DateTimeFormat[1]
  overall_info[["mvc"]] <- overall_info_sheet$MVC[1]
  overall_info[["mvce"]] <- overall_info_sheet$MVCE[1]

  .overall_info <<- hash::copy(overall_info)

  overall_info_saved <- exists(".overall_info", envir = .GlobalEnv)

  if (overall_info_saved) {
    got_overall_info <- get(".overall_info", envir = .GlobalEnv)
    cat("Please verify the following information:\n")

    overall_info_keys <- hash::keys(overall_info)
    overall_info_values <- hash::values(overall_info)

    for (i in seq_along(overall_info_keys)) {
      cat(overall_info_keys[i], ":", overall_info_values[[i]], "\n")
    }

    msg <- paste("If the overall information is incorrect, you must run",
                 "`read_overall_sheet()`.")
    message(msg)
  } else {
    message("Something went wrong and the overall information ",
            "was not set. You must run `read_overall_sheet()`.")
  }
}

#' @title Read custom units sheet in data pkg info Excel Spreadsheet
#'
#' @description
#' Once the data pkg info spreadsheet has been filled out by the user (see
#' \code{\link{save_templates()}}), the function
#' \code{\link{read_data_pkg_info()}} should be ran to read the contents. If
#' changes were made to the custom units after running that function, this
#' function needs to be ran to update the custom units. This function reads the
#' XLSX file and saves the custom units in a hidden data frame. The data frame
#' is used by \code{\link{add_units()}}.
#'
#'
#' @export
#'
read_cust_units_sheet <- function(data_pkg_info_path=NULL) {
  # The Excel file contains information that will be read and
  # store for use by many functions in the package.
  if (is.null(data_pkg_info_path)) {
    data_pkg_info_path <- svDialogs::dlg_open(
      default = get_pkg_wd(),
      title = "Select data pkg info Excel spreadsheet",
      multiple = FALSE)$res
  }

  cust_units <- readxl::read_xlsx(data_pkg_info_path,
                                  sheet = 2,
                                  col_names = TRUE,
                                  trim_ws = TRUE)

  if (nrow(cust_units) == 0) {
    stop("No custom units were found. Custom units were not set.")
  } else {
    .cust_units <<- cust_units
  }

  cust_units_saved <- exists(".cust_units", envir = .GlobalEnv)

  if (cust_units_saved) {
    cat("Custom units have been set!\n")
  } else {
    message("Something went wrong and the custom units ",
            "were not set. You must run `read_cust_units_sheet()`.")
  }
}

#' @title Set package working directory
#'
#' @description
#' This function does the same thing as \code{setwd()}, but it allows the user
#' to use the File Explorer to select the directory instead of typing out the
#' folder path or copying and pasting. That way the user doesn't have to worry
#' about which slashes they use (forward or back?), how many slashes, or
#' mistyping the path.
#'
#'
#' @export
#'
set_pkg_wd <- function() {
  msg <- paste0("The working directory should hold the queries folder, the ",
                "data package folder, and the data pkg info Excel spreadsheet. ",
                "This is also where the catvar list and field unit dictionary ",
                "CSVs will be saved by `fetch_sql_server_data` or ",
                "`fetch_attr_tables`.")

  svDialogs::dlg_message(msg, type = "ok")
  data_pkg_path <- svDialogs::dlg_dir(default = getwd(),
                                      title="Select the working directory")$res

  while (length(data_pkg_path) == 0) {
    data_pkg_path <- svDialogs::dlg_dir(
      default = getwd(),
      title = "Select the working directory")$res
  }

  .data_pkg_path <<- data_pkg_path
}

#' @title Get hidden data_pkg_path variable
#'
#' @description
#' When \code{set_pkg_wd()} is ran, it saves the package directory as a hidden
#' global variable. This function retrieves that value.
#'
#'
#' @returns A string with the data package path
#'
#' @noRd
#'
get_pkg_wd <- function() {
  if (exists(".data_pkg_path", envir = .GlobalEnv)) {
    data_pkg_path <- get(".data_pkg_path", envir = .GlobalEnv)

    while (length(data_pkg_path) == 0) {
      data_pkg_path <- svDialogs::dlg_dir(
        default = getwd(),
        title = "Select data package directory")$res

      .data_pkg_path <<- data_pkg_path
    }

    return(data_pkg_path)

  } else {
    message("The data package directory has not been set. ",
            "Run `set_pkg_wd()` to set it.")

    return(NULL)
  }
}

#' @title Get hidden data_pkg_folder variable
#'
#' @description
#' When \code{save_metdata_templates()} is ran, the folder path where the
#' templates were saved to is saved as a hidden global variable. This function
#' retrieves that value.
#'
#'
#' @returns A string with the data package folder path
#'
#' @noRd
#'
get_data_pkg_folder <- function() {
  if (exists(".data_pkg_folder", envir = .GlobalEnv)) {
    data_pkg_folder <- get(".data_pkg_folder", envir = .GlobalEnv)
  } else {
    msg <- paste0("The data package directory has not been set. ",
            "It is usually set when `save_metadata_templates()` is run. ",
            "Please select the folder where all data package files should ",
            "be saved (attribute tables, data tables, metadata files, etc.)")

    svDialogs::dlg_message(msg, type = "ok")
    data_pkg_folder <- svDialogs::dlg_dir(
      default = get_pkg_wd(),
      title = "Select folder for data package files")$res

    while (length(data_pkg_folder) == 0) {
      data_pkg_folder <- svDialogs::dlg_dir(
        default = getwd(),
        title = "Select the data package directory")$res
    }

    .data_pkg_folder <<- data_pkg_folder
  }

  return(data_pkg_folder)
}

#' @title Get overall data package info from hidden hash dictionary
#'
#' @param overall_info_key A string of the key for the overall info that
#' is needed.
#'
#' @returns A string or vector of the requested overall info or \code{NULL}
#' if the data package info has not been read.
#'
#' @noRd
#'
get_overall_info <- function(overall_info_key) {
  if (exists(".overall_info", envir = .GlobalEnv)) {
    got_overall_info <- get(".overall_info", envir = .GlobalEnv)
    overall_info_value <- got_overall_info[[overall_info_key]]

    return(overall_info_value)

  } else {
    message("The data package info has not been read and saved. ",
            "You must run `read_data_pkg_info()` to proceed.")

    return(NULL)
  }
}

#' @title Get a user response
#'
#' @description
#' Generates a menu prompt and gets a response. It checks the response to
#' see if it is an integer and if it is an acceptable integer. Otherwise, it
#' keeps asking for a valid response. Uses the integer to index the
#' \code{choices_list} and returns the value.
#'
#'
#' @param choices_list (optional) A vector of choices for the user to choose
#' from. Default is \code{c('No', 'Yes')}.
#'
#' @returns The user's selection from the \code{choices_list}.
#'
#' @noRd
get_user_response <- function(choices_list=c("No", "Yes")) {
  valid <- FALSE

  while (!valid) {
    user_input <- menu(choices_list)

    tryCatch({
      suppressWarnings(user_resp <- as.integer(user_input))
      if (user_resp %in% seq_along(choices_list)) {
        valid <- TRUE  # Set flag to exit the loop
      } else {
        cat("Invalid input. Please enter a valid number.\n")
      }
    }, error = function(e) {
      cat("Invalid input. Please enter a valid number.\n")
    })
  }

  display_response(choices_list[user_resp])

  return(choices_list[user_resp])
}

#' @title Get multiple user responses
#'
#' @description
#' Uses \code{\link{get_user_response}} and loops to allow the user to select
#' more than one value from the \code{choices_list}.
#'
#'
#' @param choices_list A vector of choices for the user to choose from.
#'
#' @returns The user's selections from the \code{choices_list}.
#'
#' @noRd
#'
get_user_response_multi <- function(choices_list) {
  more_select <- TRUE
  # Vector to hold user responses
  user_resps <- c()

  while (more_select) {
    user_resps <- c(user_resps, get_user_response(choices_list))
    cat("Do you need to make another selection?\n")
    if (get_user_response() == "No") {
      more_select <- FALSE
    }
  }

  display_response(user_resps)

  return(user_resps)
}

#' Visually outputs user selection
#'
#' @param user_selection
#'
#' @noRd
#'
display_response <- function(user_resp) {
  cat("User selected:", user_resp, "\n\n")
}
