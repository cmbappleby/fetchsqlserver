#' @title Gather base EML metadata info (non-NPS-specific portion)
#'
#' @description
#' Gathers overall package details, taxonomic, geographic, and temporal
#' information by reading files and asking for user input.
#' data_names_descriptions.xlsx must be present in the data package folder,
#' which is added using \code{\link{save_data_pkg()}}. After running
#' \code{gather_eml_info()}, the next step is to run
#' \code{\link{create_eml_metadata()}}.
#'
#'
#' @param data_pkg_folder (optional) A string of the folder path for the data
#' package.
#'
#' @returns A hash dictionary containing the base metadata needed to create EML.
#'
#' @export
#'
gather_eml_info <- function(data_pkg_folder = NULL) {
  if (is.null(data_pkg_folder)) {
    data_pkg_folder <- get_data_pkg_folder()
  }

  if (!check_data_pkg_folder(data_pkg_folder)) {
    stop(
      "The folder path ", data_pkg_folder, " is not right. ",
      "Check the path and try again."
    )
  }

  if (exists("eml_info", envir = .GlobalEnv)) {
    replace_eml <- ask_replace_eml_info()
    if (replace_eml == "No") {
      stop("Existing eml info will not be replaced.")
    }
  }

  # Create a hash dictionary to store all the metadata info
  # that way the user doesn't have to mess with this stuff again if they
  # only need to change a couple of files.
  eml_info <- hash::hash()

  data_info <- get_data_files(data_pkg_folder)
  # data_files gets used later on
  data_files <- data_info[[1]]
  eml_info[["data_files"]] <- data_files
  eml_info[["data_urls"]] <- data_info[[2]]
  eml_info[["data_type"]] <- get_overall_info("collection_status")

  overall <- get_overall_details(data_pkg_folder)

  # There was no data_names_descriptions.xlsx file if overall is null
  if (is.null(overall)) {
    stop(
      "Need the data_names_descriptions.xlsx file in the data package ",
      "folder to continue."
    )
  } else {
    package_title <- overall[[1]]
    eml_info[["data_names"]] <- overall[[2]]
    eml_info[["data_descriptions"]] <- overall[[3]]
    metadata_id <- overall[[4]]
  }

  eml_info[["taxon"]] <- taxonomic_info(data_files, data_pkg_folder)

  eml_info[["geo"]] <- geographic_info(data_files, data_pkg_folder)

  temp_info <- temporal_info(
    data_files,
    package_title,
    metadata_id,
    data_pkg_folder
  )
  eml_info[["metadata_id"]] <- temp_info[[1]]
  eml_info[["package_title"]] <- temp_info[[2]]
  eml_info[["temp_dates"]] <- c(temp_info[[3]], temp_info[[4]])

  return(eml_info)
}

#' @title Create EML metadata (non-NPS-specific portion)
#'
#' @description
#' Creates the base EML metadata sans the NPS-specific stuff, which is added
#' using \code{\link{add_nps_metadata()}}. Ensures that all core metadata files
#' are present prior to creating an \code{eml_object}.
#'
#'
#' @param data_pkg_folder A string of the folder path for the data package
#' . If \code{save_data_pkg()} was run before running
#' \code{save_core_metadata}, then the data package folder path should be
#' saved as a global variable.
#' @param eml_info A hash dictionary created by
#' \code{\link{gather_eml_info()}} containing the base metadata needed
#'  to create EML.
#'
#' @returns A list containing metadata information such as overall package
#' details, geographic, taxonomic and temporal information, and core metadata
#' information. Also called an \code{eml_object} by
#' \code{\link[EMLeditor]{EMLeditor}}. Use as an input to
#' \code{\link{add_nps_metadata()}}.
#'
#' @export
#'
create_eml_metadata <- function(eml_info) {
  data_pkg_folder <- get_data_pkg_folder()

  # Cannot create metadata if the core files do not exist
  core_good <- check_core_files(data_pkg_folder)

  if (!core_good) {
    stop(
      "Metadata object not created. Add the missing files and rerun ",
      "`create_eml_metadata`."
    )
  }

  my_metadata <-
    EMLassemblyline::make_eml(
      path = data_pkg_folder,
      dataset.title = eml_info[["package_title"]],
      data.table = eml_info[["data_files"]],
      data.table.name = eml_info[["data_names"]],
      data.table.description = eml_info[["data_descriptions"]],
      data.table.url = eml_info[["data_urls"]],
      temporal.coverage = eml_info[["temp_dates"]],
      maintenance.description = eml_info[["data_type"]],
      package.id = eml_info[["metadata_id"]],
      return.obj = TRUE,
      write.file = FALSE
    )

  eml_valid <- check_valid_eml(my_metadata)

  if (!eml_valid) {
    message(
      "Despite the issues, an EML metadata object created. Use ",
      "`EMLassemblyline` or `EMLeditor` functions to fix the EML. ",
      "Do not attempt to edit it by hand. Alternatively, you can try ",
      "to fix the erroneous files and run ",
      "`create_eml_metadata again."
    )
  } else {
    cat("EML metadata creation successful!\n")
  }

  return(my_metadata)
}

#' @title Check for valid EML schema and possible issues
#'
#' @description
#' This primary purpose of this function is for it to be used by other functions
#'  to check the validity of the EML metadata prior to continuing with other
#' processes. However, this function can be used by the user to check that their
#'  EML metadata is valid after they make changes to it using
#' \code{\link[EMLassemblyline]{EMLassemblyline}} or
#' \code{\link[EMLeditor]{EMLeditor}} prior to continuing with the metadata
#' creation process.
#'
#'
#' @param my_metadata A list containing metadata information such as overall
#' package details, geographic, taxonomic and temporal information, core
#' metadata information, and possibly NPS-specific metadata. Also called an
#' \code{eml_object} by \code{EMLeditor}.
#'
#' @returns A Boolean of EML validity.
#'
#' @noRd
#'
check_valid_eml <- function(my_metadata) {
  eml_valid <- EML::eml_validate(my_metadata)

  cat(paste0(
    "Issues that are (optional) do not need to be fixed before ",
    "proceeding. All other issues should be fixed before generating ",
    "XML file.\n"
  ))
  cat(EMLassemblyline::issues())

  if (!eml_valid) {
    message(paste0(
      "Your EML is not schema valid. The following problem(s) ",
      "need(s) to be addressed before continuing:\n",
      attr(eml_valid, "errors\n")
    ))
  }

  return(eml_valid)
}

#' @title Check that core metadata files exist
#'
#' @description
#' The files in the data package folder are checked against a list of
#' necessary core metadata files (does not include attribute or catvar files).
#' Missing files are made known to the user. The primary purpose of this
#' function is to be used in \code{\link{create_eml_metadata()}} prior to
#' creating an \code{eml_object}.
#'
#' @param data_pkg_folder A string of the folder path for the data package.
#'
#'
#' @returns A Boolean indicating whether the core files exist.
#'
#' @noRd
#'
check_core_files <- function(data_pkg_folder) {
  if (!check_data_pkg_folder(data_pkg_folder)) {
    stop(
      "The folder path ", data_pkg_folder, " is not right. ",
      "Check the path and try again."
    )
  }

  all_files <- list.files(data_pkg_folder, full.names = FALSE)
  other_files <- all_files[!grepl("\\.csv$", all_files) &
    !grepl("\\.xml$", all_files) &
    !grepl("^attributes_", all_files) &
    !grepl("^catvars_", all_files)]

  # The custom_units file is not included because it is only required if custom
  # units exist.
  core_file_names <- c(
    "abstract.txt",
    "additional_info.txt",
    "intellectual_rights.txt",
    "methods.txt",
    "keywords.txt",
    "personnel.txt"
  )

  diff_files <- setdiff(core_file_names, other_files)

  # Variable to keep track
  core_good <- TRUE

  for (file_name in diff_files) {
    if (file_name == "keywords.txt") {
      # Might be an xlsx of the keywords that hasn't been converted
      if (file.exists(file.path(data_pkg_folder, "keywords.xlsx"))) {
        xlsx_to_tsv(data_pkg_folder, "keywords.xlsx")
      } else {
        core_good <- FALSE
        cat("Missing file: keywords.txt or keywords.xlsx\n")
      }
    } else if (file_name == "personnel.txt") {
      if (file.exists(file.path(data_pkg_folder, "personnel.xlsx"))) {
        xlsx_to_tsv(data_pkg_folder, "personnel.xlsx")
      } else {
        core_good <- FALSE
        cat("Missing file: personnel.txt or personnel.xlsx\n")
      }
    } else if (file_name %in% core_file_names) {
      core_good <- FALSE
      cat("Missing file: ", file_name, "\n")
    }
  }

  return(core_good)
}

#' @title Convert XLSX to TSV
#'
#' @description
#' Reads an Excel spreadsheet (.xlsx) into a data frame and saves the table as a
#'  tab-separated value (TSV) file. The primary purpose of this function is to
#' convert keywords.xlsx and personnel.xlsx to TSVs for EML metadata creation
#' and is used by \code{\link{check_core_files}}. However, the user can use
#' the function to convert the files before running
#' \code{\link{create_eml_metadata}} if they wish to.
#'
#'
#' @param data_pkg_folder A string containing the folder path of the file. This is
#' the folder the XLSX file will be read from and where the TSV will be saved
#' to.
#' @param xlsx_file_name A string containing the file name with extension.
#'
#' @noRd
#'
xlsx_to_tsv <- function(data_pkg_folder, xlsx_file_name) {
  fp <- file.path(data_pkg_folder, xlsx_file_name)

  if (!file.exists(fp)) {
    stop("Either ", data_pkg_folder, " or ", xlsx_file_name, " does not exist!")
  }

  xlsx_df <- readxl::read_excel(fp)
  tsv_fn <- sub("\\.xlsx$", ".txt", xlsx_file_name)
  tsv_fp <- file.path(data_pkg_folder, tsv_fn)
  fc <- file(tsv_fp, open = "wt")
  write.table(xlsx_df,
              file = tsv_fp,
              sep = "\t",
              quote = FALSE,
              row.names = FALSE
  )
  close(fc)

  message(
    xlsx_file_name, " successfully converted to a TSV. Saved as ",
    tsv_fn, " in ", data_pkg_folder
  )
}

#' @title Check \code{data_pkg_folder} path
#'
#' @description
#' Verifies that \code{data_pkg_folder} does exist and is a directory/folder
#' path. Used in metadata creation functions.
#'
#'
#' @param data_pkg_folder A string containing the path to the data package
#' folder
#'
#' @returns Boolean, TRUE if the path exists and is a folder, FALSE otherwise
#' 
#' @noRd
#'
check_data_pkg_folder <- function(data_pkg_folder) {
  file_info <- file.info(data_pkg_folder)

  is_dir <- file_info$isdir

  if (length(is_dir) > 0) {
    if (!is.na(is_dir)) {
      return(file_info$isdir)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' @title Get data file names and create URLs
#'
#' @description
#' Reads the data file names (CSVs) into a list, and creates a list of temporary
#'  URLs that are used for EML metadata creation. Should only be used by
#'  \code{\link{create_eml_metadata}}.
#'
#' @param data_pkg_folder A string of the folder path for the data package
#' . If \code{save_data_pkg} was run before running
#' \code{save_core_metadata}, then the data package folder path should be
#' saved as a global variable.
#'
#'
#' @returns A list containing [1] the list of data file names and [2] a list
#' of temporary URLs.
#'
#' @noRd
#'
get_data_files <- function(data_pkg_folder) {
  data_files <- list.files(data_pkg_folder, pattern = "*.csv")
  data_urls <- c(rep("temporary URL", length(data_files)))

  return(list(data_files, data_urls))
}

#' @title Get overall package details
#'
#' @description
#' Gets the project name from an internal dictionary and uses that to create
#' the package title. Also, gets the data type (ongoing or complete) from an
#' internal dictionary. Reads the data names and descriptions from the XLSX file
#'  saved from \code{\link{save_data_pkg}}.
#'
#'
#' @param data_pkg_folder A string of the folder path for the data package
#' . If \code{save_data_pkg} was run before running
#' \code{save_core_metadata}, then the data package folder path should be
#' saved as a global variable.
#'
#' @returns A list containing [1] the package title, [2] data type, [3]
#' data names, and [4] data descriptions. If the file
#' data_names_descriptions.xlsx does not exist, then \code{NULL} is
#' returned.
#'
#' @noRd
#'
get_overall_details <- function(data_pkg_folder) {
  project_name <- get_overall_info("project_name")
  network_code <- get_overall_info("network_code")

  # Create the beginnings of the package title and metadata id,
  # finish them when dates are determined.
  package_title <- paste(network_code, project_name, "Monitoring Data Package,")
  project_name <- gsub(" ", "", project_name)
  metadata_id <- paste0(network_code, "_", project_name, "_DataPackage_")

  desc_names_file <- file.path(
    data_pkg_folder,
    "data_names_descriptions.xlsx"
  )
  if (file.exists(desc_names_file)) {
    name_desc_xlsx <- readxl::read_excel(desc_names_file)

    # Reads the two columns into separate variables that will be used later
    data_names <- name_desc_xlsx$dataFile
    data_descriptions <- name_desc_xlsx$dataFileDescriptions

    return(list(
      package_title,
      data_names,
      data_descriptions,
      metadata_id
    ))
  } else {
    message(
      "The file data_names_descriptions.xlsx could not be found.",
      " The file should have been generated with `save_data_pkg`."
    )

    return(NULL)
  }
}

#' @title Get taxonomic information using user input
#'
#' @description
#' Checks to see if a taxonomic coverage file already exists, and if the user
#' wants to replace it or it does not exist, creates a taxonomic coverage file.
#' To do this, the user is asked to select the file(s) containing the taxonomic
#' information, as well as the column containing the scientific names. This is
#' fed to \code{\link[EMLassemblyline]{template_taxonomic_coverage}} to create
#'  the file.
#'
#'
#' @param data_files A list of the data file names from the data package folder.
#' @param data_pkg_folder A string of the folder path for the data package
#' . If \code{save_data_pkg} was run before running
#' \code{save_core_metadata}, then the data package folder path should be
#' saved as a global variable.
#'
#' @returns A list containing [[1]] the user-selected data taxa tables and [[2]]
#' the user-selected data taxa fields.
#'
#' @noRd
#'
taxonomic_info <- function(data_files, data_pkg_folder) {
  fp <- file.path(data_pkg_folder, "taxonomic_coverage.txt")

  if (file.exists(fp)) {
    message(
      "taxonomic_coverage.txt already exists. Do you wish to ",
      "rewrite the taxonomic coverage and replace this file?"
    )
    replace_taxon <- get_user_response()

    if (replace_taxon == "No") {
      return("Existing taxonomic_coverage.txt will be used. \n")
    } else {
      # Delete the file or EMLassemblyline won't write a new one
      file.remove(fp)
    }
  }

  cat("Select the taxon file(s):\n")
  data_taxa_tables <- get_user_response_multi(data_files)

  # Empty vector for taxa fields
  data_taxa_fields <- c()

  for (tbl in data_taxa_tables) {
    cat("Select the column(s) with the scientific name:\n")

    df <- read.csv(file.path(data_pkg_folder, tbl))
    field <- get_user_response(colnames(df))
    data_taxa_fields <- c(data_taxa_fields, field)
  }

  EMLassemblyline::template_taxonomic_coverage(
    path = data_pkg_folder,
    data.path = data_pkg_folder,
    taxa.table = data_taxa_tables,
    taxa.col = data_taxa_fields,
    taxa.authority = c(3, 11),
    taxa.name.type = "scientific",
    write.file = TRUE
  )

  if (file.exists(fp)) {
    cat(paste0(
      "taxonomic_coverage.txt has been written to ",
      data_pkg_folder, "\n"
    ))

    return(list(data_taxa_tables, data_taxa_fields))
  } else {
    stop(paste0(
      "Something went wrong and taxonomic_coverage.txt was not ",
      "written to ", data_pkg_folder, "\n"
    ))
  }
}

#' @title Get geographic information using user input
#'
#' @description
#' Checks to see if a geographic coverage file already exists, and if the user
#' wants to replace it or it does not exist, creates a geographic coverage file.
#' To do this, the user is asked to select the file(s) containing the geographic
#'  information, as well as the columns containing the site name, coordinates,
#' and datum. If coordinates are in UTM, they are converted to lat/long and
#' added to the data table using \code{\link{add_latlong_w_utm}}. This is fed
#' to \code{\link[EMLassemblyline]{template_geographic_coverage}} to create
#' the file.
#'
#'
#' @param data_files A list of the data file names from the data package folder.
#' @param data_pkg_folder A string of the folder path for the data package.
#'
#' @returns A list containing [[1]] the user-selected coordinates table, [[2]]
#' the user-selected site name, and [[3]] Boolean for whether lat/long
#' coordinates were added.
#'
#' @noRd
#'
geographic_info <- function(data_files, data_pkg_folder) {
  fp <- file.path(data_pkg_folder, "geographic_coverage.txt")

  if (file.exists(fp)) {
    message(
      "geographic_coverage.txt already exists. Do you wish to rewrite",
      " the geographic coverage and replace this file?\n"
    )
    replace_geo <- get_user_response()

    if (replace_geo == "No") {
      return("Existing geographic_coverage.txt will be used.\n")
    } else {
      # Delete the file or EMLassemblyline won't write a new one
      file.remove(fp)
    }
  }

  cat("Select the geo file(s):\n")
  data_coordinates_table <- get_user_response_multi(data_files)

  data_latitude <- c()
  data_longitude <- c()
  data_sitename <- c()
  latlong_added <- FALSE

  for (tbl in data_coordinates_table) {
    fn <- file.path(data_pkg_folder, tbl)
    df <- read.csv(fn)

    cat("Select the site name:\n")
    sitename <- get_user_response(colnames(df))
    data_sitename <- c(data_sitename, sitename)

    cat("Select the coordinate type:\n")
    coord_type <- get_user_response(c("Lat-long", "UTM"))

    if (coord_type == "UTM") {
      # add lat/long coordinates to geo table
      add_latlong_w_utm(df, fn)

      data_latitude <- c(data_latitude, "decimalLatitude")
      data_longitude <- c(data_longitude, "decimalLongitude")
    } else {
      cat("Select the column with Latitude:\n")
      lat <- get_user_response(colnames(df))
      data_latitude <- c(data_latitude, lat)
      cat("Select the column with Longitude:\n")
      long <- get_user_response(colnames(df))
      data_longitude <- c(data_longitude, long)
    }
  }

  EMLassemblyline::template_geographic_coverage(
    path = data_pkg_folder,
    data.path = data_pkg_folder,
    data.table = data_coordinates_table,
    lat.col = data_latitude,
    lon.col = data_longitude,
    site.col = data_sitename,
    write.file = TRUE
  )

  if (file.exists(fp)) {
    cat(paste0(
      "geographic_coverage.txt has been written to ",
      data_pkg_folder, ".\n"
    ))
    return(list(data_coordinates_table, data_sitename, latlong_added))
  } else {
    stop(paste0(
      "Something went wrong and geographic_coverage.txt was not ",
      "written to ", data_pkg_folder, ".\n"
    ))
  }
}

#' @title Get temporal information using user input
#'
#' @description
#' Has the user select the data table containing the temporal coverage for the
#' data package, as well as the columns containing the dates. Uses this to find
#' the start and end dates. Also, adds the start and end year to the package
#' title and creates the metadata ID.
#'
#'
#' @param data_files A list of the data file names from the data package folder.
#' @param package_title A string containing the data package title, sans the
#' years.
#' @param data_pkg_folder A string of the folder path for the data package.
#'
#' @returns A list containing [1] the metadata ID, [2] package title with the
#' coverage years added, [3] start date, [4] end date, and [[5]]
#' user selections.
#'
#' @noRd
#'
temporal_info <- function(data_files, package_title,
                          metadata_id, data_pkg_folder) {
  attr_files <- list.files(data_pkg_folder, pattern = "attributes_*")

  all_dates <- c()

  for (i in seq_along(data_files)) {
    attr_fn <- file.path(data_pkg_folder, attr_files[i])
    attr_df <- read.table(attr_fn, header = TRUE, sep = "\t")

    data_fn <- file.path(data_pkg_folder, data_files[i])
    data_df <- read.csv(data_fn, header = TRUE)

    attr_df %<>% dplyr::filter(class == "Date" &
                                 grepl("yy", tolower(dateTimeFormatString)))

    attr_names <- attr_df$attributeName

    for (attr_name in attr_names) {
      dates <- unlist(data_df[attr_name])
      dates <- as.Date(dates, format = "%Y-%m-%d")

      all_dates <- c(all_dates, dates)
    }
  }

  class(all_dates) <- "Date"

  startdate <- min(all_dates, na.rm = TRUE)
  enddate <- max(all_dates, na.rm = TRUE)

  startdate <- lubridate::ymd(startdate)
  enddate <- lubridate::ymd(enddate)

  start_yr <- substr(as.character(startdate), 1, 4)
  end_yr <- substr(as.character(enddate), 1, 4)

  years <- paste0(start_yr, "-", end_yr)
  package_title <- paste0(package_title, " ", years)
  metadata_id <- paste0(metadata_id, years, "_metadata")

  return(list(metadata_id,
              package_title,
              startdate,
              enddate))
}

#' @title Convert UTM coordinates to lat/long and add to data file
#'
#' @description
#' The user is asked to select the columns containing the coordinates, UTM zone,
#'  and datum, as well as which datum to use for the lat/long coordinates. This
#' data is fed to \code{\link[QCkit]{generate_ll_from_utm()}} to get the
#' lat/long coordinates, and then the updated data table CSV is replaced. This
#' function is used by \code{\link{geographic_info()}}.
#'
#'
#' @param df A dataframe of the data table containing the geographic information
#'  (coordinates, UTM zone, and datum).
#' @param fn A string of the file name of the data table containing the
#' geographic information.
#'
#' @noRd
#'
add_latlong_w_utm <- function(df, fn) {
  cat("Select the UTM Easting column:\n")
  easting <- get_user_response(colnames(df))
  cat("Select the UTM Northing column:\n")
  northing <- get_user_response(colnames(df))
  cat("Select the UTM Zone column:\n")
  utm_zone <- get_user_response(colnames(df))
  cat("Select the UTM Datum column:\n")
  utm_datum <- get_user_response(colnames(df))
  cat("Select the desired Lat-Long datum column:\n")
  latlong_datum <- get_user_response(c("NAD83", "WGS84"))

  latlong_df <- QCkit::generate_ll_from_utm(
    df,
    {{ easting }},
    {{ northing }},
    {{ utm_zone }},
    {{ utm_datum }},
    {{ latlong_datum }}
  )

  latlong_df$LatLong_CRS <- latlong_datum

  write.csv(latlong_df, fn)

  message("Lat and long coordinates added!")
}

#' @title Ask user if they want to replace eml_info
#'
#' @description
#' Displays all the existing eml_info for the user so they can decide if they
#' want to replace it or not, and gets their response. Used by
#' \code{\link{gather_eml_info}}
#'
#'
#' @returns A string of "yes" or "no"
#' 
#' @noRd
#'
ask_replace_eml_info <- function() {
  eml_info <- get("eml_info", envir = .GlobalEnv)

  # Create a message to show the user to see if they want to replace eml_info
  msg <- paste0(
    "The following eml info was saved:\n",
    "Taxon table(s): ", eml_info[["taxon"]][1], "\n",
    "Taxon field(s): ", eml_info[["taxon"]][2], "\n",
    "Coordinates table(s): ", eml_info[["geo"]][1], "\n",
    "Site name: ", eml_info[["geo"]][2], "\n",
    "Lat/long added: ", eml_info[["geo"]][3], "\n",
    "Start date: ", eml_info[["temp_dates"]][1], "\n",
    "End date: ", eml_info[["temp_dates"]][2], "\n",
    "\nWould you like to replace it?"
  )

  message(msg)
  replace_eml <- get_user_response()

  return(replace_eml)
}
