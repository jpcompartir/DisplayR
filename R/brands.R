#' View which brands have styling options available for them
#'
#' @description Check which brands a _brand.yml and an optional 'logo.png' have been uploaded to DisplayR for.
#'
#' @param quiet whether to print the name of detected brands or not.
#' @returns A list of folder names relating to brands/
#' @export
#'
#' @examples
#' brands <- dr_list_brands()
dr_list_brands <- function(quiet = FALSE){
  brands_path <- system.file("brands", package = "DisplayR")

  brands <- list.dirs(brands_path, full.names = FALSE, recursive = FALSE) |>
    purrr::keep(~ file.exists(file.path(brands_path, .x, "_brand.yml")))

  if (quiet) return(invisible(brands))

  if (length(brands) == 0) {
    cli::cli_alert_info("No brands available")
  } else {
    cli::cli_alert_success("{length(brands)} brand{?s} available")
    cli::cli_ul(brands)
  }

  return(invisible(brands))
}

#' Add brand styling to your project
#'
#' @description Add brand files to your project from DisplayR's inst/brands folder. If you need a new brand to be added, contact the Data Science Team
#' @param brand Name of the brand you wish to load, as it appears in `dr_list_brands()`
#' @param directory  The directory you want the brand information to be added to, as a default it uses the current working directory
#' @param overwrite Whether to overwrite any existing files
#'
#' @returns Invisible
#' @export
#'
#' @examples
#'
#' brands <- dr_list_brands()
#' dr_add_brand("shareds")
#'
dr_add_brand <- function(brand, directory = getwd(), overwrite = NULL) {
  # just mkae sure the brand does in fact exist
  if (!brand %in% dr_list_brands(quiet = TRUE)) {
    cli::cli_abort("Brand '{brand}' not found. Use {.fn dr_list_brands} to see available brands.")
  }

  brand_path <- file.path(system.file("brands", package = "DisplayR"), brand)
  brand_files <- list.files(brand_path, full.names = TRUE)
  destination_files <- file.path(directory, basename(brand_files))
  existing_files <- basename(brand_files)[file.exists(destination_files)]

  # if files already exist, ask the user whether to overwrite or not
  if(interactive()){
    if (is.null(overwrite) && length(existing_files) > 0) {
      cli::cli_ul(existing_files)
      choice <- utils::menu(c("Yes", "No"), title = "Overwrite existing files?")
      overwrite <- choice == 1
    }
  }

  # default to TRUE if still NULL (no existing files or not specified)
  overwrite <- overwrite %||% TRUE

  # if user didn't want to overwrite let them know what happened
  if (!overwrite && length(existing_files) > 0) {
    cli::cli_alert_danger("Files already exist and were not overwritten:")
    cli::cli_ul(existing_files)
    cli::cli_alert_danger("Repeat: the above files were {.strong not} overwritten.")
    return(invisible(character(0)))
  }

  copied <- file.copy(brand_files, directory, overwrite = overwrite)
  copied_files <- basename(brand_files)[copied]

  # let the user know what happened when we do overwrite
  if (length(copied_files) > 0) {
    cli::cli_alert_success("Added {.strong {brand}} brand files:")
    cli::cli_ul(copied_files)
  }

  invisible(copied_files)
}
