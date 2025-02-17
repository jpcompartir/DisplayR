#' View which brands have styling options available for them
#'
#' @description Check which brands a _brand.yml and an optional 'logo.png' have been uploaded to DisplayR for.
#'
#' @returns A list of folder names relating to brands/
#' @export
#'
#' @examples
#' brands <- dr_list_brands()
dr_list_brands <- function(){
 brands_path <- system.file("brands", package = "DisplayR")

 brands <- list.dirs(brands_path, full.names = FALSE, recursive = FALSE)


 # Chepck for _brand.yml
 has_brand_yml <- sapply(brands, function(brand) {
   file.exists(file.path(brands_path, brand, "_brand.yml"))
  })

  return(brands[has_brand_yml])
}

#' Add brand styling to your project
#'
#' @description Add a _brand.yml and a logo.png to your project from DisplayR's inst/brands folder. If you need a new brand to be added, contact the Data Science Team
#' @param brand Name of the brand you wish to load, as it appears in `dr_list_brands()`
#' @param directory  The directory you want the brand information to be added to, as a default it uses the current working directory
#'
#' @returns Invisible
#' @export
#'
#' @examples
#'
#' brands <- dr_list_brands()
#' dr_add_brand("shareds")
#'
dr_add_brand <- function(brand, directory = getwd()) {

  brands_path <- system.file("brands", package = "DisplayR")
  brands <- list.dirs(brands_path, full.names = FALSE, recursive = FALSE)

  if(!brand %in% brands) {
    stop(sprintf("Brand: '%s' not found in listed brands", brand))
  }

  brand_path <- file.path(brands_path, brand)

  if(!file.exists(file.path(brand_path, "_brand.yml"))){
    stop("No _brand.yml found in brands folder")
  } else {
    brand_yml <- file.path(brand_path, "_brand.yml")
  }

  file.copy(
    from = brand_yml,
    to = directory,
    overwrite = TRUE
  )

  has_logo <- file.exists(file.path(brand_path, "logo.png"))
  if(has_logo){
    file.copy(
      from = file.path(brand_path, "logo.png"),
      to = directory,
      overwrite = TRUE
    )
  } else {
    message(sprintf("No 'logo.png' found in '%s' ", brand_path))
  }

  invisible(TRUE)
}



