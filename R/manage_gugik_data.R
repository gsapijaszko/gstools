globalVariables(c("admin_level", "name"))
#'
#'
#' link_gugik_data_from_archive() creates a symbolic link to file in archive
#'
#' @importFrom httr parse_url
#' @param url url from GUGiK
#' @param archive_dir folder path with archive data
#' @param output_dir local folder to store data
#'
#' @export
#'
#' @usage link_gugik_data_from_archive(url, archive_dir, output_dir)

link_gugik_data_from_archive <- function(url, archive_dir, output_dir) {
  if(!dir.exists({{output_dir}})) {dir.create(path = {{output_dir}}, recursive = TRUE)}
  path <- httr::parse_url({{url}})$path
  source_file <- paste0({{archive_dir}}, "/", path)
  dest_file <- paste0({{output_dir}}, "/", basename(path))
  message("Linking ", basename(path), " to ", dest_file)
  # file.copy(from = source_file, to = dest_file)
  file.symlink(from = source_file, to = dest_file)
}


#' municipiality_file_name() creates and returns file name for municipality
#' @importFrom stringi stri_replace_all_regex stri_replace_all_fixed
#'
#' @param municipiality Municipality name
#' @param dir Output directory
#' @param extension file extension, default .rds
#'
#' @return file_name Name of file to read / write data
#'
#' @usage municipiality_file_name(municipiality, dir, extension)
#'
#' @examples municipiality_file_name("Oborniki Śląskie", "data")
#' @examples municipiality_file_name("Oborniki Śląskie", "data", extension = ".csv")
#'
#' @export
#'
municipiality_file_name <- function(municipiality, dir, extension = ".rds") {
  file_name <- paste0({{dir}}, "/", {{municipiality}}, {{extension}}) |>
    iconv(to="ASCII//TRANSLIT") |>
    tolower() |>
    stringi::stri_replace_all_regex(pattern = "[[:punct:]]", "") |>
    stringi::stri_replace_all_fixed(pattern = " ", replacement = "_")
  return(file_name)
}


#' @title download_gugik_dataset_for_municipality() downloads boundaries of villages within municipality,
#' downloads the dataset from GUGiK and saves it to file
#'
#' @importFrom osmdata getbb opq add_osm_features osmdata_sf unique_osmdata
#' @importFrom sf st_join st_within
#'
#' @param municipiality municipality name
#' @param output_dir default "data"
#'
#' @usage download_gugik_dataset_for_municipality(municipiality, output_dir = "data")
#'
#' @export
#'
download_gugik_dataset_for_municipality <- function(municipiality, output_dir = "data") {
  message("Downloading boundaries for ", municipiality)

  dane <- data.frame(wies = character(),
                     godlo = character(),
                     akt_rok = integer(),
                     format = character(),
                     charPrzest = character(),
                     bladSrWys = numeric(),
                     ukladXY = character(),
                     #modulArch = character(),
                     ukladH = character(),
                     #nrZglosz = character(),
                     czy_ark_wypelniony = character(),
                     #daneAktualne = integer(),
                     #lok_nmt = character(),
                     url_do_pobrania = character(),
                     nazwa_pliku = character(),
                     #idNmt = integer(),
                     idSerie = integer(),
                     sha1 = character(),
                     asortyment = character()
  )

  bb <- osmdata::getbb(paste0("gmina ", {{municipiality}}))

  gr_adm <- osmdata::opq(bb, timeout = 60) |>
    osmdata::add_osm_features(
      features = c(
        "\"boundary\"= \"administrative\""
      )
    ) |> osmdata::osmdata_sf() |>
    osmdata::unique_osmdata()

  gr_gminy <- gr_adm$osm_multipolygons |>
    subset(admin_level == "7" & name == paste("gmina", municipiality)) |>
    subset(select = "name")

  gr_wsi <- gr_adm$osm_multipolygons |>
    subset(admin_level == "8") |>
    sf::st_join(gr_gminy, join = sf::st_within, left = FALSE) |>
    dplyr::arrange("name.x") |>
    subset(select = c("name.x")) |>
    stats::setNames(c("name", "geometry"))

  if(nrow(gr_wsi) == 0) {
    message("There is no village boundaries within geometry of ", municipiality, " municipiality.\n")
    message("Please consider using 'county' parameter")
  } else {
    for (j in 1:nrow(gr_wsi)) {
      print(gr_wsi[j,])
      data <- .DEM_request(gr_wsi[j,])
      dane <- cbind(data, wies = rep(gr_wsi[j, ]$name, nrow(data))) |>
        rbind(dane)
      print(nrow(dane))
    }
    if(!dir.exists({{output_dir}})) {dir.create({{output_dir}}, recursive = TRUE)}
    saveRDS(dane, file = municipiality_file_name(municipiality, output_dir, extension = ".rds"))
  }
}

