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
  if(!file.exists(source_file) || file.size(source_file) == 0L) {
    message("Seems the file is not downloaded yet. Trying to download:")
    get_gugik_data({{url}}, sha = "", {{archive_dir}})
    } else {
      message("Linking ", basename(path), " to ", dest_file)
      # file.copy(from = source_file, to = dest_file)
      file.symlink(from = source_file, to = dest_file)
  }
}


#' municipiality_file_name() creates and returns file name for municipality
#' @importFrom stringi stri_replace_all_regex stri_replace_all_fixed
#'
#' @param name Municipality name
#' @param dir Output directory
#' @param prefix optional prefix to file name, like prefix_filename.ext
#' @param suffix optional suffix to file name, like filename_suffix.ext
#' @param extension file extension, default .rds
#'
#' @return file_name Name of file to read / write data
#'
#' @usage format_file_name(name, dir, prefix, suffix, extension)
#'
#' @examples format_file_name("Oborniki Śląskie", "data")
#' @examples format_file_name("Oborniki Śląskie", "data", extension = ".csv")
#'
#' @export
#'
format_file_name <- function(name, dir, prefix = "", suffix = "", extension = ".rds") {
  if(nchar(prefix) > 0) {
    file_name <- paste0({{prefix}}, "_")
  } else {
    file_name <- ""
  }

  file_name <- paste0(file_name, {{name}})

  file_name <- stringi::stri_replace_all_regex(file_name, pattern = "[[:punct:]]", "")

  if(nchar(suffix) > 0) {
    file_name <- paste0(file_name, "_", {{suffix}})
  }

  if(!grepl("^\\.", {{extension}})) {
    .ext <- paste0(".", {{extension}})
  } else {
    .ext <- {{extension}}
  }

  file_name <- paste0({{dir}}, "/", file_name, .ext) |>
    iconv(to="ASCII//TRANSLIT") |>
    tolower() |>
    stringi::stri_replace_all_fixed(pattern = " ", replacement = "_") |>
    stringi::stri_replace_all_regex(pattern = "\\_+", replacement = "_")

  return(file_name)
}


#' @title download_gugik_dataset_for_municipality() downloads boundaries of villages within municipality,
#' downloads the dataset from GUGiK and saves it to file
#'
#' @importFrom osmdata getbb opq add_osm_features osmdata_sf unique_osmdata
#' @importFrom rgugik DEM_request ortho_request
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
  .output_file_name <- format_file_name(municipiality, output_dir, suffix = "dem", extension = ".rds")
  if(file.exists(.output_file_name) & difftime(Sys.time(), file.mtime(.output_file_name), units = "days") < 30) {
    message("File for ", {{municipiality}}, " exists")
  } else {
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

    dane_ortho <- data.frame(godlo = character(),
                          akt_rok = integer(),
                          piksel = numeric(),
                          kolor = character(),
                          zrDanych = character(),
                          ukladXY = character(),
                          #modulArch = character(),
                          #nrZglosz = character(),
                          czy_ark_wypelniony = character(),
                          #daneAktualne = integer(),
                          #daneAktDo10cm = integer(),
                          #lok_orto = character(),
                          url_do_pobrania = character(),
                          idSerie = integer(),
                          sha1 = character(),
                          akt_data = numeric(),
                          #idorto = integer(),
                          nazwa_pliku = character(),
                          #ESRI_OID = integer()
                          wies = character()
    )

    bb <- try(osmdata::getbb(paste0("gmina ", {{municipiality}})))
    if(inherits(bb, "try-error")) {
      message("Got an error during downloading")
      bb <- try(osmdata::getbb({{municipiality}}))
      .name <- mun
    } else {
      .name <- paste("gmina", mun)
    }

    gr_adm <- osmdata::opq(bb, timeout = 60) |>
      osmdata::add_osm_features(
        features = c(
          "\"boundary\"= \"administrative\""
        )
      ) |> osmdata::osmdata_sf() |>
      osmdata::unique_osmdata()

    if(grepl(",", {{municipiality}})) {
      mun <- stringi::stri_extract_first_regex({{municipiality}}, "^[[:alpha:]]+")
    } else {
      mun <- {{municipiality}}
    }

    gr_gminy <- gr_adm$osm_multipolygons |>
      subset(admin_level == "7" & name == .name) |>
      subset(select = "name")

    gr_wsi <- gr_adm$osm_multipolygons |>
      subset(admin_level == "8") |>
      sf::st_join(gr_gminy, join = sf::st_within, left = FALSE) |>
      dplyr::arrange("name.x") |>
      subset(select = c("name.x")) |>
      stats::setNames(c("name", "geometry"))

    if(nrow(gr_wsi) == 0) {
      message("There is no village boundaries within geometry of ", municipiality, " municipiality.\n")
    } else {
      for (j in 1:nrow(gr_wsi)) {
        print(gr_wsi[j,])
        data <- .DEM_request(gr_wsi[j,])
        # data <- rgugik::DEM_request(gr_wsi[j,])
        dane <- cbind(data, wies = rep(gr_wsi[j, ]$name, nrow(data))) |>
          rbind(dane)
        message("# of DEM data files: ", nrow(dane))

        # data_ortho <- rgugik::ortho_request(gr_wsi[j,])
        data_ortho <- .ortho_request(gr_wsi[j,])
        dane_ortho <- cbind(data_ortho, wies = rep(gr_wsi[j, ]$name, nrow(data_ortho))) |>
          rbind(dane_ortho)
        message("# Ortho images: ", nrow(dane_ortho))
      }
      if(!dir.exists({{output_dir}})) {dir.create({{output_dir}}, recursive = TRUE)}
      saveRDS(dane, file = format_file_name(municipiality, suffix = "dem", output_dir))
      saveRDS(dane_ortho, file = format_file_name(municipiality, suffix = "ortho", output_dir))
    }
  }
}


#' @title Downloads gugiik data to specific folder using wget
#'
#' @importFrom httr parse_url
#' @importFrom utils download.file
#' @importFrom openssl sha1
#' @param url url from GUGiK
#' @param sha optionally sha1 of the file
#' @param output_dir local folder to store data
#'
#' @export
#'
#' @usage get_gugik_data(url, sha, output_dir)
#'
#'
get_gugik_data = function(url, sha = "", output_dir) {
  path = httr::parse_url({{url}})$path
  output_dir <- paste0({{output_dir}}, "/", dirname(path))
  if(!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE)}
  destination_file <- paste0(output_dir, "/", basename(path))
  # if(!is.na(sha) && file.exists(destination_file) && as.character(openssl::sha1(file(destination_file))) == {{sha}}) {
  #   message("File already exists, not downloading")
  # } else {
    if(file.exists(destination_file) && file.size(destination_file) == 0L) {
      message("Removing file ", destination_file)
      file.remove(destination_file)
    }

    c <- try(utils::download.file(url, destfile = destination_file, mode = "wb",
                                  method = "wget", extra = "--no-check-certificate -c --progress=bar:force -T 5 -t 3"))

    if(inherits(c, "try-error")) {
      message("Got an error during downloading")
    } else {}
    Sys.sleep(1)
  # }
}
