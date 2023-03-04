#' @title Converts LAZ to LAS + Index
#'
#' @param lazfile - LAZ file name
#' @param outdir = "data/las" - output directory,
#' @param filter = "-keep_class 2 9" - optional filter
#'
#' @export
#'
#' @importFrom stringi stri_replace_all_regex stri_replace_all_fixed
#' @importFrom lidR readLAS writeLAS
#' @importFrom sf st_crs st_bbox st_geometry
#'
#' @usage convertLAZ(lazfile, outdir = "data/las", filter = "-keep_class 2 9")
#'
convertLAZ <- function(lazfile, outdir = "data/las", filter = "-keep_class 2 9") {
  if(!dir.exists({{outdir}})) { dir.create({{outdir}}, recursive = TRUE)}
  print(lazfile)
  .file <- stringi::stri_replace_all_regex({{lazfile}}, "^.*/", "")
  .outfile <- paste0({{outdir}}, "/", stringi::stri_replace_all_fixed(.file, "laz", "las"))
  if(!file.exists(.outfile)) {
    las <- lidR::readLAS(files = {{lazfile}}, filter = {{filter}})
    lidR::writeLAS(las, file = .outfile, index = TRUE)
  }
  else {
    message("Output file ", .outfile, " already exists, skipping conversion.")
  }
}
