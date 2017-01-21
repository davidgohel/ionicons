#' @title 'ionicons' png file
#' @description get full path of an icon from 'ionicons' pack. The icon
#' is been produced as a png image. Width, height and fill color can be
#' specified.
#' @details
#' All icons are 512 pixels * 512 pixels by default.
#' @param name icon name (without extensions)
#' @param png png file to produce
#' @param width output width in pixels or NULL for default.
#' @param height output height in pixels or NULL for default.
#' @param fill fill color
#' @examples
#' ionicons(name = "alert")
#' @export
#' @importFrom rsvg rsvg_png
#' @importFrom xml2 read_xml xml_set_attr xml_child write_xml
ionicons <- function( name, png = NULL, width = NULL, height = NULL, fill = "black" ){
  path <- system.file(package = "ionicons", "src", paste0(name, ".svg"))
  if( nchar(path) < 1)
    stop("could not find icon ", name, call. = FALSE)
  svg <- tempfile(fileext = ".svg")
  doc <- read_xml(path)
  xml_set_attr( xml_child(doc), "fill", fill )
  write_xml(doc, file = svg)
  if( is.null(png))
    png <- tempfile(fileext = ".png")
  rsvg_png(svg = svg, file = png, width = width, height = height)
  png
}


#' @title list 'ionicons' icons
#' @description get 'ionicons' pack icons names.
#' @examples
#' ls_ionicons()
#' @export
ls_ionicons <- function(  ){
  path <- system.file(package = "ionicons", "src")
  all_icons <- list.files(path, pattern = "\\.svg$")

  if( length(all_icons) > 0 )
    gsub(pattern = "\\.svg$", replacement = "", x = basename(all_icons))
  else character(0)
}


