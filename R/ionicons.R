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
#' as_png(name = "alert")
#' @export
#' @importFrom rsvg rsvg_png
as_png <- function( name, png = NULL, width = NULL, height = NULL, fill = "black" ){

  svg_file <- fortify_svg(name = name, fill = fill )

  if( is.null(png))
    png <- tempfile(fileext = ".png")

  rsvg_png(svg = svg_file, file = png, width = width, height = height)
  png
}


#' @title 'ionicons' png file
#' @description raster of an icon from 'ionicons' pack.
#' Width, height and fill color can be specified.
#' @details
#' All icons are 512 pixels * 512 pixels by default.
#' @param name icon name (without extensions)
#' @param width output width in pixels or NULL for default.
#' @param height output height in pixels or NULL for default.
#' @param fill fill color
#' @examples
#' as_array(name = "alert")
#' @export
#' @importFrom rsvg rsvg_raw
as_array<- function( name, width = NULL, height = NULL, fill = "black" ){

  svg_file <- fortify_svg(name = name, fill = fill )
  rsvg_raw(svg = svg_file, width = width, height = height)
}


#' @importFrom xml2 read_xml xml_set_attr xml_attr<- xml_find_first xml_replace xml_find_all write_xml
fortify_svg <- function( name, fill ){
  path <- system.file(package = "ionicons", "src", paste0(name, ".svg"))
  svg <- tempfile(fileext = ".svg")

  if( nchar(path) < 1)
    stop("could not find icon ", name, call. = FALSE)

  doc <- read_xml(path)

  # delete all fill attributes
  nodes <- xml2::xml_find_all(doc, "//*[@fill]")
  xml_attr( nodes, "fill") <- fill

  # delete style node
  node <- xml_find_first(doc, "//*[contains(local-name(), 'style')]")
  xml_replace(.x = node, .value = "style")

  # add fill attributes to node 'g'
  firstnode <- xml_find_first(doc, "//*[contains(local-name(), 'g')]")
  xml_set_attr( firstnode, "fill", fill )
  write_xml(doc, file = svg)
  svg
}

#' @title list 'ionicons' icons
#' @description get 'ionicons' pack icons names.
#' @param pattern regexp pattern to use to find icons names
#' @examples
#' ls_ionicons()
#' @export
ls_ionicons <- function( pattern = NULL ){
  path <- system.file(package = "ionicons", "src")
  all_icons <- list.files(path, pattern = "\\.svg$")

  if( length(all_icons) > 0 )
    all_icons <- gsub(pattern = "\\.svg$", replacement = "", x = basename(all_icons))

  if( !is.null(pattern) )
    all_icons <- all_icons[grepl(pattern = pattern, x = all_icons)]

  all_icons
}


