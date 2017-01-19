#' @title 'ionicons' png file
#' @description get full path of an icon from 'ionicons' pack.
#' @details
#' All icons are squares (width 512 pixels and height 512 pixels).
#' @param name icon name (without extensions)
#' @examples
#' ionicons(name = "alert")
#' @export
ionicons <- function( name ){
  path <- system.file(package = "ionicons", "png", paste0(name, ".png"))
  if( nchar(path) < 1)
    stop("could not find icon ", name, call. = FALSE)
  path
}

#' @title list 'ionicons' icons
#' @description get 'ionicons' pack icons names.
#' @examples
#' ls_ionicons()
#' @export
ls_ionicons <- function(  ){
  path <- system.file(package = "ionicons", "png")
  all_icons <- list.files(path, pattern = "\\.png$")

  if( length(all_icons) > 0 )
    gsub(pattern = "\\.png$", replacement = "", x = basename(all_icons))
  else character(0)
}


