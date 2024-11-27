#' shinyfactors
#'
#' An interactive shiny app which allows the choice of the number of factors for factor analysis
#'
#' @return a table of principal component solutions for the given number of factors
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples \dontrun{shinypointest()}
shinyfactors <- function() {
  runApp(system.file("shinyfactors",
                     package = "ROSAMATH5793proj2"),
         launch.browser = TRUE)
}
