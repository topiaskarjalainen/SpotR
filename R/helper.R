#' Retruns the contents of a GET
#' @param query An URL
#' @export

GETRequest <- function(query) {
  header <- paste("Bearer ", Sys.getenv("token"), sep = "")
  result <- httr::GET(query,
                      httr::add_headers(Authorization = header))
  return(httr::content(result))
}
