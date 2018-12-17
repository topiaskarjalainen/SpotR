#Simple helper function to make it easier to refer to the httr content function
getContent <- function(result) {
  return(httr::content(result))
}


#' Retruns the contents of a GET
#' @param query An URL
#' @export

GETRequest <- function(query) {
  header <- paste("Bearer ", Sys.getenv("token"), sep = "")
  result <- httr::GET(query,
                      httr::add_headers(Authorization = header))
  return(httr::content(result))
}
