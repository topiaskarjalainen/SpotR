#' Retruns the contents of a GET
#' @param query An URL
#' @export

GETRequest <- function(query) {
  header <- paste("Bearer ", Sys.getenv("token"), sep = "")
  message("Downloading...")
  result <- httr::GET(query,
                      httr::add_headers(Authorization = header))

  message("Done!")
  return(httr::content(result))
}

#' Internal helper. A wrapper for getting a track.
GETTrack <-  function(trackID) {
  result <- GETRequest(paste("https://api.spotify.com/v1/tracks/",
                             trackID, sep = ""))
  return(result)
}
