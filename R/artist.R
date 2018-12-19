#' TODO Functions that allow pulling data on artists
#' https://developer.spotify.com/documentation/web-api/reference-beta/#category-artists

#' Info on artist.
#' returns a list that contains the info on the artist
#' @param artistID spotify ID of the artist.
#' @export
getArtist <- function(artistID) {
  url <- paste("https://api.spotify.com/v1/artists/", artistID, sep = "")
  result <- GETRequest(url)
  return(result)
}

#' Returns the toptracks of artist.
#' @param artistID The Spotify ID of the artist
#' @param country An ISO 3166-1 alpha-2 country code or the string
#' @export
getArtistTopTracks <- function(artistID, country) {
  url <- paste("https://api.spotify.com/v1/artists/",
               artistID,
               "/top-tracks?country=",
               country,
               sep = "")

  result <- GETRequest(url)
  df <- buildToptrackDF(result)
  return(df)
}

#' Builds a data frame.
#' @param trackList a list that contains tracks
buildToptrackDF <- function(trackList) {
  df <- data.frame(track = character(10),
                   popularity = integer(10),
                   id = character(10),
                   url = character(10),
                   stringsAsFactors = FALSE)

  for (i in 1:10) {
    roww <- c(trackList$tracks[[i]]$name,
              trackList$tracks[[i]]$popularity,
              trackList$tracks[[i]]$id,
              trackList$tracks[[i]]$href)
    df[i,] <- roww
  }
  return(df)
}
