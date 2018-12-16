#'Retuns a data frame of the users playlists.
#'@param userID The wanted user id as a string
#'@value Returns a dataframe that contains the names of the palylists, the total
#'amount of songs on the list and the spotify URI of the playlist
#'@export

getUserPlayslists <- function(userID) {
  query <- paste("https://api.spotify.com/v1/users/",
                 userID,
                 "/playlists",
                 sep = "")
  message("Downloading...")
  df <- buildPlaylistDF(query)

  return(df)
}

#' Bilds the dataframe.
#'  Takes the initial query as an argument
#' @param query

buildPlaylistDF <- function(query) {
  thisQ <- GETRequest(query)
  offs <- thisQ[["offset"]]
  size <- thisQ[["total"]]
  df <- data.frame(name = character(size),
                   n.songs = numeric(size),
                   stringsAsFactors = FALSE)
  while (TRUE) {
    offs <- thisQ[["offset"]]
    NEXT <- thisQ[["next"]]

    for (i in 1:length(thisQ[["items"]])) {
      playlist <- thisQ[["items"]][[i]]
      line <- c(playlist$name, playlist$tracks$total)
      df[offs+i,] <- line
    }

    if (is.null(NEXT)) {
      break
    }

    thisQ <- GETRequest(NEXT)
  }

  return(df)
}

