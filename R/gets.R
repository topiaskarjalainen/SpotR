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

#' Builds the dataframe.
#' Takes the initial query as an argument
#' @param query

buildPlaylistDF <- function(query) {
  thisQ <- GETRequest(query)
  offs <- thisQ[["offset"]]
  size <- thisQ[["total"]]
  df <- data.frame(name = character(size),
                   n.songs = numeric(size),
                   id = character(size),
                   tracks = character(size),
                   stringsAsFactors = FALSE)
  while (TRUE) {
    offs <- thisQ[["offset"]]
    NEXT <- thisQ[["next"]]

    for (i in 1:length(thisQ[["items"]])) {
      playlist <- thisQ[["items"]][[i]]
      line <- c(playlist$name,
                playlist$tracks$total,
                playlist$id,
                playlist$tracks$href)
      df[offs+i,] <- line
    }

    if (is.null(NEXT)) {
      break
    }

    thisQ <- GETRequest(NEXT)
  }

  return(df)
}


#' Creates a list of data.frames that contain playlist tarcks.
#' @param playlists The data.frame returned by getUserPlaylists function
#' @value A list of data.frames that contain the tracks in the playlists
#' @export
getPlaylistTracks <- function(playlists) {
  theList <- vector("list", length(playlists[, 1]))
  message("Downloading...")
  for (i in 1:length(theList)) {
    traURL <- playlists$tracks[i]
    theList[[i]] <- buildTrackDF(traURL)
  }

  names(theList) <- playlists$name

  return(theList)
}


#' Hidden function. Builds the data frames that contain the tracks.
#' @param playlistURL An URL
#' @value A dataframe that contains the tracks for the playlist specified
#' by playlistURL

buildTrackDF <- function(playlistURL) {
  thisQ <- GETRequest(playlistURL)
  size <- thisQ[["total"]]

  df <- data.frame(name = character(size),
                   album = character(size),
                   artist = character(size),
                   artist_id = character(size),
                   stringsAsFactors = FALSE)

  while (TRUE) {
    offs <- thisQ[["offset"]]
    NEXT <- thisQ[["next"]]

    for (i in 1:length(thisQ[["items"]])) {
      song <- thisQ[["items"]][[i]]$track

      entry <- c(song$name,
                 song[["album"]]$name,
                 song$artists[[1]]$name,
                 song$artists[[1]]$id)

      df[offs+i,] <- entry
    }

    if (is.null(NEXT)) {
      break
    }
    thisQ <- GETRequest(NEXT)
  }

  return(df)
}







