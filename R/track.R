#' A function that returns info on a track based on its SpotiyID.
#'
#' @param trackID Tjhe Spotify ID of the track
#' @return A list that contains the track name as chr, track id as chr,
#' popularity as numeric and possible genres as an vector. It should be noted
#' that the genre comes from artist query and might not entirely mach the genre
#' of the song.
#' @export
getTrack <- function(trackID) {
  track <- GETTrack(trackID)

  rList <- list()


  return(track)
}

#' Returns audio analysisi object.
#'
#' @param trackID Spotify ID
#' @return A list
#' @export
getAudioAnalysis <- function(trackID) {
  q <- paste("https://api.spotify.com/v1/audio-analysis/", trackID, sep = "")
  res <- GETRequest(q)
}
