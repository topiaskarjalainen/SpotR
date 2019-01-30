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
#' @return An audio object
#' @export
getAudioAnalysis <- function(trackID) {
  q <- paste("https://api.spotify.com/v1/audio-analysis/", trackID, sep = "")
  res <- GETRequest(q)
  a <- Audio(track = res[[2]],
             bars = res[[3]],
             beats = res[[4]],
             tatums = res[[5]],
             sections = res[[6]],
             segments = res[[7]])
  return(a)
}


#' TODO: Change this so that there is no 100 song limit.

#' Get audio features for track.
#' @param IDs a vector of IDs. Maximum lengths is 100 IDs.
#' @export
getAudioFeatures <- function(IDs) {

  if (length(IDs) > 100) {
    stop("Too many entries")
  }

  q <- paste("https://api.spotify.com/v1/audio-features/?ids=",
             paste(IDs, collapse = ","),
             sep = "")

  res <- GETRequest(q)

  tracks <- list()

  for (i in 1:length(res[["audio_features"]])) {
    fi <- res$audio_features[[i]] %>% unlist()

    f <- audioFeatures(dancebility = as.numeric(fi[1]),
                       energy = as.numeric(fi[2]),
                       key = as.integer(fi[3]),
                       loudness = as.numeric(fi[4]),
                       mode = as.integer(fi[5]),
                       speechiness = as.numeric(fi[6]),
                       acousticness = as.numeric(fi[7]),
                       instrumentalness = as.integer(fi[8]),
                       liveness = as.numeric(fi[9]),
                       valence = as.numeric(fi[10]),
                       tempo = as.numeric(fi[11]),
                       type = fi[12],
                       id = fi[13],
                       uri = fi[14],
                       track_href = fi[15],
                       analysis_url = fi[16],
                       duration_ms = as.integer(fi[17]),
                       time_signature = as.integer(fi[18]))
    tracks[i] <- f
  }

  return(tracks)
}














