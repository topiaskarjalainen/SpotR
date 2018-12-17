#'Sets authentication parameters.
#'
#'Sets the authentication id and secret token. Does not return any value,
#'and is used only to set these parametses as enviromental variables.
#'
#'@param clientID The client ID token provided by Spotify
#'@param ClientSecret The Client Secret token provided by Spotify
#'@return Does not return a value
#'@author Topias Karjalainen
#'@export

setAuthentication <- function(clientID, ClientSecret){
  Sys.setenv('clientID' = clientID)
  Sys.setenv('secret' = ClientSecret)
}

#'Sends POST request to Spotify API and uses the authenthication ID and the Secret key
#'provided with setAuthentication function
#'@export

getAccesstoken <- function() {
  tokenURL <- "https://accounts.spotify.com/api/token"
  clientID <- Sys.getenv("clientID")
  secret <- Sys.getenv("secret")

  result <- httr::POST(tokenURL,
                 httr::authenticate(clientID, secret),
                 httr::accept_json(),
                 body = list(grant_type='client_credentials'),
                 encode = "form")

  Sys.setenv("token" = httr::content(result)$access_token)
}
