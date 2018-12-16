#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

require(httr)
require(jsonlite)
site <- "https://api.spotify.com"

id <- "f1e07008317e4742b4bf99e9c01499ba"
sec <- "0036440a186845199ac0a2c3b4ac541b"

setAuthentication(id, sec)
getAccesstoken()
getUserPlayslists("unski123")
