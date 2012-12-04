# go to 'https://developers.facebook.com/tools/explorer' to get your access token
access_token <- "AAACEdEose0cBAM8QOz5HxQPs2ZBNQrZAiWZAnI7z4r2LEsVbAAK8uFBF89jZCC9sIKIEoRjZCVAAAGJyZBKoFp0bjL4UapoIdkMfohBY8WLAZDZD"

require(RCurl)
require(rjson)

options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE)) 

# Facebook json function copied from original (Romain Francois) post
facebook <-  function( path = "me", access_token, options){
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=", unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  data <- getURL( sprintf( "https://graph.facebook.com/%s%s&access_token=%s", path, options, access_token ) )
  fromJSON( data )
}

### MY FACEBOOK POSTS

myposts <- list()
i <- 0
next.path <- "me/posts"

# download all my posts
while(length(next.path)!=0) {
  i<-i+1
  myposts[[i]] <- facebook(path=next.path , access_token=access_token)
  next.path <- sub("https://graph.facebook.com/", "", myposts[[i]]$paging$'next')
}
myposts[[i]] <- NULL

# parse the list, extract number of likes and the corresponding text (status)
parse.master <- function(x, f)
  sapply(x$data, f)
parse.likes <- function(x) if(!is.null(x$likes$count)) x$likes$count else 0
mylikes <- unlist(sapply(myposts, parse.master, f=parse.likes))
parse.messages <- function(x) if(!is.null(x$message)) x$message else NA
mymessages <- unlist(sapply(myposts, parse.master, f=parse.messages))

# and the most liked status is...
mymessages[which.max(mylikes)]

### TED FACEBOOK PAGE
# http://www.facebook.com/TED
# TED's Facebook ID 29092950651 can be found on http://graph.facebook.com/TED

ted <- list()
i<-0
next.path <- "29092950651/posts"

# download all TED posts
while(length(next.path)!=0) {
  i<-i+1
  ted[[i]] <- facebook( path=next.path , access_token=access_token)
  next.path <- sub("https://graph.facebook.com/","",ted[[i]]$paging$'next')
}
ted[[i]] <- NULL

# parse just video links posted by TED
parse.count.ted <- function(x) 
  if (x$type=="link" & x$from$id=="29092950651") x$likes$count else NA
parse.link.ted <- function(x) 
  if (x$type=="link" & x$from$id=="29092950651") x$link else NA
ted.counts <- unlist(sapply(ted, parse.master, f=parse.count.ted))
ted.links <- unlist(sapply(ted, parse.master, f=parse.link.ted))

# see three most popular talks
ted.links[order(ted.counts,decreasing=TRUE)][1:3]