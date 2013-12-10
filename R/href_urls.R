#' @title Generate hockey-reference.com URLs
#' 
#' @description These functions create data frames with valid URLs pointing to
#' different types of hockey statistics at \url{www.hockey-reference.com}. 
#' These data frames are already included as part of the nhlstats package
#' collection of data, but these functions enable the user to create updated
#' versions of these data frames if they wish.
#' 
#' @param save.file Save the result data frame to a file.  Default is \code{TRUE}
#' @param rdata.folder The directory where the data frames will be saved. 
#' If the directory does not exist, it will be created.  Default is a 
#' directory called \code{nhlstats-data} within the current working directory.
#' @param quiet Whether to suppress messages in console during execution. 
#' Default is \code{FALSE}
#' 
#' @import scraper XML stringr
#' @importFrom plyr rbind.fill arrange
#' @export
#' 
href_name_urls <- function(rdata.folder = "nhlstats-data", quiet = FALSE) {
  
  #------------------------------------------------------------------------------
  # extract unique hockey ref coaches IDs from hockey ref coaches index
  #------------------------------------------------------------------------------
  
  coach.url <- "http://www.hockey-reference.com/coaches"
  
  if(!quiet) message("generating coach URLs")
  
  page.source <- scrape(url=coach.url, verbose=FALSE, follow=TRUE)
  coach.table <- readHTMLTable(page.source[[1]], stringsAsFactors=FALSE)[[1]]
  names(coach.table) <- tolower(names(coach.table))
  coach.table$coach <- str_replace_all(coach.table$coach, "\\*", "")
  coach.table$firstName <- sapply(str_split(coach.table$coach, pattern=" "), "[", 1)
  coach.table$lastName <- sapply(str_split(coach.table$coach, pattern=" "), "[", 2)
  coach.table <- coach.table[, c("firstName", "lastName")]
  
  # import additional copies of table to obtain embedded HTML links to each player
  # need to import twice due to some player links embedded in bold 
  coach.table.url <- readHTMLTable(page.source[[1]], elFun = hrefFun_link, header=FALSE, stringsAsFactors=FALSE)[[1]]
  coach.table.url2 <- readHTMLTable(page.source[[1]], elFun = hrefFun_slink, header=FALSE, stringsAsFactors=FALSE)[[1]]
  
  # clean up urls and extract unique coach ID to coach table
  url.table <- as.data.frame(cbind(coach.table.url[,1], coach.table.url2[,1]), stringsAsFactors=FALSE)
  url.table[url.table$V1 == "list()", "V1"] <- url.table[url.table$V1 == "list()", "V2"]
  url.table$V2 <- unlist(str_split(url.table$V1, pattern="/"))[4]
  url.table$V2 <- str_replace_all(sapply(str_split(url.table$V1, pattern="/"), "[", 3), "\\.html", "")
  coach.table$type <- "coach"
  coach.table$hrefID <- url.table$V2
  coach.table$url_name <- paste0("http://www.hockey-reference.com", url.table$V1)
  
  
  #------------------------------------------------------------------------------
  # extract unique hockey ref player IDs from hockey ref player index
  #------------------------------------------------------------------------------
  
  name.urls <- paste0("http://www.hockey-reference.com/players/", letters)
  
  page.source <- scrape(url=name.urls, verbose=FALSE, follow=TRUE)
  id.list <- vector("list", length(name.urls))
  
  for(i in 1:length(name.urls)) {
    
    #if(!quiet) message(paste0("generating player URLs with last name starting with ", upcase(letters[i])))
    
    letter.table <- readHTMLTable(page.source[[i]], stringsAsFactors=FALSE)
    
    # check to make sure there is a table with player names starting with this letter
    # if not, make list element missing and go to next letter
    if(!any(names(letter.table) %in% c("players"))) {
      id.list[[i]] <- NA
      next
    }
    
    # import table and remove asterisk from player name
    letter.table <- letter.table[[1]]
    names(letter.table) <- tolower(names(letter.table))
    letter.table$player <- str_replace_all(letter.table$player, "\\*", "")
    letter.table$firstName <- sapply(str_split(letter.table$player, pattern=" "), "[", 1)
    letter.table$lastName <- sapply(str_split(letter.table$player, pattern=" "), "[", 2)
    letter.table <- letter.table[, c("firstName", "lastName")]
    
    # import additional copies of table to obtain embedded HTML links to each player
    # need to import twice due to some player links embedded in bold 
    letter.table.url <- readHTMLTable(page.source[[i]], elFun = hrefFun_link, header=FALSE, stringsAsFactors=FALSE)[[1]]
    letter.table.url2 <- readHTMLTable(page.source[[i]], elFun = hrefFun_slink, header=FALSE, stringsAsFactors=FALSE)[[1]]
    
    # clean up urls and extract unique player ID to letter table
    url.table <- as.data.frame(cbind(letter.table.url[,1], letter.table.url2[,1]), stringsAsFactors=FALSE)
    url.table[url.table$V1 == "list()", "V1"] <- url.table[url.table$V1 == "list()", "V2"]
    url.table$V2 <- unlist(str_split(url.table$V1, pattern="/"))[4]
    url.table$V2 <- str_replace_all(sapply(str_split(url.table$V1, pattern="/"), "[", 4), "\\.html", "")
    letter.table$type <- "player"
    letter.table$hrefID <- url.table$V2
    letter.table$url_name <- paste0("http://www.hockey-reference.com", url.table$V1)
    
    id.list[[i]] <- letter.table
  }
  
  # combine into one data frame using rbind.fill
  player.table <- rbind.fill(id.list[!is.na(id.list)])
  
  # combine both player and coaches into one table
  urls_href_names <- rbind.fill(coach.table, player.table)
  
  # sort by last name
  urls_href_names <- arrange(urls_href_names, lastName)
  
  # save to file if user requested it
  if(save.file) {
    dir.create(rdata.folder, showWarnings = FALSE)
    save(urls_href_names, file = file.path(rdata.folder, "urls_href_names.RData"))
  }
  
  return(urls_href_names)
}