#------------------------------------------------------------------
# experiment with obtaining shootout goals from nhl.com
#------------------------------------------------------------------

# compress mightily on save
options(save.defaults=list(compress="bzip2", compression_level=9))

library(RCurl)
library(stringr)
library(XML)
library(plyr)
library(lubridate)

url.checker <- function(url) {
  fn <- try(suppressWarnings(readLines(con <- url(url))), silent=TRUE)
  close(con)
  !inherits(fn, "try-error")
}

hrefFun <- function(x){
  xpathSApply(x,'./a',xmlAttrs)  
}

base.url <- "http://www.nhl.com/ice/shootoutstats.htm?season=20122013&team=&viewName=shootoutsSkaters"

doc = htmlParse(base.url)
tableNodes = getNodeSet(doc, "//table")

table.list <- readHTMLTable(base.url,
                            header=FALSE,
                            stringsAsFactors=FALSE)

names(table.list)
lapply(table.list, function(x) head(x))

tmp <- table.list[[5]]
