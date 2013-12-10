# collection of utility functions used in web scraping

# checks if URL is valid.  Returns TRUE if valid
url.checker <- function(url) {
  fn <- try(suppressWarnings(readLines(con <- url(url))), silent=TRUE)
  close(con)
  !inherits(fn, "try-error")
}

# functions to use within readHTMLTable calls in order to parse hyperlinks
# out of table cells. 

# use this if table cell is not bold (strong) text
hrefFun_link <- function(x) {
  xpathSApply(x,'./a',xmlAttrs)  
}

# use this if table cell is bold (strong) text
hrefFun_slink <- function(x) {
  xpathSApply(x,'./strong /a',xmlAttrs)  
}
