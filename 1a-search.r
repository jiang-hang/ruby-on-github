library(lubridate)
library(plyr)
source("requests.r")

search_repo <- function(query, page = NULL) {
  Sys.sleep(12)
  path <- paste0(base, "/search/repositories")
  qs <- list(q = paste("stars:>100","language:ruby", query), per_page = 100, page = page)
  
  req <- GET(path, config, query = qs)
  c <- content(req)
  if (req$status_code != 200) {
    browser()
    stop(c$message, call. = FALSE)
  }
  
  n <- c$total_count
  items <- c$items
  
  # Only one page of results
  if (!is.null(page) || n <= 100) {
    message(n, " repos")
    return(items)
  }
  
  # Multiple pages of results
  if (n > 1000) {
    warning(n , " repos. Only first 1000 returned", call. = FALSE)
  } else {
    message(n, " repos")
  }
  
  # Combine individual pages
  max_page <- ceiling(n / 100)
  if( max_page > 10 ){ 
	max_page = 10
  }
  results <- lapply(2:max_page, search_repo, query = query)
  c(items, unlist(results, recursive = FALSE))
}

if (!file.exists("cache/prehistory.rds")) {
  prehistory <- search_repo("created:<2008-01-01")
  saveRDS(prehistory, "cache/prehistory.rds") 
} else {
  prehistory <- readRDS("cache/prehistory.rds")
}

get_month <- function(year, month, day) {
  cache_path <- paste0("cache/", year, "-", month, ".rds")
  if (file.exists(cache_path)) return(readRDS(cache_path))
  
  message("Downloading repos created in ", year, "-", month)
  if( month != 12 ) {
      start_s=sprintf("%4d-%02d-%02d",year,month,1)
      end_s=sprintf("%4d-%02d-%02d",year,month+1,1)
  }else{
      start_s=sprintf("%4d-%02d-%02d",year,month,1)
      end_s=sprintf("%4d-%02d-%02d",year,month,31)
  }
  query <- paste0("created:", start_s,"..", end_s)
  results <- search_repo(query)
  
  saveRDS(results, cache_path)
  results
}

all_months <- function() {
  cur_year <- year(today())
  
  past <- expand.grid(year = 2008:(cur_year - 1), month = 1:12, day=1)
  pres <- data.frame(year = cur_year, month = 1:(month(today()) - 1), day=1)
  all <- rbind(past, pres)
  
  unlist(mlply(all, get_month), recursive = FALSE)
}

repos <- c(prehistory, all_months())
forks <- sapply(repos, "[[", "fork")

names <- unique(unname(sapply(repos, "[[", "full_name")))
saveRDS(names, "repos.rds")

# Distribution of repo counts

owners <- count(sapply(repos, function(x) x$owner$login))
subset(arrange(owners, desc(freq)), freq > 2)
