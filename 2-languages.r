library(ggplot2)
library(plyr)
library(reshape2)


genDf<-function(){

repos <- llply(dir("cache-repo", full.names = TRUE), readRDS)



res=ldply(repos, function(x) {
   yy = x$info
   data.frame(name=yy$full_name,owner=yy$owner$login,desc=ifelse(is.null(yy$description),"",yy$description), star=yy$stargazers_count, url=yy$html_url)
   })

saveRDS(res,"./gemdf.rds")
}

topStars<-function()
{
    gems=readRDS("./gemdf.rds")
    gems$name = rwp::link(gems$url,gems$name)
    gems$url=NULL
    gems$star = as.numeric(gems$star)
    gg=gems[order(gems$star,decreasing=T),]
    rownames(gg)=NULL
    rwp::blogtable(head(gg,300),none_escape_column=1)	
}

topOwner<-function()
{
    gems=readRDS("./gemdf.rds")
    gems$name = rwp::link(gems$url,gems$name)
    gems$url=NULL
    gems$star = as.numeric(gems$star)
    rr=plyr::ddply(gems,.(owner),function(df) data.frame(totalStar=sum(df$star),pkgs=paste(df$name,collapse=",")))

    rwp::blogtable(rr,none_escape_column=c("pkgs"))
}



