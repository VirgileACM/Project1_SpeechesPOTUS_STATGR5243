library(qdap)
library(tm)

# recode sigmoid function
sigmoid = function(x) {
  1 / (1 + exp(-x))
}

# to compute abreviation of US President
Abrev <- function(x){
  name <- strsplit(x," ")
  return(paste(substr(name[[1]][1],1,1),".",name[[1]][length(name[[1]])],sep="") )
}

# Compute our index of readability based on the average of 3 measures:
# Coleman Liau, based on text. Gives equivalent to US Grade level (https://en.wikipedia.org/wiki/Coleman%E2%80%93Liau_index)
# ARI, based on text. Gives equivalent of US Grade level (https://en.wikipedia.org/wiki/Automated_readability_index)
# Flesch_Kincaid, based on text. Gives ease to read and US Grade level (https://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_tests)

ReadabilityIndexes <- function(x){
  sentences=sent_detect(x, endmarks = c("?", ".", "!", "|",";"))
  ARI <- automated_readability_index(sentences)$Readability$Automated_Readability_Index
  ColemanLiau <- coleman_liau(sentences)$Readability$Coleman_Liau
  FK_grd <- flesch_kincaid(sentences)$Readability$FK_grd.lvl
  return (mean(ARI,ColemanLiau,FK_grd))
}

Agg_Year_ReadIndex <- function(df){
  presidents <- unique(df$President)
  df_out <- data.frame(Y1=c(1), RI1=c(1), Y2=c(1), RI2=c(1))
  i <- 1
  for (p in presidents) {
    df_out[i,] <- c(filter(df, President==p, Term==1)$Year, filter(df, President==p, Term==1)$ReadIndex, filter(df, President==p, Term==2)$Year, filter(df, President==p, Term==2)$ReadIndex)
    i <- i+1
  }
  return(df_out)
}

