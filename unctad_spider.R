install.packages("RCurl")
install.packages("XML")
install.packages("httr")
library(RCurl)
library(XML)
library(httr)

# change to path to the directory where you want to save the files
path = "/Users/yingbozhang/Desktop/rproj/files/"

rooturl = 'http://unctad.org'
webpage = getURL("http://unctad.org/en/Pages/DIAE/FDI%20Statistics/FDI-Statistics-Bilateral.aspx")
webpage = htmlTreeParse(webpage, useInternalNodes=TRUE)
title_pdf = getNodeSet(webpage, path="//select[@id='FDIcountries']/option[@value!='']/text()")
link_pdf = getNodeSet(webpage, path="//select[@id='FDIcountries']/option[@value!='']/attribute::value")
title_excel = getNodeSet(webpage, path="//select[@id='FDIcountriesxls']/option[@value!='']/text()")
link_excel = getNodeSet(webpage, path="//select[@id='FDIcountriesxls']/option[@value!='']/attribute::value")

for(i in 1:length(title_pdf)){
  file_name = paste(path, as(title_pdf[[i]], "character"), ".pdf", sep="")
  GET(url=paste(rooturl,link_pdf[[i]], sep=''), write_disk(file_name, overwrite = TRUE))
  file_name = paste(path, as(title_excel[[i]], "character"), ".xls", sep="")
  GET(url=paste(rooturl,link_excel[[i]], sep=''), write_disk(file_name, overwrite = TRUE))
}

