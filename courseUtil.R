loadCourseCSV = function(year = NULL, topic = NULL, file = NULL) {
  path = "https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/"
  return(read.csv(paste(path, year, "/", topic, "/", file, sep = ""), 
                    fileEncoding = "UTF-8-BOM", na.strings = c("NA", "na")))
}