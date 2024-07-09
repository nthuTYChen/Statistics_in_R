loadCourseCSV = function(year = NULL, week = NULL, file = NULL) {
  path = "https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/"
  return(read.csv(paste(path, year, "/", week, "/", file, sep = ""), 
                    fileEncoding = "UTF-8-BOM", na.strings = c("NA", "na")))
}