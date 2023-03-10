loadCourseCSV = function(week = NULL, file = NULL) {
  path = "https://lngproc.hss.nthu.edu.tw/statisticsR/"
  return(read.csv(paste(path, week, "/", file, sep = ""), 
                    fileEncoding = "UTF-8-BOM", na.strings = c("NA", "na")))
}