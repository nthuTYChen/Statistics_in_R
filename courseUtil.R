loadCourseCSV = function(year = NULL, topic = NULL, file = NULL) {
  path = "https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/"
  return(read.csv(paste(path, year, "/", topic, "/", file, sep = ""), 
                    fileEncoding = "UTF-8-BOM", na.strings = c("NA", "na")))
}

compNormSamp = function(data = NULL) {
  data.mean = mean(data)
  data.sd = sd(data)
  data.den = density(data)
  plot(data.den, main = "Sample vs. Theoretical Distribution",
       xlim = c(data.mean - data.sd * 3, data.mean + data.sd * 3))
  data.seq = seq(from = data.mean - data.sd * 3, to = data.mean + data.sd * 3,
                 by = .1)
  data.norm.den = dnorm(data.seq, mean = data.mean, sd = data.sd)
  lines(x = data.seq, y = data.norm.den, col = "purple", lty = 2, lwd = 2)
}