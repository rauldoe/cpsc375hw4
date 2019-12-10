
workDir <- "C:/temp/cpsc375hw4/"
filename <- "seattle_rainfall.txt"
sc <- scan(paste(workDir, filename, sep = ""))

aplt <- acf(sc)
aplt

xts <- ts(sc, frequency = 12)

de <- decompose(xts)

deplt <- plot(de)
