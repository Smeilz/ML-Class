
cat(".Rprofile: Setting US repositoryn")
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
rm(r)

install.packages("CHAID", repos="http://R-Forge.R-project.org")
install.packages("pROC")
install.packages("rattle")
install.packages("smbinning")

data <- read.csv2("C:/Trees/Churn_binned.csv")
str(data)
data$longdist2 <- ordered(data$longdist, levels = c("<2", "2--8", " 9--14", "15-20", "21+"))
data$longdist <- NULL
str(data$longdist2)


library(ggplot2)
qplot(data = data, x = data$longdist2)
str(data$longdist2)