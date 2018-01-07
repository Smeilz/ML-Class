installed.packages('ggplot2')
library(ggplot2)
getwd()
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
names(pf)
ggplot(aes(x = dob_day), data = pf) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 1:31)

str(pf)

ggplot(data = pf, aes(x = dob_day)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 1:31) +
  facet_wrap(~dob_month)