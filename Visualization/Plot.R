setwd("/Users/fangshu/Desktop/course/Spring\ 2021/IE\ 592/Output_all/v26/Evaluation")
filenames <- list.files()
# filenames eg: evaluation_2011-04-01_2012-03-30.csv
# get 2011-04-01_2012-03-30 from filenames
names <- substr(filenames,12,32) # get the date 
dat <- data.frame(names)

for(i in 1:length(filenames)){
  dat_single <- read.csv(filenames[i],header = T)
  dat[i,2:3] <- apply(dat_single[,2:3], 2, sum)
}

colnames(dat) <- c("Date_Range", "absolute criteria","relative criteria")
# reshape the data
library(reshape2)
library(ggplot2)

dat_plot <- melt(dat,id=c("Date_Range"))

dat_plot$Date_Range <- as.factor(dat_plot$Date_Range)

p <- ggplot(dat_plot,aes(Date_Range,value,color=variable))+geom_line(aes(group=variable)) + coord_flip()
p <- p + scale_x_discrete(limits = rev(levels(dat_plot$Date_Range)))
p <- p + theme(legend.title = element_blank(),legend.position = "top") + labs(y = "How many criteria are satisfied out of 6", x = "Date Range of the test data") 
#ggsave('evaluation.png',p)
p
