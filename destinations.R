##Google Trends categories
library(MacrobondAPI)
library(tidyverse)
library(zoo)
library(xts)
library(tsbox)
library(ggplot2)
#library(plotly)
library(htmlwidgets)
library(dygraphs)
library(gridExtra)


library(gtrendsR)

data("categories")
categories[grepl("Tourist", categories$name), ]
# 208: Tourist Destination
# 67: Travel

time <- paste("2019-12-01 ",Sys.Date(), sep = "")

keywords=c("italy", "spain", "greece", "germany", "france", "portugal")
##keywords=c("rome", "barcelona", "athen", "berlin", "paris")

#set the time window
destination <- sapply(keywords, function(x) gtrends(x, geo = "", time, onlyInterest=TRUE, category = 208))

##destination_l<- data.frame(destination$rome.interest_over_time$hits,
  #                         destination$barcelona.interest_over_time$hits,destination$athen.interest_over_time$hits,
  #                         destination$berlin.interest_over_time$hits, destination$paris.interest_over_time$hits)

destination_l<- data.frame(destination$italy.interest_over_time$hits,
                     destination$spain.interest_over_time$hits,destination$greece.interest_over_time$hits,
                     destination$germany.interest_over_time$hits, destination$france.interest_over_time$hits,
                     destination$portugal.interest_over_time$hits)

colnames(destination_l) <- keywords
head(destination_l)

destination_ts <- as.xts(destination_l, order.by =destination$italy.interest_over_time$date )

destination_week <-  rollapply(destination_ts, 7, mean)
destination_week <- as.xts(apply(destination_week,2, function(x)  x*100/x["2020-01-01"]))

plot(destination_week)

dygraph(destination_week["20191201/"], main = "Tourist Destination", width = 800)%>%
  dyAxis("x", drawGrid = FALSE)%>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))%>%
  dyOptions(colors = c("black","maroon", "gray", "red", "blue", "orange", "green", "navy"))%>%
  dyLegend(labelsSeparateLines = TRUE, show = "always")%>%
  dyCSS("legend.css")%>%
  dyRangeSelector()%>%
  dyUnzoom()

p<- autoplot.zoo(destination_week["201901/"], facets = NULL, main = "") +
  aes(linetype = Series, size=Series) + 
  scale_color_manual(values=c("black","maroon", "gray", "navy", "blue", "red", "orange")) +
  scale_size_manual(values = c(0.7, 0.7, 0.7, 0.7, 0.7,0.7,0.7))+
  theme_bw() +xlab("") + 
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title = "Tourist Destination",
       caption = "Quelle: Flossbach von Storch Research Institute, \n Google Trends")

p + theme(plot.title = element_text(hjust = 0.5))
ggsave("tourist_dest.png")


###Insolvenz
time <- paste("2019-10-01 ",Sys.Date(), sep = "")
keywords <- c("insolvenz")
insolvenz <- gtrends(keywords, geo = "DE", time, onlyInterest=TRUE)
hh <- insolvenz$interest_over_time
hh <- data_frame(hh$date,hh$geo,hh$hits)
colnames(hh)<-c("time","country","value")
write.table(hh, file="C:/Users/DuarteP/OneDrive - Flossbach von Storch AG/FvS_work/nowcasting/tracker/insolvenz.csv",sep=",",row.names=F)

head(hh)
insolvenz_ts <- ts_xts(hh)

insolvenz_week <-  rollapply(insolvenz_ts, 7, mean)

p<- autoplot.zoo(insolvenz_week["201901/"], facets = NULL, main = "") +
  aes(linetype = Series, size=Series) + 
  scale_color_manual(values=c("black","maroon", "gray", "navy", "blue", "red", "orange")) +
  scale_size_manual(values = c(0.7, 0.7, 0.7, 0.7, 0.7,0.7,0.7))+
  theme_bw() +xlab("") + 
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title = "Google Trends: Insolvenz",
       caption = "7-Tages-Durchschnitte. \n Quelle: Flossbach von Storch Research Institute, \n Google Trends")

p + theme(plot.title = element_text(hjust = 0.5))
ggsave("C:/Users/DuarteP/OneDrive - Flossbach von Storch AG/FvS_work/nowcasting/tracker/insolvenz.jpg")

write.table(hh, file="C:/Users/DuarteP/OneDrive - Flossbach von Storch AG/FvS_work/nowcasting/tracker/insolvenz.csv",sep=",",row.names=F)

