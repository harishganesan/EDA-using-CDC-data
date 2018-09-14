library(ggplot2)
library(reshape2)
library(maps)

#----------------- Influenza national summary - Graph 1
NationalSummary_dataset <- read.csv(file.choose()) #-- National_Summary.csv

colnames(NationalSummary_dataset) <- c("Week","TotalA","TotalB","PercentPositiveA","PercentPostiveB","TotalTested","XPositive")

NationalSummary_dataset$Week <- factor(NationalSummary_dataset$Week)

head(NationalSummary_dataset)

total<-NationalSummary_dataset$TotalA + NationalSummary_dataset$TotalB
NationalSummary_dataset$TotalAB <- total

b <- ggplot(data = NationalSummary_dataset, aes(x = Week, y = TotalTested)) +
  geom_bar(stat="identity")
b
b_dat <- melt(NationalSummary_dataset,id="Week")
head(b_dat)
b_dat$group <- gsub(".+_","",b_dat$variable)

b1 <- subset(b_dat,group %in% c("TotalA","TotalB"))
b2 <- subset(b_dat, group %in% c("XPositive"))
b3 <- subset(b_dat, group %in% c("TotalAB"))

head(b1)
head(b2)
# b1$group <- gsub(".+_","",b1$variable)

#-- X Positive
max_Total <- max(NationalSummary_dataset$TotalAB)
z = c()
for(i in 1:length(NationalSummary_dataset$Week)){
  z = c(z,(max_Total*NationalSummary_dataset$XPositive[i])/28)
}

z <- data.frame(z)
z$Week <- c("201740","201741","201742","201743","201744","201745","201746","201747","201748","201749",
            "201750","201751","201752","201801","201802","201803","201804")
z$Week <- factor(z$Week)

#-- A positive
max_Total <- max(NationalSummary_dataset$TotalAB)
z1 = c()
for(i in 1:length(NationalSummary_dataset$Week)){
  z1 = c(z1,(max_Total*NationalSummary_dataset$PercentPositiveA[i])/28)
}

z1 <- data.frame(z1)
z1$Week <- c("201740","201741","201742","201743","201744","201745","201746","201747","201748","201749",
            "201750","201751","201752","201801","201802","201803","201804")
z1$Week <- factor(z1$Week)

#-- B Positive
max_Total <- max(NationalSummary_dataset$TotalAB)
z2 = c()
for(i in 1:length(NationalSummary_dataset$Week)){
  z2 = c(z2,(max_Total*NationalSummary_dataset$PercentPostiveB[i]/28))
}

z2 <- data.frame(z2)
z2$Week <- c("201740","201741","201742","201743","201744","201745","201746","201747","201748","201749",
             "201750","201751","201752","201801","201802","201803","201804")
z2$Week <- factor(z2$Week)

#----Final Graph
newZ <- z
newZ$z1 <- z1$z1
newZ$z2 <- z2$z2

p3 <- ggplot(data = b3, aes(x = Week, y = value)) +
  geom_bar(data = b1,aes(group=variable,fill=variable),stat="identity", color="Black")+
  scale_fill_manual(values = c("Yellow","Dark Green"))+
  scale_y_continuous(limits = c(0,18000),breaks=c(0,2000,4000,6000,8000,10000,12000,14000,16000,18000),
                     sec.axis = sec_axis(~.*30/18000, name = "Percent Positive"))+
  geom_line(data = newZ,aes(x=as.numeric(Week),y=z,group=1, linetype = "Percent Positive"),stat = "identity", color = "Black")+
  geom_line(data = newZ,aes(x=as.numeric(Week),y=z1,group=1, linetype = "% Positive Flu A"),stat = "identity", color = "Blue")+
  geom_line(data = newZ,aes(x=as.numeric(Week),y=z2,group=1, linetype ="% Positive Flu B"),stat = "identity", color = "Red")+
  scale_linetype_manual(values = c("Percent Positive" = 1 ,"% Positive Flu A" = 2,"% Positive Flu B" = 3))+
  theme(axis.text.x = element_text(size = 8, angle = 50, vjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())+
  ggtitle("Influenza Positive Tests Reported to CDC by 
          U.S. Clincal Laboratories,
          National Summary, 2017-2018 Season")+
  ylab("Number of Positive Specimens")+
  guides(fill = guide_legend(keywidth = 3, keyheight = 1))

p3 #--Part2_Graph1.png

#---------------------- Positive tested - Graph 2

positiveTested_dataset <- read.csv(file.choose()) #-- PositiveTested.csv
head(positiveTested_dataset)
colnames(positiveTested_dataset) <- c("Week","H3N2v","A(H1N1)","A(H3N2)","A(unable to sub type)",
                                      "A(subtyping not performed)","B(Lineage not perfomed)",
                                      "B(Victoria Lineage)","B(Yamagata Lineage)","TotalTested")

positiveTested_dataset$Week <- factor(positiveTested_dataset$Week)

positiveTested_dataset <- positiveTested_dataset[c(1,6,3,4,2,7,8,9,5,10)]

pt_data <- melt(positiveTested_dataset,id="Week")
head(pt_data)
pt_data$group <- gsub(".+_","",pt_data$variable)
pt1 <- subset(pt_data,group %in% c("A(subtyping not performed)","A(H1N1)","A(H3N2)","H3N2v",
                                   "B(Lineage not perfomed)",
                                   "B(Victoria Lineage)","B(Yamagata Lineage)"))
head(pt1)

q1 <- ggplot()+
  geom_bar(data = pt1,aes(x=Week,y=value,group=variable,fill=variable),stat="identity", color="Black")+
  scale_y_continuous(limits = c(0,4000),breaks=c(0,500,1000,1500,2000,2500,3000,3500,4000))+
  theme(axis.text.x = element_text(size = 8, angle = 50, vjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_blank())+
  ggtitle("Influenza Positive Tests Reported to CDC by 
          U.S. Public Health Laboratories, 
          National Summary, 2017-2018 Season")+
  ylab("Number of Positive Specimens")

q1 #-- Part2_Graph2.png

#-------------- Mortality - Graph 3

mortality_dataset <- read.csv(file.choose()) #-- NCHSData04.csv
head(mortality_dataset)
colnames(mortality_dataset) <- c("Year", "Week", "PercentPnI", "Expected",
                                 "Threshold", "All Deaths","PDeaths","IDeaths")

r1 <- ggplot()+
  geom_line(data = mortality_dataset, aes(x = Date, y = PercentPnI))

r1

for(i in 1:length(mortality_dataset$PercentPnI)){
  if(nchar(mortality_dataset$Week[i]) < 2){
    mortality_dataset$Week[i] = paste(0,mortality_dataset$Week[i], sep='')
  }
}

nDate2 <- as.Date(paste(mortality_dataset$Date,1),"%Y%U%u")
mortality_dataset$nDate <- nDate2

m1 <- subset(mortality_dataset, Date >= 201340)

z9 = c()
for(i in 1:length(m1$PercentPnI)){
  z9 = c(z9,(paste(m1$Year[i],m1$Week[i],sep='')))
}

m1$Date <- z9

ndate <- as.Date(paste(z9,1),"%Y%U%u")
m1$nDate <- ndate

z10 = c()
for(i in 1:length(m1$Date)){
  z10 = c(z10,(m1$PercentPnI[i]))
}

z10 <- data.frame(z10)
z10$nDate <- m1$nDate

z10$Date <- z9
z10$Date <- factor(z9)
z10$Date = as.numeric(levels(z10$Date))[z10$Date]

z11 = c()
for(i in 1:length(m1$Date)){
  z11 = c(z11,(m1$Expected[i]))
}

z11 <- data.frame(z11)
z11$nDate <- m1$nDate

z11$Date <- z9
z11$Date <- factor(z9)

z12 = c()
for(i in 1:length(m1$Date)){
  z12 = c(z12,(m1$Threshold[i]))
}

z12 <- data.frame(z12)
z12$nDate <- m1$nDate

z12$Date <- z9
z12$Date <- factor(z9)


r3 <- ggplot()+
  geom_line(data = z10,aes(x = nDate, y = z10, group = 1), stat = "identity", color ="Red")+
  geom_line(data = z11,aes(x = nDate, y = z11, group = 1), stat = "identity")+
  geom_line(data = z12,aes(x = nDate, y = z12, group = 1), stat = "identity")+
  scale_y_continuous(limits = c(4,12), breaks = c(4, 6, 8 , 10, 12))+
  ylab("% of All Deaths Due to P&I")+
  xlab("MMRWEEK")+
  ggtitle("Pneumonia & Influenza mortality")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_labels = "%Y%W", date_breaks = "10 week")

r3  #-- Part2_Graph3.png

#--- Another method to plot the Graph
r2 <- ggplot()+
  geom_line(data = z10,aes(x = as.numeric(Date), y = z10, group = 1), stat = "identity", color ="Red")+
  geom_line(data = z11,aes(x = as.numeric(Date), y = z11, group = 1), stat = "identity")+
  geom_line(data = z12,aes(x = as.numeric(Date), y = z12, group = 1), stat = "identity")+
  scale_y_continuous(limits = c(4,12), breaks = c(4, 6, 8 , 10, 12))+
  ylab("% of All Deaths Due to P&I")+
  xlab("MMRWEEK")+
  ggtitle("Pneumonia & Influenza mortality")+
  theme(plot.title = element_text(hjust = 0.5))

r2  

#-----------Influenza sub-type pie-charts 
#### PIE CHARTS

# Main pie chart
par(mfrow=c(1,1))
mainChart <- read.csv(file.choose()) #-- MainPieChart.csv
mainChart1 <- colSums(mainChart)
mainChart2 <- c(mainChart1[3],mainChart1[4],mainChart1[6],mainChart1[7],mainChart1[8],mainChart1[9])
nam <- names(mainChart1)
lbls <- c(nam[3],nam[4],nam[6],nam[7],nam[8],nam[9])
lbls <- paste(lbls, mainChart2)
mainp <- pie(mainChart2, labels = lbls, main = "Influenza Positive Tests Reported to CDC by
             U.S. Public Health Laboratories 
             2017-2018 Season")

#Reading data forother 4 pie charts
genetic <- read.csv(file.choose(),stringsAsFactors = FALSE) #-- Genetic04.csv
genetic[genetic==""] <- NA
genetic[genetic==" "] <- NA
genetic[is.na(genetic)] <- "0%"

par(mfrow=c(2,2))
#Plotting 1st pie chart
pie1 <- genetic[genetic$X.Sub.type == "H3", ]
lbls <- pie1$Genetic_Group
lbls <- paste(lbls, pie1$X..of..Sub.type.Total) # add percents to labels 
h1 <- pie(pie1$Number, labels = lbls , main="Influenza A H3N2", col=rainbow(length(pie1$Genetic_Group)), radius = 1.0)

#Plotting 2nd pie chart
pie2 <- genetic[genetic$X.Sub.type == "H1pdm09", ]
lbls <- pie2$Genetic_Group
lbls <- paste(lbls, pie2$X..of..Sub.type.Total) # add percents to labels 
h2 <- pie(pie2$Number, labels = lbls , main="H1pdm09", col=rainbow(length(pie2$Genetic_Group)), radius = 1.0)

#Plotting 3rd pie chart
pie3 <- genetic[genetic$X.Sub.type == "B/Victoria", ]
lbls <- pie3$Genetic_Group
lbls <- paste(lbls, pie3$X..of..Sub.type.Total) # add percents to labels
h3 <- pie(pie3$Number, labels = lbls , main="B/Victoria", col=rainbow(length(pie3$Genetic_Group)), radius = 1.0)

#Plotting 4th pie chart
pie4 <- genetic[genetic$X.Sub.type == "B/Yamagata", ]
lbls <- pie4$Genetic_Group
lbls <- paste(lbls, pie4$X..of..Sub.type.Total) # add percents to labels
h4 <- pie(pie4$Number, labels = lbls , main="B/Yamagata", col=rainbow(length(pie4$Genetic_Group)), radius = 1.0)

#-- Final Graphs: Part2_Pie_Chart_1.png and Part2_Pie_Chart_2.png

#---------------Heat Map

states <- map_data("state")
head(states)
heatmap_data <- read.csv(file.choose()) #-- StateDataforMap_2017-18week8.csv
head(heatmap_data)
heatmap_data$STATENAME = tolower(heatmap_data$STATENAME)
merged_states <- merge(states, heatmap_data, by.x = "region",by.y = "STATENAME")
head(merged_states)

map1 <- ggplot(merged_states, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=ACTIVITY.LEVEL))+
  coord_map()+
  ggtitle("2017-18 Influenza Season Week 8 ending Feb 24, 2018")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Activity Level"))
  
map1 #-- Part2_HeatMap.png
