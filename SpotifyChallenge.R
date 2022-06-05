library(readxl)
SpotifyChallenge <- data.frame(read_excel("SpotifyChallenge.xlsx"))
View(SpotifyChallenge)
library(ggrepel)


#Average order value of the dataset 
aov<-sum(SpotifyChallenge$order_amount)/nrow(SpotifyChallenge)

#Show mean median and mode
#Mean substantially higher then the mode.
summary(SpotifyChallenge$order_amount)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#Mode substantially lower then the mean.
#There are few high dollar orders that skew up the average
getmode(SpotifyChallenge$order_amount)


totalOrderAmount = 0

myList<-list()
aovList<-list()

x<-1:100

for (val in x) {
  
  myList[[val]]<-subset(SpotifyChallenge,shop_id ==val)
  aov =  sum(myList[[val]][4])/nrow(myList[[val]])
  aovList[[val]]<-aov
  
  
}

aov.df<-as.data.frame(aovList)
finaldf<-as.data.frame(t(aov.df))
row.names(finaldf)<-1:100
summary(finaldf)



require("ggrepel")
set.seed(42)
ggplot(finaldf,aes(x="",y=V1))+geom_boxplot()+
  scale_y_continuous(name = "aov",breaks = c(150,200,600,245101))+coord_trans(y = "log10") +
  geom_text_repel(aes(label = rownames(finaldf)),
            size = 3.5)




outlierRemoved<- subset(SpotifyChallenge,shop_id!= 78 & shop_id != 42 )
summary(outlierRemoved$order_amount)

##Mean is now 300.2


