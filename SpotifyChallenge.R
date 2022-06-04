library(readxl)
SpotifyChallenge <- data.frame(read_excel("SpotifyChallenge.xlsx"))
View(SpotifyChallenge)



totalOrderAmount = 0

myList<-list()
aovList<-list()

x<-1:100

for (val in x) {
  
  myList[[val]]<-subset(SpotifyChallenge,shop_id ==val)
  aov =  sum(myList[[val]][4])/nrow(myList[[val]])
  aovList[[val]]<-aov
  
  
}



sumAOV = 0
for (val in x) {
  sumAOV = sumAOV + aovList[[val]]
 
  
  
}
