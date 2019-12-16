# the total valid sample
a <- read.csv("dataset.CSV",header=F)
na = length(a[,1])

sav1 <- summary(a$V1)
pie(sav1,labels=paste(names(sav1),": ",round(100*sav1/sum(sav1),2),"%",sep=""),main="Net Bar Consideration")

# sample that consider going to net bar
b <- a[a$V1!="No",]
nb <- length(b[,1])

b$V2[-36]

plot(b$V2,b$V3,pch=21,bg=c("red","blue","green")[b$V1],main="ScatterPlot of Time and Price")
legend("topright",pch=c(21,21),pt.bg=c("green","red"),legend=c("Yes","Maybe"))

# sample that needs private room
c <- b[b$V5=="Yes",]
nc <- length(c[,1])

sbv5 <- summary(b$V5)
pie(sav1,labels=paste(names(sbv5),": ",round(100*sbv5/sum(sbv5),2),"%",sep=""),main="Private room Consideration")

c$V6[2<c$V6&c$V6<8]

# ratios
cat("user rate:",nb/na,'\n')
cat("user private rate:",nc/nb,'\n')
cat("total private rate:",nc/na,'\n')
