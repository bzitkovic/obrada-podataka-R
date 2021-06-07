# Uèitavanje podataka
Knjige = read.delim(file.choose(),header=T,sep=",")

attach(Knjige)

# Grafièki prikaz Jezici
class(Language)
dim(Knjige)
Language<-as.character(Language)
count<-table(Language)
count
count<-sort(count, decreasing = TRUE )
count
postoci<-round(100*count/dim(Knjige)[1],2)
barplot(count[2:10], col=rgb(0.2,0.4,0.6,0.6))
barplot(postoci[1:10], col = terrain.colors(6), main="Struktura po jezicima",xlab="Jezik",ylab="Postotak", las=1)
barplot(count[2:10], col = terrain.colors(6), main="Struktura po jezicima",xlab="Jezik",ylab="#", las=1)

postoci <- c(postoci)
pie(postoci[1:5], labels=postoci,edges = 900, radius = 1, col = terrain.colors(5), main="Struktura po jezicima")
legend(-2, 1.0, c("Neodreðeno", "eng", "eng-US", "spa", "en-GB"), NULL, fill=terrain.colors(5))


# Grafièki prikaz pagesNumber
boxplot(pagesNumber, col = terrain.colors(5), varwidth=FALSE, outline=FALSE, ylab="", main="")

# Grafièki prikaz rating1
require(stringr)
rating1<-str_sub(RatingDist1, start = 3, end = -1L)
rating1<-as.numeric(rating1)
rating1
class(rating1)
boxplot(rating1, col = terrain.colors(5), varwidth=FALSE, outline=FALSE, ylab="Ocjena (1)", main="Knjige ocjenjene jednom zvjezdicom")


# Grafièki prikaz rating2
require(stringr)
rating2<-str_sub(RatingDist2, start = 3, end = -1L)
rating2<-as.numeric(rating2)
boxplot(rating2, col = terrain.colors(5), varwidth=FALSE, outline=FALSE, ylab="Ocjena (2)", main="Knjige ocjenjene s dvije zvjezdice")


# Grafièki prikaz rating3
require(stringr)
rating3<-str_sub(RatingDist3, start = 3, end = -1L)
rating3<-as.numeric(rating3)
boxplot(rating3, col = terrain.colors(5), varwidth=FALSE, outline=FALSE, ylab="Ocjena (3)", main="Knjige ocjenjene s tri zvjezdice")

# Grafièki prikaz rating4
require(stringr)
rating4<-str_sub(RatingDist4, start = 3, end = -1L)
rating4<-as.numeric(rating4)
rating4
class(rating4)
boxplot(rating4, col = terrain.colors(5), varwidth=FALSE, outline=FALSE, ylab="Ocjena (4)", main="Knjige ocjenjene s èetiri zvjezdice")

# Grafièki prikaz rating5
require(stringr)
rating5<-str_sub(RatingDist5, start = 3, end = -1L)
rating5<-as.integer(rating5)
boxplot(rating5, col = terrain.colors(5), varwidth=FALSE, outline=FALSE, ylab="Ocjena (5)", main="Knjige ocjenjene s pet zvjezdice")


# Grafièki prikaz ratingTotal
require(stringr)
ratingTotal<-str_sub(RatingDistTotal, start = 7, end = -1L)
ratingTotal<-as.numeric(ratingTotal)
boxplot(ratingTotal, col = terrain.colors(5), varwidth=FALSE, outline=FALSE, ylab="Ocjena ukupno", main="Ukupni broj zvjezdica")

# Grafièki prikaz godina
require(stringr)
class(PublishYear)
boxplot(PublishYear, col = terrain.colors(5), outline=FALSE, ylab="Godina", main="Knjiga izdana prema godini")

# Grafièki prikaz dan, tj. mjesec
require(stringr)
class(PublishDay)
boxplot(PublishDay, col = terrain.colors(5), outline=TRUE, ylab="Mjesec", main="Knjiga izdana prema mjesecu")

# Grafièki prikaz mjesec, tj. dan
require(stringr)
class(PublishMonth)
boxplot(PublishMonth, col = terrain.colors(5), outline=TRUE, ylab="Dan", main="Knjiga izdana prema danu")

# Grafièki prikaz izdavaèi
count<-table(Publisher)
count
count<-sort(count, decreasing = TRUE )
count
postoci<-round(100*count/dim(Knjige)[1],2)
dim(Knjige)[1]
barplot(count[1:5], col=rgb(0.2,0.4,0.6,0.6))
barplot(postoci[1:5], col = terrain.colors(6), main="Struktura po izdavaèima",xlab="Izdavaè",ylab="Postotak", las=1)
barplot(count[1:5], col = terrain.colors(6), main="Struktura po izdavaèima",xlab="Izdavaè",ylab="Broj naklada", las=1)

postoci <- c(postoci)
pie(postoci[1:5], labels=postoci,edges = 900, radius = 1, col = terrain.colors(5), main="Struktura po izdavaèima")
legend(-2.1, 1.0, c("Vintage", "Oxford University", "Penguin Books", "Routledge", "Cambridge University"), NULL, fill=terrain.colors(5))
      

# Grafièki prikaz review
class(CountsOfReview)
boxplot(CountsOfReview, col = terrain.colors(5), outline=FALSE, ylab="Broj recenzija", main="Knjiga prema broju recenzija")

# Grafièki prikaz avgRating
class(Rating)
boxplot(Rating, col = terrain.colors(5), outline=TRUE, ylab="Projeèni broj ocjene", main="Knjiga prema prosjeènom broju zvjezdica")


# Grafièki prikaz autorima
count<-table(Authors)
count
count<-sort(count, decreasing = TRUE )
count
postoci<-round(100*count/dim(Knjige)[1],2)
barplot(count[1:5], col=rgb(0.2,0.4,0.6,0.6))
barplot(postoci[1:5], col = terrain.colors(6), main="Struktura po autorima",xlab="Autor",ylab="Postotak", las=1)
barplot(count[1:5], col = terrain.colors(6), main="Struktura po autorima",xlab="Autor",ylab="Broj napisanih knjiga", las=1)

postoci <- c(postoci)
pie(postoci[1:5], labels=postoci,edges = 900, radius = 1, col = terrain.colors(5), main="Struktura po autorima")
legend(-2.12, 1.0, c("William Shakespeare", "Carolyn Keene", "Anonymous", "Isaac Asimov", "Piers Anthony"), NULL, fill=terrain.colors(5))


# Grafièki prikaz rating/godina
class(Rating)
rating<-round(Rating,0)
boxplot(Rating~PublishDay, col = terrain.colors(5), outline=TRUE, xlab="Mjesec", ylab="Prosjeèna ocjena", main="Prosjeèna ocjena knjige prema mjesecu objavljivanja")
boxplot(pagesNumber~rating, col = terrain.colors(5), outline=FALSE, xlab="Ocjena", ylab="Broj stranica", main="Stranice knjige prema prosjeènom broju ocjena")


# Matrica korelacije
#data<-matrix(PublishMonth, PublishDay, Rating, pagesNumber, rating1, rating2, rating3, rating4, rating5, nrow = 9, ncol = 9)
require(stringr)
rating1<-str_sub(RatingDist1, start = 3, end = -1L)
rating1<-as.integer(rating1)

rating2<-str_sub(RatingDist2, start = 3, end = -1L)
rating2<-as.integer(rating2)

rating3<-str_sub(RatingDist3, start = 3, end = -1L)
rating3<-as.integer(rating3)

rating4<-str_sub(RatingDist4, start = 3, end = -1L)
rating4<-as.integer(rating4)

rating5<-str_sub(RatingDist5, start = 3, end = -1L)
rating5<-as.integer(rating5)

class(Knjige$RatingDist1)

data<-matrix(c(Knjige$PublishMonth, Knjige$PublishDay, Knjige$Rating, Knjige$pagesNumber, Knjige$PublishYear, rating1, rating2, rating3, rating4, rating5), ncol = 10, nrow = 10)
colnames(data) <- c("PD","PM","Rating","PN","PY","RD1","RD2","RD3","RD4","RD5")
res <- round(cor(data),2)

library(writexl)
write_xlsx(as.data.frame(res), path = "results.xlsx")


palette<-colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = res, col = palette, symm = TRUE)

library(corrplot)
corrplot(res,type = "lower", order = "hclust", tl.col = "black", tl.srt = 0)

library("PerformanceAnalytics")
chart.Correlation(res, histogram=TRUE, pch=19)

# Normalnost razdioba
shapiro.test(Knjige$PublishMonth[3:300])
shapiro.test(Knjige$PublishDay[3:300])
shapiro.test(Knjige$Rating[3:300])
shapiro.test(Knjige$pagesNumber[3:300])
shapiro.test(Knjige$PublishYear[3:300])
shapiro.test(rating1[3:300])
shapiro.test(rating2[3:300])
shapiro.test(rating3[3:300])
shapiro.test(rating4[3:300])
shapiro.test(rating5[3:300])

qqnorm(Knjige$PublishMonth, pch = 1, frame = FALSE)
qqline(Knjige$PublishMonth, col = "steelblue", lwd = 2)

library("car")
qqPlot(Knjige$PublishMonth[3:300])
qqPlot(Knjige$PublishDay[3:300])
qqPlot(Knjige$Rating[3:300])
qqPlot(Knjige$pagesNumber[3:300])
qqPlot(Knjige$PublishYear[3:300])
qqPlot(rating1[3:300])
qqPlot(rating2[3:300])
qqPlot(rating3[3:300])
qqPlot(rating4[3:300])
qqPlot(rating5[3:300])

# Dodavanje nove varijable Starost
Knjige$Starost <- cut(Knjige$PublishYear,breaks=c(162,1995,2010,Inf),labels=c("Do1995", "1995Do2010", "2010Nadalje"),
              as.factor.result=TRUE)

count<-table(Knjige$Starost)
count<-sort(count, decreasing = TRUE )
count

postoci<-round(100*count/dim(Knjige)[1],2)
pie(postoci, labels=postoci, edges = 900, radius = 1, col = terrain.colors(5), main="Struktura po starosti")
legend(-2, 1.0, c("1995Do2010", "Do1995", "2010Nadalje"), NULL, fill=terrain.colors(5))

# Dodavanje nove varijable Languge1
count<-table(Knjige$Language1)
count<-sort(count, decreasing = TRUE )

Knjige$Language1 <- Knjige$Language

Knjige$Language1[Knjige$Language1 == "eng"] <- "eng"
Knjige$Language1[Knjige$Language1 == "en-US"] <- "en-US"
Knjige$Language1[Knjige$Language1 == "spa"] <- "spa"
Knjige$Language1[Knjige$Language1 == ""] <- "Ostalo"
Knjige$Language1[Knjige$Language1 != "eng" & Knjige$Language1 != "en-US" & Knjige$Language1 != "spa"] <- "Ostalo"

postoci<-round(100*count/dim(Knjige)[1],2)
pie(postoci, labels=postoci, edges = 900, radius = 1, col = terrain.colors(5), main="Struktura po jezicima")
legend(-2, 1.0, c("Ostalo", "eng", "en-US", "spa"), NULL, fill=terrain.colors(5))

# Ispitivanje razlike varijable Rating i Starost
library(car)

tapply(Knjige$Rating, Knjige$Starost, mean)
bartlett.test(Knjige$Rating ~ Knjige$Starost)
leveneTest(Knjige$Rating ~ Knjige$Starost, center=mean)

boxplot(Knjige$Rating ~ Knjige$Starost, col = terrain.colors(5), varwidth=TRUE, outline=TRUE,xlab="Starost", ylab="Rating", main="Rating/Starost")

kruskal.test(Knjige$Starost~Knjige$Rating)

AnovaModel.2 <- aov(Knjige$Rating~Knjige$Starost)
summary(AnovaModel.2)  

# Ispitivanje razlike varijable Broj recenzija i Jzeik
tapply(Knjige$CountsOfReview, Knjige$Language1, mean)
bartlett.test(Knjige$CountsOfReview,Knjige$Language1)
leveneTest(Knjige$CountsOfReview ~ Knjige$Language1, center=mean)

boxplot(Knjige$CountsOfReview ~ Knjige$Language1, col = terrain.colors(5), varwidth=TRUE, outline=FALSE,xlab="Jezik", ylab="Broj recenzija", main="Broj recenzija/Jezik")

kruskal.test(Knjige$CountsOfReview ~ Knjige$Language1)

AnovaModel.2 <- aov(Knjige$CountsOfReview ~ Knjige$Language1)
summary(AnovaModel.2)   

# Regresijski model Rating
library(psych)
library(corrplot)

RegModelRating <- lm(Rating[1:500] ~ pagesNumber[1:500] + CountsOfReview[1:500] + Starost[1:500] + Language[1:500])
summary(RegModelRating)

# Raèunanje po granama
do1995<-subset(Knjige, Starost  == "Do1995")
od1995Do2010<-subset(Knjige, Starost  == "1995Do2010")
od2010Nadalje<-subset(Knjige, Starost  == "2010Nadalje")

RegModelStarost1 <- lm(Knjige$Rating[1:500] ~ Knjige$pagesNumber[1:500] + Knjige$RatingDistTotal[1:500] + Knjige$CountsOfReview[1:800], data=do1995)
summary(RegModelStarost1)

RegModelStarost2 <- lm(Knjige$Rating[1:500] ~ Knjige$pagesNumber[1:500] + Knjige$RatingDistTotal[1:500] + Knjige$CountsOfReview[1:500], data=od1995Do2010)
summary(RegModelStarost2)

RegModelStarost3 <- lm(Knjige$Rating[1:500] ~ Knjige$pagesNumber[1:500] + Knjige$RatingDistTotal[1:500] + Knjige$CountsOfReview[1:500], data=od2010Nadalje)
summary(RegModelStarost3)


# Izbor varijabli RegModelRating
library(MASS)
step1 <- stepAIC(RegModelRating, direction="both")
summary(step1)
step2 <- stepAIC(RegModelRating, direction="backward")
step3 <- stepAIC(RegModelRating, direction="forward")

# Normalni prikaz rezidualnih vrijednosti RegModelRating
plot(RegModelRating, which=2, pch=20, lwd=2, font=2, font.lab=2, font.sub=2, las=1)
shapiro.test(residuals(RegModelRating))

# Regresijski model RatingDistTotal
RegModelRatingDistTotal <- lm(ratingTotal[1:500] ~ pagesNumber[1:500] + CountsOfReview[1:500] + Starost[1:500] + Language[1:500])
summary(RegModelRatingDistTotal)

# Izbor varijabli RatingDistTotal
library(MASS)
step1 <- stepAIC(RegModelRatingDistTotal, direction="both")
summary(step1)
step2 <- stepAIC(RegModelRatingDistTotal, direction="backward")
step3 <- stepAIC(RegModelRatingDistTotal, direction="forward")

# Normalni prikaz rezidualnih vrijednosti RatingDistTotal
plot(RegModelRatingDistTotal, which=2, pch=20, lwd=2, font=2, font.lab=2, font.sub=2, las=1)
shapiro.test(residuals(RegModelRatingDistTotal))
