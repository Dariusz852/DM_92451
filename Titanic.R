#Grupa: Dariusz Tomaszuk, Dominika Drzemicka i Bartosz Za³êcki


#Miejsce gdzie znajduja sie pliki
setwd("C:/Users/Darek/Downloads")

#Wczytywanie danych trains
train <- read.csv("C:/Users/Darek/Downloads/train.csv")

#Wczytywanie danych test
test <- read.csv("C:/Users/Darek/Downloads/test.csv")

#Wyswietla tabelke
str(train)

train <- read.csv("train.csv", stringsAsFactors=FALSE)

#Podzial ta tych co przezyja i nie
table(train$Survived)

#podzial procentowy na ocalalych i nie
prop.table(table(train$Survived))

test$Survived <- rep(0, 418)

#tworzy plik submit. data.frame tworzy 
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

#The write.csv command has sent that dataframe out to a CSV file
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

#czesc 2

#ilosc kobiet i mezczyzn
summary(train$Sex)

#ilosc kobiet i mezczyzn w procentach
prop.table(table(train$Sex, train$Survived))

#Proporcie w wierszach
prop.table(table(train$Sex, train$Survived),1)

#Here we have begun with adding the “everyone dies” prediction column as before, except that we’ll ditch the rep command and just assign the zero to the whole column, it has the same effect. We then altered that same column with 1’s for the subset of passengers where the variable “Sex” is equal to “female”
#Wydzielamy kobiety ktore przezyja
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

#Wypisuje wiek
summary(train$Age)

#Tworzymy osobna kategorie dla dzieci
train$Child <- 0
train$Child[train$Age < 18] <- 1

#agregujemy dane do tabelki i wypisujemy dzieci ktore przezyja w podziale na plec
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

#Doprecyzujemy dane
aggregate(Survived ~ Child + Sex, data=train, FUN=length)

#Procent dzieci ktore przezyja
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#dzielimy na taryfy od 30 dolarow
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

#Laczymy taryfy razem z iinymi danymi
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

#plik na dane za 2 etap
write.csv(test, file="Part2.csv")

#czesc 3 Tworzenie uczenia maszynowego

#import biblioteki
library(rpart)

#budowa pierwszego modelu
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train,method = "class")

#Proba drzewa
plot(fit)
text(fit)

#Pakiety do drzewa
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#renderowanie drzewa
fancyRpartPlot(fit)

#Przewidywania do drzewa i wyprowadzanie danych dalej
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#bardziej zlozone drzewo
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train,method="class",control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

#Czesc 4

train$Name[1]

#W testcie nie pokrywaja sie kolumny i trzeba to naprawic
test$Child <- NA
test$Fare2 <- NA

#laczenie
combi <- rbind(train, test)

#Wyciaganie danych z combi
combi$Name <- as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

#laczenie po title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

combi$Title <- sub(' ', '', combi$Title)

#wyciaganie danych
table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

#tworzenie tabeli
table(combi$FamilyID)

#tworzenie pliku w data
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891,]
test <- combi[892:1309,]

#nowa wersja drzewa
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data=train,method="class")

#wyswietlanie drzewa
fancyRpartPlot(fit)

#Czesc 5

#wypisywanie wedlug kryteriow
sample(1:10, replace = TRUE)
summary(combi$Age)

#tworzenie Agefity w data
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=combi[!is.na(combi$Age),],  method="anova")

#laczenie combi
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

#sumwanie combie
summary(combi)
summary(combi$Embarked)

which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

#sumowanie combie
summary(combi$Fare)

#oytanie do zapytania combi
which(is.na(combi$Fare))

combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

#potrzebny pakiek
install.packages('randomForest')
library(randomForest)

set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data=train, importance=TRUE, ntree=2000)

#Wykres
varImpPlot(fit)

#tworzenie pliku
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

#pakiet dodatkowy
install.packages('party')
library(party)

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")
