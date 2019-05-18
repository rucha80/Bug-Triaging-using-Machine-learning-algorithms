#Importing dataset
dataset = read.delim("final dataset for work ecllipse.txt",quote = '',
                     stringsAsFactors = FALSE,na.strings = c(" "))
dataset$Summary.1 <- NULL
dataset$Alias <- NULL

#check weather we have null entries in developer column
null_data <- dataset[is.na(dataset$Assignee),]

#counting no of bugs assigned to each developer
p<- table(dataset$Assignee.Real.Name)
q <- as.data.frame(p,row.names = NULL, 
                   responseName = "NO_of_bugs_Assigned", 
                   stringsAsFactors = TRUE)

#removing data of developers having bug count more than threshold 
#upper threshold t1 and lower threshold t2
colnames(q) <- c("Devloper_names","No_of_bugs_Assigned")
r <- q[!(q$No_of_bugs_Assigned<t1) | !(q$No_of_bugs_Assigned>t2),]
s <- nrow(r)
dataset_1<- dataset
for(i in 1:s){
  dataset_1 <- dataset_1[!(dataset_1$Assignee.Real.Name == r$Devloper_names[i]), ]
}

#Creating Bag of Words Model
install.packages('tm')
library(tm)
install.packages('SnowballC')
library(SnowballC)
corpus=VCorpus(VectorSource(dataset_1$Summary))
corpus=tm_map(corpus,content_transformer(tolower)) #convert to lower case
corpus=tm_map(corpus,removeNumbers)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,stopwords())
corpus=tm_map(corpus,stemDocument)
corpus=tm_map(corpus,stripWhitespace)

dtm=DocumentTermMatrix(corpus)
dtm=removeSparseTerms(dtm,0.999)

data_new=as.data.frame(as.matrix(dtm))
data_new$name_of_developer = dataset_1$Assignee.Real.Name

m<- table(data_new$name_of_developer)
n <- as.data.frame(m,row.names = NULL, 
                   responseName = "NO_of_bugs_Assigned", 
                   stringsAsFactors = TRUE,
                   sep = "", base = list(LETTERS))

#Splitting dataset into training set and testing set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data_new$name_of_developer, SplitRatio = 0.75)
training_set = subset(data_new, split == TRUE)
test_set = subset(data_new, split == FALSE)

#z=total number of columns after creating bag of word model and adding developer column

#Using Random Forest Algorithm
install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-z],
                          y = training_set$name_of_developer,
                          ntree = 500,keep.forest = TRUE)

#Using SVM-linear
install.packages('e1071')
library(e1071)
set.seed(123)
classifier = svm(formula= name_of_developer~.,
                 data=training_set,
                 type= 'C-classification',
                 kernel= 'linear')

#Using SVM-Polynomial
install.packages('e1071')
library(e1071)
set.seed(123)
classifier = svm(formula= name_of_developer~.,
                 data=training_set,
                 type= 'C-classification',
                 kernel= 'polynomial')

#Using SVM-Radial
install.packages('e1071')
library(e1071)
set.seed(123)
classifier = svm(formula= name_of_developer~.,
                 data=training_set,
                 type= 'C-classification',
                 kernel= 'radial')

#Using SVM-Sigmoid
install.packages('e1071')
library(e1071)
set.seed(123)
classifier = svm(formula= name_of_developer~.,
                 data=training_set,
                 type= 'C-classification',
                 kernel= 'sigmoid')

#Using J48 Algorithm
install.packages('RWeka')
library('RWeka')
set.seed(123)
classifier = J48(formula= name_of_developer~.,
                 data=training_set)

#Determine performance
y_pred = predict(classifier, newdata = test_set[-z])
cm = table(test_set[, z], y_pred)
accuracy <- sum(diag(cm))/sum(cm)
precision <- (diag(cm) / rowSums(cm))
recall <- (diag(cm) / colSums(cm))
fmeasure <- 2 * precision * recall / (precision + recall)