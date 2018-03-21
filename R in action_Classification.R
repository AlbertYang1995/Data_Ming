#------------------------download the packages----------------------
pkgs <- c('rpart', 'rpart.plot', 'party')
install.packages(pkgs, depend = T)
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc, ds, sep = "")
breast <- read.table(url, sep = ",", header = F, na.strings = "?")
str(breast)
#-----------------------preprocess the data-------------------------
names(breast) <- c("ID", "clumpThickness", "sizeUniformity",
                   "shapeUniformity", "maginalAdhesion",
                   "singleEpithelialCellSize", "bareNuclei",
                   "blandChromatin", "normalNucleoli", "mitosis", "class")
str(breast)
df <- breast[-1]
df$class <- factor(df$class, levels = c(2, 4),
                   labels = c("benign", "malignant"))
table(df$class)
set.seed(1234)
train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]
table(df.train$class)
table(df.validate$class)

#----------------------logistic regression-------------------------
fit.logit <- glm(class ~ ., data = df.train, family = binomial())
summary(fit.logit)
prop <- predict(fit.logit, df.validate, type = "response")
logit.pred <- factor(prop > .5, levels = c(FALSE, TRUE),
                     labels = c("benign", "malignant"))
logit.perf <- table(df.validate$class, logit.pred,
                    dnn = c("Actual", "Predicted"))
logit.perf

#--------------------Tree structure---------------------------
library(rpart)
set.seed(1234)
dtree <- rpart(class ~ ., data = df.train, method = "class",
               parms = list(split = "information"))
dtree$cptable
plotcp(dtree)
dtree.pruned <- prune(dtree, cp = .0125)
library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104,
    fallen.leaves = T, main = "Decision Tree")
dtree.pred <- predict(dtree.pruned, df.validate, type = "class")
dtree.perf <- table(df.validate$class, dtree.pred,
                    dnn = c("Actual", "Predicted"))
dtree.perf

#----------------IF ELSE TREE------------------------------
library(party)
fit.ctree <- ctree(class ~ ., data = df.train)
plot(fit.ctree, main = "Conditional Inference Tree")
ctree.pred <- predict(fit.ctree, df.validate, type = "response")
ctree.perf <- table(df.validate$class, ctree.pred,
                    dnn = c("Actual", "Predicted"))
ctree.perf

#---------------------random forest--------------------------
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(class ~ ., data = df.train,
                           na.action = na.roughfix,
                           importance = T)
fit.forest
importance(fit.forest, type = 2)
forest.pred <- predict(fit.forest, df.validate)
forest.perf <- table(df.validate$class, forest.pred,
                     dnn = c("Actual", "Predicted"))
forest.perf

#--------------------------SVM-----------------------------
library(e1071)
set.seed(1234)
fit.svm <- svm(class ~ ., data = df.train)
fit.svm
svm.pred <- predict(fit.svm, na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$class,
                  svm.pred, dnn = c("Actual", "Predicted"))
svm.perf
set.seed(1234)
tuned <- tune.svm(class ~ ., data = df.train,
                  gamma = 10^(-6:1),
                  cost = 10^(-10:10))
tuned
fit.svm <- svm(class ~ ., data = df.train, gamma = .01, cost = 1)
svm.pred <- predict(fit.svm, na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$class,
                  svm.pred, dnn = c("Actual", "Predicted"))
svm.perf







