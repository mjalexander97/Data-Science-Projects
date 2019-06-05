knitr::opts_chunk$set(echo = TRUE, cache = TRUE)

indent1 = '    '
indent2 = paste(rep(indent1, 2), collapse='')
indent3 = paste(rep(indent1, 3), collapse='')

doeval = FALSE

library(knitr)
library(tidyverse)
library(ggmap)
library(maps)
library(Rtsne)
library(NbClust)
library(tree)
library(maptree)
library(class)
library(reshape2)

#Read in all of the necessary data sets
election.raw = read.csv("~/final-project/data/election/election.csv") %>% as.tbl 
census_meta = read.csv("~/final-project/data/census/metadata.csv", sep = ";") %>% as.tbl 
census = read.csv("~/final-project/data/census/census.csv") %>% as.tbl 
census$CensusTract = as.factor(census$CensusTract)


# filter the election data into separate variables that satisfy certain conditions
election_federal <- filter(election.raw, state == "US")
election_state_no <- filter(election.raw[33:18351,], is.na(county))
election_no <- filter(election.raw, county != 'NA')
election_state <- election_state_no[1:286,] %>% rbind(election_state_no[293:308,])
election <- election_no %>% rbind(election_state_no[287:292,]) %>% rbind(election_state_no[309:312,])

# create a barplot of each candidate and their respective number of votes 
barplot(names.arg = election_federal$candidate, height = election_federal$votes, las = 2,
        main = "Barplot of Votes for each Presidential Candidate")

# shows the winning candidate of each county and state respectively
county_winner <- election  %>% group_by(fips) %>% mutate(pct = votes/sum(votes)) %>% top_n(n = 1)

state_winner <- election_state %>% group_by(fips) %>% mutate(pct = votes/sum(votes)) %>% top_n(n = 1)

# creates a map of the entire country with each of the states divided into its counties 
counties = map_data("county")

ggplot(data = counties) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) 

# creates a map of the country that shows the winning candidate of that state for the 2016 election
states = map_data("state")
states <- states %>% mutate(fips = state.abb[match(region, tolower(state.name))])
statesplot <- left_join(states,state_winner)
ggplot(data = statesplot) + 
  geom_polygon(aes(x = long, y = lat, fill = candidate, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  ggtitle("Map of Winning Candidate for each State")


# shows the winning candidate of each county 
region <- c()
for (i in 1:3085){
  region = c(region,strsplit(maps::county.fips$polyname, ',')[[i]][1])
}
subregion <- c()
for (i in 1:3085){
  subregion = c(subregion,strsplit(maps::county.fips$polyname, ',')[[i]][2])
}
countyregion <- maps::county.fips
countyregion <- countyregion %>% cbind(region) %>% cbind(subregion)
countyleft <- left_join(countyregion,counties)
countywinnerstrings <- county_winner
countyleftstrings <- countyleft
countywinnerstrings$fips <- as.character(countywinnerstrings$fips)
countyleftstrings$fips <- as.character(countyleftstrings$fips)
countyleft2 <- left_join(countywinnerstrings,countyleftstrings)

ggplot(data = countyleft2) + 
  geom_polygon(aes(x = long, y = lat, fill = candidate, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  ggtitle("Map of Winning Candidate for each County")

census.del <- na.omit(census)
census.del$Men <- census.del$Men/sum(census.del$Men)
census.del$Citizen <- census.del$Citizen/sum(census.del$Citizen)
census.del$Employed <- census.del$Employed/sum(census.del$Employed)
census.del <- mutate(census.del, Minority = Hispanic + Black + Native + Asian + Pacific)
census.del <- select(census.del, -c(Walk, PublicWork, Construction))

census.subct <- census.del %>% group_by(State,County) %>% add_tally() %>% mutate(weight = TotalPop/n)

census.ct <- census.subct %>% summarize_at(vars(TotalPop:weight), sum)

head(census.ct)

tmpwinner = county_winner %>% ungroup %>%
  mutate(state = state.name[match(state, state.abb)]) %>%               ## state abbreviations
  mutate_at(vars(state, county), tolower) %>%                           ## to all lowercase
  mutate(county = gsub(" county| columbia| city| parish", "", county))  ## remove suffixes
tmpcensus = census.ct %>% ungroup %>% mutate_at(vars(State, County), tolower)

election.cl = tmpwinner %>%
  left_join(tmpcensus, by = c("state"="State", "county"="County")) %>% 
  na.omit

## saves meta information to attributes
attr(election.cl, "location") = election.cl %>% select(c(county, fips, state, votes, pct))
election.cl = election.cl %>% select(-c(county, fips, state, votes, pct))


election.cl.jobs <- election.cl %>% 
  mutate(prof.prop = Professional/(Professional+Service+Office+Production)) %>% 
  mutate(serv.prop = Service/(Professional+Service+Office+Production)) %>% 
  mutate(office.prop = Office/(Professional+Service+Office+Production)) %>% 
  mutate(prod.prop = Production/(Professional+Service+Office+Production))

# boxplots showing the distribution of votes for each candidate based on type of job (professional, service, office, and production)
p <- (ggplot(election.cl.jobs,aes(candidate,prof.prop,color=candidate)) + geom_boxplot() + 
        ggtitle("Boxplot of Winner vs. Percentage of Professional Jobs"))
s <- (ggplot(election.cl.jobs,aes(candidate,serv.prop,color=candidate)) + geom_boxplot() + 
        ggtitle("Boxplot of Winner vs. Percentage of Service Jobs"))
o <- (ggplot(election.cl.jobs,aes(candidate,office.prop,color=candidate)) + geom_boxplot() + 
        ggtitle("Boxplot of Winner vs. Percentage of Office Jobs"))
pro <- (ggplot(election.cl.jobs,aes(candidate,prod.prop,color=candidate)) + geom_boxplot() + 
          ggtitle("Boxplot of Winner vs. Percentage of Production Jobs"))

# shows the output from the previously created variables
p
s
o
pro


# use of PCA for dimensionality reduction
censuscounty.pca <- prcomp(census.ct[,3:36], scale = T)
censussubcounty.pca <- prcomp(census.subct[,4:37], scale = T)
ct.pc <- data.frame(censuscounty.pca$x)
subct.pc <- data.frame(censussubcounty.pca$x)

#comparing plots of the principal components

scaled_census <- scale(census.ct[,-c(1,2)],center = T,scale = T)
dist.ct <- dist(scaled_census)
hc.ct <- hclust(dist.ct, method = "complete")

ct.pc2 <- data.frame(censuscounty.pca$x[,1:5])
cc <- palette()
palette(c(cc,"purple","brown"))
plot(ct.pc2[,1:2], col = cutree(hc.ct, k = 10))
points(ct.pc[227,1],ct.pc[227,2], col = "orange")

plot(ct.pc2[,2:3], col = cutree(hc.ct, k = 10))
points(ct.pc2[227,2],ct.pc2[227,3], col = "orange")

plot(ct.pc2[,3:4], col = cutree(hc.ct, k = 10))
points(ct.pc2[227,3],ct.pc2[227,4], col = "orange")


dist.pc <- dist(ct.pc2)
hc.pc <- hclust(dist.pc, method = "complete")
plot(ct.pc2, col = cutree(hc.pc, k=10))


set.seed(10) 
n = nrow(election.cl)
in.trn= sample.int(n, 0.8*n) 
trn.cl = election.cl[ in.trn,] # training set
tst.cl = election.cl[-in.trn,] # testing set 

set.seed(20) 
nfold = 10 # assign the number of folds
folds = sample(cut(1:nrow(trn.cl), breaks=nfold, labels=FALSE))

calc_error_rate = function(predicted.value, true.value){
  return(mean(true.value!=predicted.value))
} # calculate the error rate of our classification
records1 = matrix(NA, nrow=3, ncol=2)
colnames(records1) = c("train.error","test.error")
rownames(records1) = c("tree","knn","logistic regression")


# need to figure out the best size of the cv 
set.seed(20)
electiontree <- tree(candidate ~ ., data = trn.cl, control = tree.control(nobs = nrow(trn.cl)))
cv <- cv.tree(electiontree, FUN = prune.misclass, K = nfold, rand = folds)
cv

best.size.cv <- 10
draw.tree(electiontree, cex=0.6, nodeinfo=TRUE)
electiontree.pruned <- prune.tree(electiontree, best = best.size.cv)
draw.tree(electiontree.pruned, cex = 0.6, nodeinfo = TRUE)

set.seed(20)
electiontree.pruned.predvl <- predict(electiontree.pruned, tst.cl, type = 'class')
electiontree.pruned.predtr <- predict(electiontree.pruned, trn.cl, type = 'class')
records1[1,1] = calc_error_rate(electiontree.pruned.predtr, trn.cl$candidate) 
records1[1,2] = calc_error_rate(electiontree.pruned.predvl, tst.cl$candidate)
records1

kvec = 29:32 # we evaluated 1:50 but we reduced it to this to speed up knitting time
do.chunk <- function(chunkid, folddef, Xdat, Ydat, k){
  train = (folddef!=chunkid)
  Xtr = Xdat[train,]
  Ytr = Ydat[train]
  Xvl = Xdat[!train,]
  Yvl = Ydat[!train]
  ## get classifications for current training chunks
  predYtr = knn(train = Xtr, test = Xtr, cl = Ytr, k = k)
  ## get classifications for current test chunk
  predYvl = knn(train = Xtr, test = Xvl, cl = Ytr, k = k)
  data.frame(train.error = calc_error_rate(predYtr, Ytr),
             val.error = calc_error_rate(predYvl, Yvl))
}

error.folds = NULL
for (j in kvec){
  tmp = lapply(1:9, do.chunk, folds, election.cl %>% select(-candidate), election.cl$candidate, j)
  tmp$neighbors = j
  error.folds = rbind(error.folds, tmp)
}


best.kfold <- 31
set.seed(20)
Ytrain <- trn.cl$candidate
Xtrain <- trn.cl %>% select(-candidate)
Xtest <- tst.cl %>% select(-candidate)
Ytest <- tst.cl$candidate

pred.YTrain = knn(train=Xtrain, test=Xtrain, cl=Ytrain, k=31)
pred.YTest = knn(train=Xtrain, test=Xtest, cl=Ytrain, k=31)
records1[2,1] = calc_error_rate(pred.YTrain, trn.cl$candidate)
records1[2,2] = calc_error_rate(pred.YTest, tst.cl$candidate)
records1

pca.records = matrix(NA, nrow=2, ncol=2)
colnames(pca.records) = c("train.error","test.error")
rownames(pca.records) = c("tree","knn")

trn.cl.indp <- trn.cl %>% select(-candidate)
trn.cl.indp.pc <- prcomp(trn.cl.indp, scale = T)

pr.var = trn.cl.indp.pc$sdev ^2
pve = pr.var/sum(pr.var)
plot(pve, xlab="Principal Component",ylab="Proportion of Variance Explained ", ylim=c(0,0.3),type='b')

tr.pca <- data.frame(trn.cl.indp.pc$x[,1:4])
tr.pca <- cbind(trn.cl$candidate, tr.pca)

tst.cl.indp <- tst.cl %>% select(-candidate)
tst.cl.indp.pc <- prcomp(tst.cl.indp, scale = T)

test.pca <- data.frame(tst.cl.indp.pc$x[,1:4])
test.pca <- cbind(tst.cl$candidate, test.pca)
colnames(tr.pca)[1] <- "candidate"
colnames(test.pca)[1] <- "candidate"

set.seed(20)
pcatree <- tree(candidate ~ ., data = tr.pca, control = tree.control(nobs = nrow(tr.pca)))
cvtree <- cv.tree(pcatree, FUN = prune.misclass, K = nfold, rand = folds)
cvtree #best size is 2

best.size.cvtree <- 2
draw.tree(pcatree, cex=0.6, nodeinfo=TRUE)
pcatree.pruned <- prune.tree(pcatree, best = best.size.cvtree)
draw.tree(pcatree.pruned, cex = 0.6, nodeinfo = TRUE)

set.seed(20)
pcatree.pruned.predvl <- predict(pcatree.pruned, test.pca, type = 'class')
pcatree.pruned.predtr <- predict(pcatree.pruned, tr.pca, type = 'class')
pca.records[1,1] = calc_error_rate(pcatree.pruned.predtr, tr.pca$candidate) 
pca.records[1,2] = calc_error_rate(pcatree.pruned.predvl, test.pca$candidate)
pca.records

kvec1 <- 17:19 # we evaluated 1:50 but we reduced it to this to speed up knitting time
election.cl.indp <- election.cl %>% select(-candidate)
election.cl.indp.pc <- prcomp(election.cl.indp, scale = T)

election.pca <- data.frame(election.cl.indp.pc$x[,1:4])
election.pca <- cbind(election.cl$candidate, election.pca)
colnames(election.pca)[1] <- "candidate"

error.folds = NULL
for (j in kvec1){
  tmp = lapply(1:9, do.chunk, folds, election.pca %>% select(-candidate), election.pca$candidate, j)
  tmp$neighbors = j
  error.folds = rbind(error.folds, tmp)
}


best.kfold <- 18
set.seed(20)
Ytrainpca <- tr.pca$candidate
Xtrainpca <- tr.pca %>% select(-candidate)
Xtestpca <- test.pca %>% select(-candidate)
Ytestpca <- test.pca$candidate

pred.YTrainpca = knn(train=Xtrainpca, test=Xtrainpca, cl=Ytrainpca, k=18)
pred.YTestpca = knn(train=Xtrainpca, test=Xtestpca, cl=Ytrainpca, k=18)
pca.records[2,1] = calc_error_rate(pred.YTrainpca, tr.pca$candidate)
pca.records[2,2] = calc_error_rate(pred.YTestpca, test.pca$candidate)
pca.records

set.seed(20)
trn.cl.fact <- as.factor(trn.cl$candidate)
trn.cl.fact <- cbind(trn.cl.fact,trn.cl[,2:35])
colnames(trn.cl.fact)[1] <- "candidate"


glm.fit <- glm(candidate ~ .,data = trn.cl, family = binomial)
probtraining <- round(predict(glm.fit,type='response'))

prob.cl.training3 = trn.cl %>%
  mutate(predy = as.factor(ifelse(probtraining<=0.5, "Donald Trump", "Hillary Clinton")))

prob.cl.training3$candidate <- factor(prob.cl.training3$candidate)

pred_prob_training <- calc_error_rate(prob.cl.training3$predy,prob.cl.training3$candidate)
pred_prob_training

probtest <-round(predict(glm.fit,tst.cl,type='response'))
prob.cl.test = tst.cl %>%
  mutate(predy = as.factor(ifelse(probtest<=0.5, "Donald Trump", "Hillary Clinton")))

prob.cl.test$candidate <- factor(prob.cl.test$candidate)

pred_prob_test <- calc_error_rate(prob.cl.test$predy,prob.cl.test$candidate)
pred_prob_test

records1[3,1] <- pred_prob_training
records1[3,2] <- pred_prob_test
records1