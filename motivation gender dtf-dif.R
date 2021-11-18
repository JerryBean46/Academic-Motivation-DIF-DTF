## Set Your Directory
setwd("I:/Articles/Motivation - DIF-DTF Analysis (GitHub)")

library("psych")
library("lessR")
library('mirt')


## Run this helper function
get.dif.items <- function(f.data,p.val=.05,parms){
  r.warnings = ""
  keep.vars <- c("X2", "df", "p") # just keep these variables
  f.data <- f.data[keep.vars]
  f.data$p = round(f.data$p,3)
  if(missing(f.data)) return('Missing model output out.list')
  f.data$sig <- ifelse(f.data$p < p.val,'dif','no_dif')
  if(!missing(parms)){
    if(nrow(f.data) == nrow(parms)){
      f.data <- cbind(f.data,parms)
    }else{
      r.warnings = "There number of item parameters doesn't match the number of items "
      r.warnings = paste(r.warnings,"given to get.dif.items. Item parameters omitted.")
    }
  }
  dif.items <- subset(f.data, sig == 'dif')
  no.dif.items <- subset(f.data, sig == 'no_dif')
  if(!missing(parms) && nrow(f.data) == nrow(parms)){
    if(nrow(no.dif.items)>1){
      no.dif.items <- no.dif.items[order(-no.dif.items$a1),]
    }
  }
  r.list <- list(dif_items = dif.items, no_dif = no.dif.items, warnings = r.warnings)
  return(r.list)
}

## Load data sets
datam <- read.csv("males.csv", header = TRUE, sep=",")
males <-(datam[,1:6])
dataf <- read.csv("females.csv", header = TRUE, sep=",")
females <-(dataf[,1:6])
MaleN <- 1570
FemaleN = 1651
group <- c(rep('Ref', FemaleN), rep('Foc', MaleN))
dat <- rbind(females, males)

############# Step 1: Testing assumptions ############

## Graded Response Model Fit
ref.model <- mirt(females, model = 1, itemtype = "graded", SE=TRUE, verbose = FALSE)
M2(ref.model, type = "C2")
foc.model <- mirt(males, model = 1, itemtype = "graded", SE=TRUE, verbose = FALSE)
M2(foc.model, type = "C2")

## Item Fit
(females.fit <- itemfit(ref.model))
(males.fit <- itemfit(foc.model))

############# Step 2: Get Item Parameters ############

######### Freely estimated model ####################

model.free <- multipleGroup(dat, 1, group, verbose = FALSE)
coef(model.free, IRTpars = TRUE, printSE = TRUE, simplify = TRUE)  # for the manuscript

############# Step 3: Baseline Model ##################

model.constrained <- multipleGroup(dat, 1, group, invariance = c(colnames(dat), 'free_means', 'free_var'))
(constrained.parameters <- coef(model.constrained, simplify = TRUE)[[1]][[1]])  ##constrained to one table

############# Step 4: First round of DIF analyses (LRTs) - All Others As Anchors #################

(dif.drop <- DIF(model.constrained, c('a1','d1','d2','d3','d4'), scheme = 'drop', seq_stat = .05))

## use the optional function to table the output
get.dif.items(f.data=dif.drop,p.val=.05,parms=constrained.parameters)

############# Step 5: Specifiy a New Baseline Model Using Anchor Items (MaxA5 Approach)  #################

itemnames <- colnames(dat)
anc.items.names <- itemnames[c(1,2,4,6)] #Top 5 a-parms from Step 4 as anchors
test.items <- c(3,5) #DIF items from Step 4

model_anchor <- multipleGroup(dat, model = 1, group = group,
                              invariance = c(anc.items.names, 'free_means', 'free_var'))

(anchor.parms <-coef(model_anchor,simplify = TRUE)[[1]][[1]])

############# Step 6: Run Final Invariance Tests  #################

(dif.anchor <- DIF(model_anchor, c('a1','d1','d2','d3','d4'), items2test = test.items))

## use the optional function to table the output
get.dif.items(f.data=dif.anchor,p.val=.05)

############# Step 7: Compute Effect Sizes  #################

empirical_ES(model_anchor, plot=FALSE, as.table = TRUE) # expected item score plots
empirical_ES(model_anchor, DIF=FALSE, plot=FALSE) # expected test score plot

## Item Plots
ES.item <- empirical_ES(model_anchor, plot=TRUE, as.table = TRUE)  
ES.item$main <- ""
ES.item$legend$top$args$key$text[[1]] <- c('Focal - Males', 'Reference - Females')
ES.item$legend$top$args$key$points$col[1:2] <- c('black', 'gray') 
ES.item$par.settings$strip.background$col[[1]] <-"#E0E0E0" 
ES.item$panel.args.common$col[1:2] <- c('black', 'gray')
ES.item$ylab <- "Expected Item Score"
ES.item$xlab <- expression(paste("Focal Group  ", theta))
ES.item

## Scale Plot
ES.test <- empirical_ES(model_anchor, DIF=FALSE, plot=TRUE) 
ES.test$main <- "" 
ES.test$legend$top$args$key$text[[1]] <- c('Focal - Males', 'Reference - Females') 
ES.test$legend$top$args$key$points$col[1:2] <- c('black', 'gray') 
ES.test$panel.args.common$col[1:2] <- c('black', 'gray') 
ES.test$xlab <- expression(paste("Focal Group  ", theta))
ES.test 





