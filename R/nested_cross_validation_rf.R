set.seed(12345)
library(caret)
library(DMwR)
library(ROSE)
library(pROC)
library(parallel)
library(mltools)

source("./R/supplementary_functions/supplementary_functions.R")

#######################################################################################################################################
##########################################             Global variables                   #############################################
#######################################################################################################################################

## The arguments can be passed by the command line ##
argscomd = commandArgs(trailingOnly=TRUE)

list_cv_path = as.character(argscomd[1]) # 
ent = as.character(argscomd[2]) 
KfoldsInnerLoop = as.numeric(as.character(argscomd[3])) 
outdir = as.character(argscomd[4]) 
index = as.numeric(as.character(argscomd[5])) 

### other global variables 
ncores <- 10  # multi-threads option
sizes_splits="Two_thirds" ## rfe splits

#######################################################################################################################################
##########################################             Load the data                      #############################################
#######################################################################################################################################

load(list_cv_path) ## An R object that contains a list of cross-validations partitions

in.table <- CV_List_data_frame[[index]]$Train
in.test  <- CV_List_data_frame[[index]]$Test

in.table$conditions <- factor(as.character(in.table$conditions),levels=c(ent,"other"))
in.test$conditions <- factor(as.character(in.test$conditions),levels=c(ent,"other"))

#######################################################################################################################################
##########################################                  Script                        #############################################
#######################################################################################################################################

cl <- parallel::makeForkCluster(ncores)
doParallel::registerDoParallel(cl)

################################################
#### Create the inner-cross validation #########
################################################
Fold_ind_train <- createFolds(in.table$conditions, k = KfoldsInnerLoop, list = TRUE, returnTrain = T)

list_feature_selected <- vector(mode = "list", length = KfoldsInnerLoop)
names(list_feature_selected) <- names(Fold_ind_train)
statistics_models <- data.frame()

################################################
#### Do the K-fold inner-cross validation ######
################################################

for(incv in names(Fold_ind_train)){

	incv.train <- in.table[Fold_ind_train[[incv]] , ]
	incv.test <- in.table[-Fold_ind_train[[incv]] , ]

	###################################
	####  Create the datasets #########
	###################################
	list_df <- Create_datasetes(in_table = incv.train )
	up_train <- list_df[["up_train"]]
	down_train <- list_df[["down_train"]]
	smote_train  <- list_df[["smote_train"]]
	rose_train <- list_df[["rose_train"]]
	
	###################################
	########  Run the RFE    ##########
	###################################
	list_rfe <- rfe_wrapper(sizes_splits = "Two_thirds", num_variables = 87 , list_data_frame = list_df )
	list_feature_selected[[incv]] <- list_rfe

	###############################################
	########     Subset the datasets     ##########
	###############################################

	#### up #####
	incv.train.up <- up_train[ , match(  c(  "conditions",list_rfe$up.predictors )  , colnames(up_train) ) ] 
	incv.test.up <- incv.test[ , match(  c(  "conditions",list_rfe$up.predictors )  , colnames(incv.test) ) ] 

	#### down #####
	incv.train.down <- down_train[ , match(  c(  "conditions",list_rfe$down.predictors )  , colnames(down_train) ) ] 
	incv.test.down <- incv.test[ , match(  c(  "conditions",list_rfe$down.predictors )  , colnames(incv.test) ) ] 

	#### smote #####
	incv.train.smote <- smote_train[ , match(  c(  "conditions",list_rfe$smote.predictors )  , colnames(smote_train) ) ] 
	incv.test.smote <- incv.test[ , match(  c(  "conditions",list_rfe$smote.predictors )  , colnames(incv.test) ) ] 

	#### rose #####
	incv.train.rose <- rose_train[ , match( c(  "conditions",list_rfe$rose.predictors )  , colnames(rose_train) ) ] 
	incv.test.rose <- incv.test[ , match(  c(  "conditions",list_rfe$rose.predictors )  , colnames(incv.test) ) ] 


	###############################################
	########  Train the ML classifier    ##########
	###############################################
	ctrl <- trainControl(method = "repeatedcv", number = 10 , repeats = 10 , classProbs = T, summaryFunction = twoClassSummary)

	up_classifier <- train(conditions ~ ., data = incv.train.up, method = "rf",trControl = ctrl, maximize = T, ntree = 1000)
	down_classifier <- train(conditions ~ ., data = incv.train.down, method = "rf",trControl = ctrl, maximize = T, ntree = 1000)
	smote_classifier <- train(conditions ~ ., data = incv.train.smote, method = "rf",trControl = ctrl, maximize = T, ntree = 1000)
	rose_classifier <- train(conditions ~ ., data = incv.train.rose, method = "rf",trControl = ctrl, maximize = T, ntree = 1000)
	incv_classifier <- train(conditions ~ ., data = incv.train, method = "rf",trControl = ctrl, maximize = T, ntree = 1000)

	###############################################
	########  Test the ML classifier    ##########
	###############################################

	up.stats <- best_model_stats(test_df=incv.test.up, classifier_model=up_classifier, ent= ent)
	down.stats <- best_model_stats(test_df=incv.test.down, classifier_model=down_classifier, ent= ent)
	rose.stats <- best_model_stats(test_df=incv.test.rose, classifier_model=rose_classifier, ent= ent)
	smote.stats <- best_model_stats(test_df=incv.test.smote, classifier_model=smote_classifier, ent= ent)
	incv.stats <- best_model_stats(test_df=incv.test, classifier_model=incv_classifier, ent= ent)

	###############################################
	########     Join the datasets     ############
	###############################################
	res_df <- rbind(up.stats,down.stats,rose.stats,smote.stats,incv.stats)
	res_df <- data.frame( Method = c("up","down","rose","smote","default"), Fold = incv , res_df)
	statistics_models <- rbind(statistics_models,res_df)



}

########################################################################################################################################################
############################################                  Select the best model             ########################################################
########################################################################################################################################################

statistics_models <- statistics_models[order(statistics_models$MCC,decreasing=T),]
statistics_models <- statistics_models[complete.cases(statistics_models) , ]

MCC_top <- statistics_models$MCC > quantile(statistics_models$MCC,na.rm=T)[4]
AUC_top <- statistics_models$AUC > quantile(statistics_models$AUC,na.rm=T)[4]
F_Measurement_top <- statistics_models$F_Measurement > quantile(statistics_models$F_Measurement,na.rm=T)[4]
Kappa_top <- statistics_models$Kappa > quantile(statistics_models$Kappa,na.rm=T)[4]
top_models <- apply( cbind(MCC_top,AUC_top,F_Measurement_top,Kappa_top) , 1, all )
statistics_models_best <- statistics_models[top_models,]

if(dim(statistics_models_best)[1] == 0){ ### In case no single classifier satisfies the criteria, reduce the cut-off to the 50% quantile(statistics_models$MCC,na.rm=T)[3]

	MCC_top <- statistics_models$MCC > quantile(statistics_models$MCC,na.rm=T)[3]
	AUC_top <- statistics_models$AUC > quantile(statistics_models$AUC,na.rm=T)[3]
	F_Measurement_top <- statistics_models$F_Measurement > quantile(statistics_models$F_Measurement,na.rm=T)[3]
	Kappa_top <- statistics_models$Kappa > quantile(statistics_models$Kappa,na.rm=T)[3]
	top_models <- apply( cbind(MCC_top,AUC_top,F_Measurement_top,Kappa_top) , 1, all )
	statistics_models_best <- statistics_models[top_models,]

}


if(dim(statistics_models_best)[1] == 0){ ### In case no single classifier satisfies the criteria, reduce the cut-off to the 25%  quantile(statistics_models$MCC,na.rm=T)[2]

	MCC_top <- statistics_models$MCC > quantile(statistics_models$MCC,na.rm=T)[2]
	AUC_top <- statistics_models$AUC > quantile(statistics_models$AUC,na.rm=T)[2]
	F_Measurement_top <- statistics_models$F_Measurement > quantile(statistics_models$F_Measurement,na.rm=T)[2]
	Kappa_top <- statistics_models$Kappa > quantile(statistics_models$Kappa,na.rm=T)[2]
	top_models <- apply( cbind(MCC_top,AUC_top,F_Measurement_top,Kappa_top) , 1, all )
	statistics_models_best <- statistics_models[top_models,]

}

if(dim(statistics_models_best)[1] == 0){ ### In case no single classifier satisfies the criteria, reduce the cut-off to the 0%  quantile(statistics_models$MCC,na.rm=T)[1]

	MCC_top <- statistics_models$MCC > quantile(statistics_models$MCC,na.rm=T)[1]
	AUC_top <- statistics_models$AUC > quantile(statistics_models$AUC,na.rm=T)[1]
	F_Measurement_top <- statistics_models$F_Measurement > quantile(statistics_models$F_Measurement,na.rm=T)[1]
	Kappa_top <- statistics_models$Kappa > quantile(statistics_models$Kappa,na.rm=T)[1]
	top_models <- apply( cbind(MCC_top,AUC_top,F_Measurement_top,Kappa_top) , 1, all )
	statistics_models_best <- statistics_models[top_models,]

}
Method <- as.character(statistics_models_best[1,]$Method)
Fold <- as.character(statistics_models_best[1,]$Fold)
mtry <- statistics_models_best[1,]$model.train.mtry

if(Method == "default"){
	Features_selected = colnames(in.table)
	Features_selected <- Features_selected[!grepl("conditions",Features_selected)]
}else{
	Features_selected <-list_feature_selected[[Fold]][[paste0(Method,".predictors")]]
}

########################################################################################################################################################
############################################                  Perform the outer K-fold CV             ##################################################
########################################################################################################################################################

indices <- c( 1:length(CV_List_data_frame) )
indices <- indices[!indices %in% index ]

#######################################
### Create the rminer output list #####
 time <- c()
 test <- list()
 pred <- list()
 error <- c()
 mpar <- list()
 model <- "randomForest"
 task <- "prob"
 method <- list(m ="kfold" , p = length(indices))
 sen <- matrix(NA,length(indices),length(Features_selected))
 sresponses <- NULL
 runs <- length(index) ### modify 
 attributes <- NULL
 feature <- "sens"

 mtry <- mtry
 colnames(sen) <- Features_selected

#######################################

sum <-1
stats_outer_loop_cv <- data.frame()
Random_seed <- .Random.seed
for(i in indices){


	#############################################
	####    Load the data    ####################
	#############################################

	intable <- CV_List_data_frame[[i]]$Train
	intest  <- CV_List_data_frame[[i]]$Test
	intable$conditions <- factor(as.character(intable$conditions),levels=c(ent,"other"))
	intest$conditions <- factor(as.character(intest$conditions),levels=c(ent,"other"))

	############################################
	#### Subset the features selected  #########
	############################################

	incv.train <- intable[ , match(  c(  "conditions",Features_selected )  , colnames(intable) ) ] 
	incv.test <-  intest[ , match(   c(  "conditions",Features_selected )  , colnames(intest) ) ] 


	####################################################################
	#### Balance according to the method that maximizes the MCC ########
	####################################################################


	if(Method == "default"){
		balance_train <- incv.train
	}else{
		list_df <- Create_datasetes(in_table = incv.train )
		balance_train <- list_df[[paste0(Method,"_train")]]
	}



	####################################
	#### Train the classifier  #########
	####################################

	ctrl <- trainControl(method = "none",  classProbs = T, summaryFunction = twoClassSummary)

	incv_classifier <- train(conditions ~ ., data = balance_train, method = "rf",trControl = ctrl, tuneGrid = data.frame(mtry = mtry), ntree = 1000)
	incv.stats <- outer_loop_cv_stats(test_df=intest, classifier_model=incv_classifier, ent= ent)

	ret <- t(data.frame(incv.stats))
	stats_outer_loop_cv <- rbind(stats_outer_loop_cv,ret)	

	### Importance ###
	Imp <- varImp(incv_classifier,scale = FALSE)
	Importance <- Imp$importance$Overall
	names(Importance) <- rownames(Imp$importance)
	Importance <- Importance[match(Features_selected, names(Importance))]

	### Prediction ###
	incv.test$conditions <- factor(as.character(incv.test$conditions),levels=c(ent,"other"))
	P <- predict(incv_classifier, newdata = incv.test, type = "prob" )


	sen[sum,] <- Importance
	time[sum] <- i
	test[[sum]] <- incv.test$conditions
	pred[[sum]] <- P
	error[sum] <- incv.stats[2]
	mpar[[sum]] <- list(mtry = mtry)
	sum <- sum + 1


}

rminer_list <- list(time,test,pred,error,mpar,model,task,method,sen,sresponses,runs,attributes,feature)
names(rminer_list) <- c("time","test","pred","error","mpar","model","task","method","sen","sresponses","runs","attributes","feature")

Best_model_summary <- list(
	Analisis = outdir, 
	Enterotype = ent, 
	parametrs = c(mtry=mtry), 
	Features_selected=Features_selected, 
	balance_method = Method, 
	Fold = Fold,
	mean_auc = mean(stats_outer_loop_cv$AUC,na.rm=T) , 
	mean_MCC = mean(stats_outer_loop_cv$MCC,na.rm=T),
	list_cv_path = list_cv_path,	
	Ind_CV <- index
)

print(Best_model_summary)


########################################################################################################################################################
###################################       Best fit parameters/features into training test       ########################################################
########################################################################################################################################################


ctrl <- trainControl(method = "none",  classProbs = T, summaryFunction = twoClassSummary)

############################################
#### Subset the features selected  #########
############################################

incv.train <- in.table[ , match(  c(  "conditions",Features_selected )  , colnames(in.table) ) ] 
incv.test <-  in.test[ , match(   c(  "conditions",Features_selected )  , colnames(in.test) ) ] 


####################################################################
#### Balance according to the method that maximizes the MCC ########
####################################################################

if(Method == "default"){
	balance_train <- incv.train
}else{
	list_df <- Create_datasetes(in_table = incv.train )
	balance_train <- list_df[[paste0(Method,"_train")]]
}

####################################
#### Train the classifier  #########
####################################

incv_classifier <- train(conditions ~ ., data = balance_train, method = "rf",trControl = ctrl, tuneGrid = data.frame(mtry = mtry), ntree = 1000)

####################################
####        Performance    #########
####################################
incv.stats_OLOOP <- outer_loop_cv_stats(test_df=incv.test, classifier_model=incv_classifier, ent= ent)
ret_stats_OLOOP <- t(data.frame(incv.stats_OLOOP))

### Importance ###
Imp <- varImp(incv_classifier,scale = FALSE)
Importance_OLOOP <- Imp$importance$Overall
names(Importance_OLOOP) <- rownames(Imp$importance)
Importance_OLOOP <- Importance_OLOOP[match(Features_selected, names(Importance_OLOOP))]

### Prediction ###
incv.test$conditions <- factor(as.character(incv.test$conditions),levels=c(ent,"other"))
P_OLOOP <- predict(incv_classifier, newdata = incv.test, type = "prob" )
test_OLOOP <- incv.test$conditions
error_OLOOP <- incv.stats_OLOOP[2]

####  Merge the results
stats_outer_loop_cv <- rbind(stats_outer_loop_cv,ret_stats_OLOOP)	

sen <- rbind( sen , Importance_OLOOP )
time[sum] <- index
test[[sum]] <- incv.test$conditions
pred[[sum]] <- P_OLOOP
error[sum] <- incv.stats_OLOOP[2]
mpar[[sum]] <- list(mtry = mtry)


rminer_list_40 <- list(time,test,pred,error,mpar,model,task,method,sen,sresponses,runs,attributes,feature)
names(rminer_list_40) <- c("time","test","pred","error","mpar","model","task","method","sen","sresponses","runs","attributes","feature")

Best_model_summary_40 <- list(
	Analisis = outdir, 
	Enterotype = ent, 
	parametrs = c(mtry=mtry), 
	Features_selected=Features_selected, 
	balance_method = Method, 
	Fold = Fold,
	mean_auc = mean(stats_outer_loop_cv$AUC,na.rm=T) , 
	mean_MCC = mean(stats_outer_loop_cv$MCC,na.rm=T),
	list_cv_path = list_cv_path,	
	Ind_CV <- index
)

print(Best_model_summary_40)

########################################################################################################################################################
##########################################                Save the results                      ########################################################
########################################################################################################################################################


dir.create(outdir)

name.statistics_models <- paste0("./",outdir,"/statistics_models.inner_loop.",index,".RData")
name.list_feature_selected <- paste0("./",outdir,"/list_feature_selected.inner_loop.",index,".RData")
name.stats_outer_loop_cv <- paste0("./",outdir,"/stats_outer_loop_cv.",index,".RData")
name.Best_model_summary <- paste0("./",outdir,"/Best_model_summary.",index,".RData")
name.rminer_list <- paste0("./",outdir,"/rminer_list.",index,".RData")
name.Random_seed <- paste0("./",outdir,"/Random_seed.",index,".RData")
name.Best_model_summary_40 <- paste0("./",outdir,"/Best_model_summary_40.",index,".RData")
name.rminer_list_40 <- paste0("./",outdir,"/rminer_list_40.",index,".RData")
name.ret_stats_OLOOP <- paste0("./",outdir,"/ret_stats_OLOOP.",index,".RData")

save(statistics_models, file = name.statistics_models)
save(list_feature_selected, file = name.list_feature_selected)
save(stats_outer_loop_cv, file = name.stats_outer_loop_cv)
save(Best_model_summary, file = name.Best_model_summary)
save(rminer_list, file = name.rminer_list)
save(Random_seed, file = name.Random_seed)
save(rminer_list_40, file = name.rminer_list_40)
save(Best_model_summary_40, file = name.Best_model_summary_40)
save(ret_stats_OLOOP, file = name.ret_stats_OLOOP)

stopCluster(cl)
