set.seed(12345)
library(caret)
library(DMwR)
library(ROSE)
library(mltools)

###################################
#### Create the datasetes #########
###################################
Create_datasetes <- function(in_table = data.frame() ){

	#### Up train ####
	up_train <- upSample(x = in_table[, -1], y = in_table$conditions) 
	colnames(up_train) <- gsub("Class","conditions",colnames(up_train))                        
	dim(up_train)
	table(up_train$conditions)
	 
	#### Down train ####
	down_train <- downSample(x = in_table[, -1],y = in_table$conditions)
	colnames(down_train) <- gsub("Class","conditions",colnames(down_train))                        
	dim(down_train)
	table(down_train$conditions) 

	#### SMOTE train ####
	smote_train <- SMOTE(conditions ~ ., data  = in_table)   
	dim(smote_train)                      
	table(smote_train$conditions) 

	#### ROSE train ####
	rose_train <- ROSE(conditions ~ ., data  = in_table)$data                         
	table(rose_train$conditions) 

	list_ret <- list(up_train, down_train, smote_train, rose_train)
	names(list_ret) <- c("up_train", "down_train", "smote_train", "rose_train")
	return(list_ret)
}

###################################
#############   RFE  ##############
###################################
rfe_wrapper <- function( sizes_splits = "Two_thirds", num_variables = 87 , list_data_frame = list() ){


	##### Create the splits for rfe #######
	if( sizes_splits=="default" ){sizes_num = 2^(2:4) }
	if( sizes_splits=="Third" ){sizes_num = 2:round(num_variables/3) }
	if( sizes_splits=="Half" ){sizes_num = 2:round(num_variables/2) }
	if( sizes_splits=="Two_thirds" ){sizes_num = 2:round(num_variables * 2/3 ) }

	##### Obtain the different datasets #######
	up_train <- list_data_frame[["up_train"]]
	down_train <- list_data_frame[["down_train"]]
	smote_train  <- list_data_frame[["smote_train"]]
	rose_train <- list_data_frame[["rose_train"]]

	##### RFE  #####
	subsetSizes <- sizes_num
	seeds <- vector(mode = "list", length = 11)
	for(i in 1:10) seeds[[i]] <- sample.int(1000, length(subsetSizes) + 1)
	seeds[[11]] <- sample.int(1000, 1)

	control <- rfeControl(functions=rfFuncs, method="repeatedcv", seeds = seeds , number=10, allowParallel = TRUE)

	colnames(up_train) <- paste0(colnames(up_train),"__sep")
	colnames(down_train) <- paste0(colnames(down_train),"__sep")
	colnames(smote_train) <- paste0(colnames(smote_train),"__sep")
	colnames(rose_train) <- paste0(colnames(rose_train),"__sep")


	up.rfe <- rfe(conditions__sep ~ ., data=up_train, sizes=sizes_num, rfeControl=control, metric = "Kappa")
	down.rfe <- rfe(conditions__sep ~ ., data=down_train, sizes=sizes_num, rfeControl=control, metric = "Kappa")
	rose.rfe <- rfe(conditions__sep ~ ., data=rose_train, sizes=sizes_num, rfeControl=control, metric = "Kappa")
	smote.rfe <- rfe(conditions__sep ~ ., data=smote_train, sizes=sizes_num, rfeControl=control, metric = "Kappa")

	up.predictors    <- predictors(up.rfe)
	down.predictors  <- predictors(down.rfe)
	rose.predictors  <- predictors(rose.rfe)
	smote.predictors <- predictors(smote.rfe)

	up.predictors    <- unique(sapply(strsplit(up.predictors, "__sep"), `[`, 1))
	down.predictors  <- unique(sapply(strsplit(down.predictors, "__sep"), `[`, 1))
	rose.predictors  <- unique(sapply(strsplit(rose.predictors, "__sep"), `[`, 1))
	smote.predictors <- unique(sapply(strsplit(smote.predictors, "__sep"), `[`, 1))



	list_ret <- list(up.predictors, down.predictors, rose.predictors, smote.predictors)
	names(list_ret) <- c("up.predictors", "down.predictors", "rose.predictors", "smote.predictors")
	return(list_ret)
}

################################################
#############   Model statistics  ##############
################################################
Matt_Coef <- function (conf_matrix)
{
  TP <- conf_matrix$table[1,1]
  TN <- conf_matrix$table[2,2]
  FP <- conf_matrix$table[1,2]
  FN <- conf_matrix$table[2,1]

  mcc_num <- (TP*TN - FP*FN)
  mcc_den <- 
  as.double((TP+FP))*as.double((TP+FN))*as.double((TN+FP))*as.double((TN+FN))

  mcc_final <- mcc_num/sqrt(mcc_den)
  return(mcc_final)
}


best_model_stats <- function(test_df, classifier_model, ent){

	###############################################
	#### Best model parameters and statistics #####
	###############################################
	whichTwoPct <- tolerance(classifier_model$results, metric = "ROC",  tol = 2, maximize = TRUE)  
	best_mod_stas <- as.numeric( classifier_model$results[whichTwoPct,] )
	names(best_mod_stas) <- paste0("model.train.",colnames(classifier_model$results))


	###############################################
	######## confusion Matrix enterotype ##########
	###############################################
	pre<- predict(classifier_model, newdata = test_df )
	pre <- factor(as.character(pre),levels=c(ent,"other"))
	confusion_Matrix_ent <- confusionMatrix(test_df$conditions,  pre, positive = ent )
	#confusion_Matrix_other <- confusionMatrix(test_df$conditions,  pre, positive = "other" )

	#####################################################
	#### Etimate the Matthews correlation coefficient ###
	#####################################################
	MCC <- mcc(preds=pre, actuals=test_df$conditions)
	#MCC2 <-Matt_Coef(confusion_Matrix_ent)

	###############################################
	######## F1, recall, and precision ############
	###############################################
	con_table <- table(Obs =test_df$conditions, predictions = pre )
	Recall <- caret::recall(con_table, relevant = ent )
	Precision <- caret::precision(con_table, relevant = ent)
	F_Measurement <- caret::F_meas(con_table, relevant = ent, beta = 1)

	###############################################
	########     Estimate the AUC      ############
	###############################################

	prediction <- predict(classifier_model, newdata = test_df, type="prob")
	model.ROC <- roc(predictor=prediction[,ent], response=test_df$conditions, levels=(levels(test_df$conditions)))
	AUC <-  model.ROC$auc[1]

	model.ROC <- roc(predictor=prediction[,ent], response=test_df$conditions, levels=(levels(test_df$conditions)), direction = ">" )
	AUC.controls <-  model.ROC$auc[1]

	model.ROC <- roc(predictor=prediction[,ent], response=test_df$conditions, levels=(levels(test_df$conditions)), direction = "<" )
	AUC.case <-  model.ROC$auc[1]



	ret_vec <- c(MCC = MCC, AUC = AUC, AUC.controls= AUC.controls, AUC.cases = AUC.case, F_Measurement = F_Measurement, Recall=Recall, Precision=Precision, confusion_Matrix_ent$overall, best_mod_stas)

	return(ret_vec)
	
	
}



outer_loop_cv_stats <- function(test_df, classifier_model, ent){

	###############################################
	######## confusion Matrix enterotype ##########
	###############################################
	pre<- predict(classifier_model, newdata = test_df )
	pre <- factor(as.character(pre),levels=c(ent,"other"))
	confusion_Matrix_ent <- confusionMatrix(test_df$conditions,  pre, positive = ent )
	#confusion_Matrix_other <- confusionMatrix(test_df$conditions,  pre, positive = "other" )

	#####################################################
	#### Etimate the Matthews correlation coefficient ###
	#####################################################
	MCC <- mcc(preds=pre, actuals=test_df$conditions)
	#MCC2 <-Matt_Coef(confusion_Matrix_ent)

	###############################################
	######## F1, recall, and precision ############
	###############################################
	con_table <- table(Obs =test_df$conditions, predictions = pre )
	Recall <- caret::recall(con_table, relevant = ent )
	Precision <- caret::precision(con_table, relevant = ent)
	F_Measurement <- caret::F_meas(con_table, relevant = ent, beta = 1)

	###############################################
	########     Estimate the AUC      ############
	###############################################

	prediction <- predict(classifier_model, newdata = test_df, type="prob")
	model.ROC <- roc(predictor=prediction[,ent], response=test_df$conditions, levels=(levels(test_df$conditions)))
	AUC <-  model.ROC$auc[1]

	model.ROC <- roc(predictor=prediction[,ent], response=test_df$conditions, levels=(levels(test_df$conditions)), direction = ">" )
	AUC.controls <-  model.ROC$auc[1]

	model.ROC <- roc(predictor=prediction[,ent], response=test_df$conditions, levels=(levels(test_df$conditions)), direction = "<" )
	AUC.case <-  model.ROC$auc[1]

	ret_vec <- c(MCC = MCC, AUC = AUC, AUC.controls= AUC.controls, AUC.cases = AUC.case, F_Measurement = F_Measurement, Recall=Recall, Precision=Precision, confusion_Matrix_ent$overall)

	return(ret_vec)


}























