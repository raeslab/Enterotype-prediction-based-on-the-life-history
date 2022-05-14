set.seed(12345)
library(caret)
library(purrr)

#######################################################################################################################################
##########################################             FUNCTIONS                   ####################################################
#######################################################################################################################################

kfold_CV_list_file <- function( list_ents=list(), numKfolds = 10, scale = "" , out_dir = "",out_names=""){	
	for(i in names(list_ents)){
		ent_df <- list_ents[[i]]
		Fold_ind_train <- createFolds(ent_df$conditions, k = numKfolds, list = TRUE, returnTrain = T)
		CV_List_data_frame <- vector(mode = "list", length = numKfolds)
			
		for( incv in 1:length(names(Fold_ind_train)) ){
			Train <- ent_df[Fold_ind_train[[incv]] , ]
			Test <- ent_df[-Fold_ind_train[[incv]] , ]
			CV_List_data_frame[[incv]] <- list(Train=Train,Test=Test)
		}
		cv_name <- paste(out_names,i,"CV",numKfolds,"SCALE",scale,sep="_")
		cv_name <- paste0(cv_name,".RData")
		outdir_name <- paste0(out_dir,"/kfold_",numKfolds)
		#out_file_name <- paste0(outdir_name,"/",cv_name)
		#dir.create(outdir_name,showWarnings = TRUE)
		save(CV_List_data_frame, file = cv_name)
	}
}


###  Generate ramdom data ###
ent_rand_df_func <- function(n = 10000, nsubjects = 300, nvar = 75, enterotype = ""){

	rnames<- paste0("Subject_",1:nsubjects)
	cnames<- paste0("var",1:75)

	num_subjects_ent <- 0.2 * 	nsubjects
	num_subjects_noent <- nsubjects - num_subjects_ent

	Entdf <-rbind(
		data.frame(conditions = enterotype, matrix(rnorm(n, mean = 0, sd = 1), num_subjects_ent,75) ) , 
		data.frame(conditions = "other", matrix(rnorm(n, mean = 2, sd = 1),    num_subjects_noent,75) )
	)
	colnames(Entdf) <- c("conditions",cnames)
	rownames(Entdf) <- rnames

	return(Entdf)

}




#######################################################################################################################################
##########################################             Script                   #######################################################
#######################################################################################################################################

bac1 <- ent_rand_df_func(n = 10000, nsubjects = 300, nvar = 75, enterotype = "Bact1")
bac2 <- ent_rand_df_func(n = 10000, nsubjects = 300, nvar = 75, enterotype = "Bact2")
prevo <- ent_rand_df_func(n = 10000, nsubjects = 300, nvar = 75, enterotype = "Prev")
rumi <- ent_rand_df_func(n = 10000, nsubjects = 300, nvar = 75, enterotype = "Rum")
sub_ent_list <- list(bac1,bac2,prevo,rumi)
names(sub_ent_list) <- c("Bact1","Bact2","Prev","Rum")
kfold_CV_list_file(list_ents=sub_ent_list, numKfolds = 40, scale = "TRUE" , out_dir = "./",  out_names="All_years")










