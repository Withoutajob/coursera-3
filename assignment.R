
library(readr)
library(dplyr)
library(dplyr)
library(data.table)

 ##reading TEST GROUP####

bax_test <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//Inertial Signals//body_acc_x_test.txt")
bay_test <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//Inertial Signals//body_acc_y_test.txt")
baz_test <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//Inertial Signals//body_acc_z_test.txt")
bgx_test <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//Inertial Signals//body_gyro_x_test.txt")
bgy_test <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//Inertial Signals//body_gyro_y_test.txt")
bgz_test <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//Inertial Signals//body_gyro_z_test.txt")
tax_test <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//Inertial Signals//total_acc_x_test.txt")
tay_test <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//Inertial Signals//total_acc_y_test.txt")
taz_test <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//Inertial Signals//total_acc_z_test.txt")

 ##reading TRAIN GROUP######

bax_train <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//Inertial Signals//body_acc_x_train.txt")
bay_train <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//Inertial Signals//body_acc_y_train.txt")
baz_train <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//Inertial Signals//body_acc_z_train.txt")
bgx_train <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//Inertial Signals//body_acc_x_train.txt")
bgy_train <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//Inertial Signals//body_acc_y_train.txt")
bgz_train <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//Inertial Signals//body_acc_z_train.txt")
tax_train <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//Inertial Signals//body_acc_x_train.txt")
tay_train <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//Inertial Signals//body_acc_y_train.txt")
taz_train <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//Inertial Signals//body_acc_z_train.txt")

f_test  		   <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//X_test.txt")     
f_train 		   <- fread("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//X_train.txt")             
f_names		   <- read_lines("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//features.txt")

f_names  <- gsub("[0-9]","",f_names)

v <- 1:6
w <- 41:46
z <- 121:126
temp <- NULL
temp1<- NULL
feat_half <- NULL
feat_full <- NULL

for (i in v) 
{
f_test[[i]] 		<- as.matrix(f_test[[i]]) 
f_train[[i]] 		<- as.matrix(f_train[[i]])
temp      			<- f_test[[i]]
temp1     			<- f_train[[i]]
colnames(temp) 		<- f_names[i] 
colnames(temp1) 		<- f_names[i]
feat_half			<- cbind(feat_half,temp)
feat_full			<- cbind(feat_full,temp1)
temp <- NULL
temp1 <- NULL
}
for (i in w) 
{
f_test[[i]] 		<- as.matrix(f_test[[i]]) 
f_train[[i]] 		<- as.matrix(f_train[[i]])
temp      			<- f_test[[i]]
temp1     			<- f_train[[i]]
colnames(temp) 		<- f_names[i] 
colnames(temp1) 		<- f_names[i]
feat_half			<- cbind(feat_half,temp)
feat_full			<- cbind(feat_full,temp1)
temp <- NULL
temp1 <- NULL
}
for (i in v) 
{
f_test[[i]] 		<- as.matrix(f_test[[i]]) 
f_train[[i]] 		<- as.matrix(f_train[[i]])
temp      			<- f_test[[i]]
temp1     			<- f_train[[i]]
colnames(temp) 		<- f_names[i] 
colnames(temp1) 		<- f_names[i]
feat_half			<- cbind(feat_half,temp)
feat_full			<- cbind(feat_full,temp1)
temp <- NULL
temp1 <- NULL
}

features <- rbind(feat_half,feat_full)



#######################################################################################################################
#################creating a matrix with a "mean" and "sd" for each window (128 hz)############

list_test <- list(
bax_test=bax_test,bay_test=bay_test,baz_test=baz_test,bgx_test=bgx_test,
bgy_test=bgy_test,bgz_test=bgz_test,tax_test=tax_test,tay_test=tay_test,
taz_test=taz_test)

list_train <- list(
bax_train=bax_train,bay_test=bay_train,baz_train=baz_train,bgx_train=bgx_train,
bgy_train=bgy_train,bgz_train=bgz_train,tax_train=tax_train,tay_train=tay_train,taz_train=taz_train)

vec	 <- c("bax","bay","baz","bgx","bgy","bgz","tax","tay","taz")


s 			<- seq(1,length(vec_test),by=1)
complete_df_test	<- rep(NA,2947)
complete_df_train	<- rep(NA,2947)
temp 			<- rep(NA,2947) 

for (i in s) 

{ 
	list_test[[i]]			<- as.matrix(list_test[[i]])
	nr 				<- seq(1,nrow(list_test[[i]]),by=1) 
	value1 <- NULL
	value2 <- NULL

	for (n in nr) 
		{
		value1			<- c(value1, mean(list_test[[i]][n,]))
		value2			<- c(value2, sd(list_test[[i]][n,]))
		}

	pasted1 			<- paste(vec[i],"_mean",sep="")
	pasted2 			<- paste(vec[i],"_sd",sep="")
	temp				<- cbind(value1,value2)
	colnames(temp)	  	<- c(pasted1,pasted2)
	
	complete_df_test		<- cbind(complete_df_test,temp)
		
}

complete_df_test 				<- as.data.frame(complete_df_test)
complete_df_test$complete_df_test   <- NULL
temp 						<- rep(NA,2947) 


#############second dataframe

s 			<- seq(1,length(vec_train),by=1)

for (i in s)

{ 
	list_train[[i]]		<- as.matrix(list_train[[i]])
	nr 				<- seq(1,nrow(list_train[[i]]),by=1) 
	value1 <- NULL
	value2 <- NULL

	for (n in nr) 
		{
		value1			<- c(value1, mean(list_train[[i]][n,]))
		value2			<- c(value2, sd(list_train[[i]][n,]))
		}

	pasted1 			<- paste(vec[i],"_mean",sep="")
	pasted2 			<- paste(vec[i],"_sd",sep="")
	temp				<- cbind(value1,value2)
	colnames(temp)	  	<- c(pasted1,pasted2)
	
	complete_df_train			<- cbind(complete_df_train,temp)
		
}
complete_df_train 			 <- as.data.frame(complete_df_train)
complete_df_train$complete_df_train  <- NULL



##########################################################################################################
#######changing name for each activity id################################################################

activity_id_test      <- read_lines("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//y_test.txt")
activity_id_train     <- read_lines("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//y_train.txt")

activity_id_test 		<- as.numeric(activity_id_test)
activity_id_test 		<- gsub(1,"walking",activity_id_test)
activity_id_test 		<- gsub(2,"walking_up",activity_id_test)
activity_id_test 		<- gsub(3,"walking_down",activity_id_test)
activity_id_test 		<- gsub(4,"sitting",activity_id_test)
activity_id_test 		<- gsub(5,"standing",activity_id_test)
activity_id_test 		<- gsub(6,"laying",activity_id_test)
activity_id_train 	<- as.numeric(activity_id_train)
activity_id_train 	<- gsub(1,"walking",activity_id_train)
activity_id_train 	<- gsub(2,"walking_up",activity_id_train)
activity_id_train 	<- gsub(3,"walking_down",activity_id_train)
activity_id_train 	<- gsub(4,"sitting",activity_id_train)
activity_id_train 	<- gsub(5,"standing",activity_id_train)
activity_id_train 	<- gsub(6,"laying",activity_id_train)

#########################################################################################################
# creating a time reference for each table

subject_id_test  	    <- read_lines("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//test//subject_test.txt")
subject_id_train      <- read_lines("C://Users//ivano//Documents//R//Rdocument//R codes//UCI HAR Dataset//train//subject_train.txt")


subject_id_test 		<- as.numeric(subject_id_test)
ids_test         		<- cbind(subject_id_test,activity_id_test)
ids_test			<- as.data.frame(ids_test)

lev <- as.factor(as.character(sort(as.numeric(levels(factor(ids_test[,1]))))))

len <- seq(1,length(lev),by=1)
time <- c()

for (i in len)
{
	a     <- paste("^",lev[i],"$",sep="")
	max 	<- length(grep(a,ids_test[,1]))
	seq 	<- seq(1,max,by=1)
	time  <- c(time,seq)
	
}

time 				 <- time*2.56
ids_test			 <- cbind(ids_test,time)
colnames(ids_test)	 <- c("subject_id","activity","time")




###second dataframe##################################################





subject_id_train 		<- as.numeric(subject_id_train)
ids_train         	<- cbind(subject_id_train,activity_id_train)
ids_train			<- as.data.frame(ids_train)

lev <- as.factor(as.character(sort(as.numeric(levels(factor(ids_train[,1]))))))

len <- seq(1,length(lev),by=1)
time <- c()

for (i in len)
{
	a     <- paste("^",lev[1],"$",sep="")

	a     <- paste("^",lev[i],"$",sep="")
	max 	<- length(grep(a,ids_train[,1]))
	seq 	<- seq(1,max,by=1)
	time  <- c(time,seq)
	
}

time 				 <- time*2.56
ids_train			 <- cbind(ids_train,time)
colnames(ids_train)	 <- c("subject_id","activity","time")

############################################################################################################
#merging all togheter

complete_df_test  	<- cbind(ids_test,complete_df_test)
complete_df_train 	<- cbind(ids_train,complete_df_train)

complete_df 		<- rbind(complete_df_test,complete_df_train)

complete_df			<- complete_df[order(complete_df[,1]),]
complete_df$		<-NULL 

colnames(complete_df)   <- c( "subject_id","activity","time",
					"x_body_acc_mean","x_body_acc_sd",
				      "y_body_acc_mean","y_body_acc_sd",
					"z_body_acc_mean","z_body_acc_sd",
					"x_gyro_acc_mean","x_gyro_acc_sd",
				      "y_gyro_acc_mean","y_gyro_acc_sd",
					"z_gyro_acc_mean","z_gyro_acc_sd",
					"x_tot_acc_mean","y_tot_acc_mean","z_tot_acc_mean",
				      "x_tot_acc_sd",  "y_tot_acc_sd",  "z_tot_acc_sd")

complete_df_with_features		<- cbind(complete_df,features)

###########################################################################################################
####making the separate dataset; i'm eliminating time column#########

complete_df$time 		<- NULL

complete_df$subject_id 	<- as.factor(complete_df$subject_id)
complete_df$activity 	<- as.factor(complete_df$activity)

summary_df <- complete_df %>%  group_by(complete_df$subject_id,complete_df$activity) %>% summarise_all(mean)









