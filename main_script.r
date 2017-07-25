rm(list = ls())
library('zoo')

############################# setting up path ############################# 

path <-"C:\\Users\\Ashutosh\\Desktop\\Submission"

setwd(path)

############################  reading data ####################################

print("Reading data ...")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
submission <- read.csv("sample_submission.csv")

################# Function to compute Quadatically Weighted mean ####################### 

wt_mean <- function(x){
  x <- x[order(-x)]
  wt <- as.numeric((1 : length(x))^2)
  wt <- wt/sum(wt)
  m <- sum(wt*x)
  m
}

######################################## Pre-processing  ######################################

## Converting date into numeric values

print("Preprocessing ...")
train$Date <- as.character(train$Date)
train$Date <- apply(as.data.frame(train$Date), 1, function(x) paste(substr(x,1,4),substr(x,5,6),sep = '-'))
train$Date <- as.yearmon(train$Date, '%Y-%m')
train$Date <-  as.yearmon('2014-01','%Y-%m') - train$Date

## Outlier treatment

dd <- train
dd$first.char <- substr(dd$Event,start = 1, stop = 1)
dd$two.char <- substr(dd$Event,start = 1, stop = 2)


dd <- subset(dd, dd$first.char  != '8')
dd <- subset(dd, dd$first.char  != '6')
dd <- subset(dd, dd$first.char  != '9')
dd <- subset(dd, dd$first.char  != 'E')
dd <- subset(dd, dd$first.char  != 'A')
dd <- subset(dd, dd$first.char  != '0')


train <- dd
train$first.char <- NULL
train$two.char <- NULL


## Storing Data of each patient into a list

patient_list <- split(train,as.factor(train$PID))
n <- length(patient_list)




############################## Global Probability Estimation##################################### 

events <- train$Event
n_record <- length(events)
event_prob <- as.data.frame(table(events))
event_prob$prob.glob <- event_prob$Freq/n_record
event_prob$Freq <- NULL  

#################################  Processing  ########################################### 

print("Processing . . . ")

solution <- data.frame()
id <- c()
for(i in 1:n){
  print("Record ...")
  print(i)
  
  ## Splitting Data for a patient based on month  
  
  patient <- patient_list[[i]]
  pid <- patient[1,1]
  patient_month <- split(patient, patient$Date)
  
  date_dis <- tapply(patient$Date ,patient$Event, wt_mean)
  date_dis <- date_dis[!is.na(date_dis)]
  date_dis <- data.frame(rownames(date_dis), date_dis)
  
  ############################  Monthly Probability Estimation ###############################
  
  un_patient_month <- lapply(patient_month ,function(x) unique( as.character(x[[3]])) )
  pat_month_vec  <- as.vector(unlist(un_patient_month))
  monthly_freq <- as.data.frame(table(pat_month_vec))
  monthly_freq <- monthly_freq[order(-monthly_freq$Freq),]
  
  
  monthly_freq_date <- merge(date_dis, monthly_freq, by.x = "rownames.date_dis." , by.y  = "pat_month_vec")
  monthly_freq_date$monthly_prob <- monthly_freq_date$Freq/(sum(monthly_freq$Freq)*monthly_freq_date$date_dis )
  monthly_freq_date$Freq <- NULL
  
  
  
  monthly_freq_date <- monthly_freq_date[order(-monthly_freq_date$monthly_prob),]
  
  pred <- monthly_freq_date$rownames.date_dis.[1:10]
  
  
  pred <- as.matrix(as.character(pred))
  pred <- t(pred)
  id <- c(id, pid)
  solution <- rbind(solution,pred)
  #colnames(solution) <- NULL
}
solution <- cbind(id, solution)



#################################### Post-processing #########################################

## Generating "predictions.csv" file

test$row.no <- 1:nrow(test)
names(solution) <- names(submission)
fin_solution <- merge(solution, test,all = T)
fin_solution <- fin_solution[order(fin_solution$row.no),]
fin_solution$row.no <- NULL
write.csv(fin_solution,"predictions.csv",row.names = F)
