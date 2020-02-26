# Change working directory in R Studio to allow download of train .txt files;
# Be sure that both the "train" and "test" files are available!  To do this,
# I put both in the directory "UCI HAR Dataset 2"
##setwd(your_directory)

# Download train files 
Xtrain<-read.table("X_train.txt"); ytrain<-read.table("y_train.txt"); sttrain<-read.table("subject_train.txt")

# cbind all three into one big data frame (562nd column is y which is important, 563rd is st)
traindata<-cbind(ytrain,sttrain); traindata<-cbind(Xtrain,traindata)

# Remove X, y and st so as to only keep the one unified data frame
rm(Xtrain);rm(ytrain);rm(sttrain)

# Repeat all above for test .txt files
X<-read.table("X_test.txt"); y<-read.table("y_test.txt"); st<-read.table("subject_test.txt");
testdata<-cbind(y,st); testdata<-cbind(X,testdata);
rm(X);rm(y);rm(st)

# rbind testdata and traindata into one large dataset "alldata"; remove traindata and testdata;
# make last two columns of all data a separate dataframe "supp"
alldata<-rbind(traindata,testdata); rm(testdata); rm(traindata); supp<-alldata[,562:563]; alldata<-alldata[,1:561]

# Download features file and then make a character vector of its names (from column2)
features<-read.table("features.txt"); features<-as.character(features[,2])

# Ascribe "features" vector to names of alldata columns; thereby allowing for paring down
# and starting the process of giving descriptive variable names to columns of alldata
names(alldata)<-features

# Use grep to find all columns that use "mean" or "std"; make numeric vector wit these column numbers;
# remove intermediate vectors
meancols<-as.character(grep("mean",names(alldata))); stdcols<-as.character(grep("std",names(alldata)));
columns<-sort(as.numeric(c(meancols,stdcols))); rm(meancols); rm(stdcols)

# Pare down alldata into only columns with "mean" or "std" in description; call it "datause"
datause<-alldata[,columns]

# Use gsub to clean up Column names (make descriptions a bit more clear)
names(datause)<-gsub("Acc", "Acceleration",names(datause)); names(datause)<-gsub("std", "Standard Deviation",names(datause));
names(datause)<-gsub("fBody", "Frequency: Body",names(datause)); names(datause)<-gsub("tBody", "Time: Body",names(datause));
names(datause)<-gsub("tGravity", "Time: Gravity",names(datause))

# Label each row with the activity that corresponds to the observation: walking,
# walking upstairs, walking downstairs, laying down, standing or sitting.
Activities<-character(10229); Walking<-grep("1",supp[,1]); Activities[Walking]<-c("Walking");
WalkingUp<-grep("2",supp[,1]); Activities[WalkingUp]<-c("Walking Upstairs");
WalkingDown<-grep("3",supp[,1]); Activities[WalkingDown]<-c("Walking Downstairs");
Sitting<-grep("4",supp[,1]); Activities[Sitting]<-c("Sitting");
Standing<-grep("5",supp[,1]); Activities[Standing]<-c("Standing");
Laying<-grep("6",supp[,1]); Activities[Laying]<-c("Laying")
rm(Standing);rm(Sitting);rm(Laying);rm(WalkingUp);rm(WalkingDown);rm(Walking)

# Cbind "Activities" and "datause" and "Subject"
datause<-cbind(Activities,datause,Subject)
datause<-as.data.frame(group_by(datause,Activities,Subject))
# View(datause) will allow you to view whole dataset

# Make df "try" with only means shown
try<-summarize(datause,mean)
###rownames(df2)<-c("Means")
View(try)




