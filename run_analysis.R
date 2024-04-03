# Merges the training and the test sets to create one data set.


X_training <- read.table("data/train/X_train.txt")
X_test <- read.table("data/test/X_test.txt")
df <- rbind(X_training,X_test)
# head(df, 1)

# Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("data/features.txt")[,2]
colnames(df) <- features
selected_measures <- grep('-(mean|std)\\(',features)

library('dplyr')
df_m <-  df %>% select( grep('-(mean|std)\\(',features))
head(df_m, 1)

# Uses descriptive activity names to name the activities in the data set

## remove all () in the names
colnames(df_m) <- gsub("\\(\\)", "", colnames(df_m))
## replace all - in the names
colnames(df_m) <- gsub("-", " ", colnames(df_m))

# Appropriately labels the data set with descriptive variable names.
y_train <- read.table("data/train/y_train.txt")
y_test <- read.table("data/test/y_test.txt")
df_y <- rbind(y_train,y_test)[,1]
labels <- read.table("data/activity_labels.txt")[,2]
df_y <- labels[df_y]
df_m$Activity <- df_y

# From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable 
# for each activity and each subject.
subject_train <- read.table("data/train/subject_train.txt")
subject_test <- read.table("data/test/subject_test.txt")
subjects <- rbind(subject_train,subject_test)[,1]
df_m <- cbind(Subject = subjects,df_m)

df_average <- df_m %>%
  group_by(Subject,Activity) %>%
  summarise_each(list(mean=mean))
write.csv(df_average, file = "tidy_data_set.txt", row.names = FALSE)

# generate code book
code_book <- lapply(names(df_average), function(var) {
  data.frame(
    Variable = var,
    Description = "", # Replace with actual descriptions
    Type = sapply(df_average[[var]], typeof),
    Levels = if (is.factor(df_average[[var]])) levels(df_average[[var]]) else NA,
    Example.Values = paste(df_average[[var]][1:3], collapse = ", ")
  )
})
code_book_df <- do.call(rbind, code_book)
write.csv(code_book_df, file = "code_book.csv", row.names = FALSE)

