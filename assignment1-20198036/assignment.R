df= read.csv('data.csv')

df[1:10,] #to show the first 10 rows

head(df,10) #to show the first 10 rows too

tail(df,10) #to show the last 10 rows



#Using Date of Birth attribute, extracts the gender, average commuting
#time, and ancestry data for the oldest three.
orderdDFByDate = df[order(as.Date(df$dob, format="%Y/%m/%d")),]
interest_Att_byDate = orderdDFByDate[1:3,c('gender','avg_commute','ancestry')]
print(interest_Att_byDate)



#Identifies the gender, daily internet use, average commute time, ancestry,
#and diseases among those with more than two children.

interst_Att_byChild = df[df$children >2,c('gender', 'daily_internet_use',
                                          'avg_commute','ancestry')]
print(interst_Att_byChild)



#Using a table , indicate the number of rows that have any missing values
#and the number that do not.

na_count= is.na(df)
table(na_count)


#Provide a summary of the data for each column, showing "Min, 1st Qu,
#Median Mean, 3rd Qu and Max" for each numerical column and the
#Number of each Category for categorical data.
#, df$education,df$marital_status,df$ancestry,df$ancestry
lapply(df, summary)
print('employmen status :' )
table(df$employment_status)

print('Education :')
table(df$education)

print('Marita status :')
table(df$marital_status)

print('Ancestry :')
table(df$ancestry)

print('desease :')
table(df$disease)


#Identify the columns that are having any missing values, and then remove
#any rows where all of the columns have missing values.
#there is no columns that has missing value but to remove na from rows we use na.omit()

#identify columns that have missing values 
na = is.na.data.frame(df)
without_na =0
for(i in na){
  if(i == TRUE){
    without_na = na.omit(df)
    break
  }
}


#Show the average daily usage of the internet for each level of education.
edu = factor(df$education)
avrg_usage = tapply(df$daily_internet_use, edu, mean)
avrg_usage



#Show the distribution of the children count using a histogram
hist(df$children, col="red",main="children count distribution")


#Utilizing line graphs, compare how men and women's avg commute
#distributions differ.

gdf = subset(df, gender == 'female')
denf <- density(gdf$avg_commute)
plot(denf, main="women's avg commute", col= 'pink')

gdm = subset(df, gender == 'male')
denm <- density(gdm$avg_commute)
plot(denm, main="men's avg commute", col= 'blue')

#Make a histogram to show the gender distribution.
cp_df = df
cp_df$gender =ifelse(cp_df$gender=="male", 1, 0) #zero for female , one for male
hist(cp_df$gender, main='Histogram for gender', col = 'black',xlab = 'gender')

g =table(df$gender)
barplot(g,col =c('red','blue') )


#Use a histogram to show gender distribution for each disease.
femaleList  = list()
maleList  = list()
f=m =0
warnings()
for(i  in 1:nrow(df$gender))
{
  for(j in 1:nrow(df$gender))
  {
    if(df$gender[i] == 'female' & df$disease[j] =='Alzheimer'){
      femaleList[f]<- "female Alzheimer"
      f= f+1
    }
    else if (df$gender[i] =='male' & df$disease[j] =='Alzheimer')
    {
      femaleList[m]<- "male Alzheimer"
      m= m+1
    }
    
  }

}

li = c(femaleList,maleList)

barplot(li, col =c('red','blue'))






