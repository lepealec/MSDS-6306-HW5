---
title: "HW5"
author: "Alec Lepe"
date: "9/25/2018"
output: 
  html_document:
    keep_md: true
---



Questions
Backstory: Your client is expecting a baby soon. However, he is not sure what to name the child. Being out of the loop, he hires you to help him figure out popular names. He provides for you raw data in order to help you make a decision.
1. Data Munging (30 points): Utilize yob2016.txt for this question. This file is a series of popular children’s names born in the year 2016 in the United States. It consists of three columns with a first name, a gender, and the amount of children given that name. However, the data is raw and will need cleaning to make it tidy and usable.

```r
#setwd("~/Desktop/SMU/DataScience/Week 4/HW5")
setwd("~/MSDS-6306-Hw5")
df=as.data.frame(fread("yob2016.txt"))
colnames(df)=cc("Name Gender Count")
head(df)
```

```
##       Name Gender Count
## 1     Emma      F 19414
## 2   Olivia      F 19246
## 3      Ava      F 16237
## 4   Sophia      F 16070
## 5 Isabella      F 14722
## 6      Mia      F 14366
```
b. Display the summary and structure of df

```r
summary(df)
```

```
##      Name              Gender              Count        
##  Length:32869       Length:32869       Min.   :    5.0  
##  Class :character   Class :character   1st Qu.:    7.0  
##  Mode  :character   Mode  :character   Median :   12.0  
##                                        Mean   :  110.7  
##                                        3rd Qu.:   30.0  
##                                        Max.   :19414.0
```

```r
str(df)
```

```
## 'data.frame':	32869 obs. of  3 variables:
##  $ Name  : chr  "Emma" "Olivia" "Ava" "Sophia" ...
##  $ Gender: chr  "F" "F" "F" "F" ...
##  $ Count : int  19414 19246 16237 16070 14722 14366 13030 11699 10926 10733 ...
```
c. Your client tells you that there is a problem with the raw file. One name was entered twice and misspelled. The client cannot remember which name it is; there are thousands he saw! But he did mention he accidentally put three y’s at the end of the name. Write an R command to figure out which name it is and display it.

```r
# https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html
grep("y{3}",df[,"Name"],value=T)
```

```
## [1] "Fionayyy"
```
d. Upon finding the misspelled name, please remove this particular observation, as the client says it’s redundant. Save the remaining dataset as an object: y2016

```r
y2016=df[-which(df[,"Name"]==grep("y{3}",df[,"Name"],value=T)),]
```
2. Data Merging (30 points): Utilize yob2015.txt for this question. This file is similar to yob2016, but contains names, gender, and total children given that name for the year 2015.
a. Like 1a, please import the .txt file into R. Look at the file before you do. You might have to change some options to import it properly. Again, please give the dataframe human-readable column names. Assign the dataframe to y2015.

```r
#setwd("~/Desktop/SMU/DataScience/Week 4/HW5")
setwd("~/MSDS-6306-Hw5")
y2015=as.data.frame(fread("yob2015.txt"))
colnames(y2015)=cc("Name Gender Count")
```
b. Display the last ten rows in the dataframe. Describe something you find interesting about these 10 rows.

```r
y2015[((nrow(y2015)-9):nrow(y2015)),]
```

```
##         Name Gender Count
## 33054   Ziyu      M     5
## 33055   Zoel      M     5
## 33056  Zohar      M     5
## 33057 Zolton      M     5
## 33058   Zyah      M     5
## 33059 Zykell      M     5
## 33060 Zyking      M     5
## 33061  Zykir      M     5
## 33062  Zyrus      M     5
## 33063   Zyus      M     5
```
The last ten rows are all male, 5 count, and all start with a "Z'.

c. Merge y2016 and y2015 by your Name column; assign it to final. The client only cares about names that have data for both 2016 and 2015; there should be no NA values in either of your amount of children rows after merging.

```r
colnames(y2015)[2:3]=cc("2015_Gender 2015_Count")
colnames(y2016)[2:3]=cc("2016_Gender 2016_Count")
final=merge(y2015,y2016,by="Name")
final[is.na(final[["2015_Count"]]),]
```

```
## [1] Name        2015_Gender 2015_Count  2016_Gender 2016_Count 
## <0 rows> (or 0-length row.names)
```

```r
final[is.na(final[["2016_Count"]]),]
```

```
## [1] Name        2015_Gender 2015_Count  2016_Gender 2016_Count 
## <0 rows> (or 0-length row.names)
```

```r
# no na's counts
```


3. Data Summary (30 points): Utilize your data frame object final for this part.
a. Create a new column called “Total” in final that adds the amount of children in 2015 and 2016 together. In those two years combined, how many people were given popular names?

```r
Total=final
Total[["Total_Count"]]=final[["2016_Count"]]+final[["2016_Count"]]
Total=Total[rev(order(Total[["Total_Count"]])),]
head(Total)
```

```
##         Name 2015_Gender 2015_Count 2016_Gender 2016_Count Total_Count
## 9820    Emma           F      20415           F      19414       38828
## 9818    Emma           M         10           F      19414       38828
## 23609 Olivia           M          8           F      19246       38492
## 23607 Olivia           F      19638           F      19246       38492
## 23260   Noah           F        137           M      19015       38030
## 23258   Noah           M      19594           M      19015       38030
```

```r
sum(head(Total)[,"Total_Count"])
```

```
## [1] 230700
```

```r
sum((Total)[,"Total_Count"])
```

```
## [1] 11337770
```
Rougly 220,000 babies of the 11,400,000 babies were given the top 6 names.

b. Sort the data by Total. What are the top 10 most popular names?

```r
Total=Total[rev(order(Total[["Total_Count"]])),]
Total[,"Name"][1:10]
```

```
##  [1] "Emma"   "Emma"   "Olivia" "Olivia" "Noah"   "Noah"   "Liam"  
##  [8] "Liam"   "Ava"    "Ava"
```
c. The client is expecting a girl! Omit boys and give the top 10 most popular girl’s
names.

```r
Total[Total[,"2015_Gender"]=="F" & Total[,"2016_Gender"]=="F",][1:10,"Name"]
```

```
##  [1] "Emma"      "Olivia"    "Ava"       "Sophia"    "Isabella" 
##  [6] "Mia"       "Charlotte" "Abigail"   "Emily"     "Harper"
```
d. Write these top 10 girl names and their Totals to a CSV file. Leave out the other columns entirely.

```r
out=Total[Total[,"2015_Gender"]=="F" & Total[,"2016_Gender"]=="F",][1:10,"Name"]
write.csv(out,"Top 10 Girl Names.csv")
```
4. Upload to GitHub (10 points): Push at minimum your RMarkdown for this homework assignment and a Codebook to one of your GitHub repositories (you might place this in a Homework repo like last week). The Codebook should contain a short definition of each object you create, and if creating multiple files, which file it is contained in. You are welcome and encouraged to add other files—just make sure you have a description and directions that are helpful for the grader.

```r
URL=""
```

To complete this assignment, please submit one RMarkdown and matching HTML file at least one hour before your 6th Live Session.
