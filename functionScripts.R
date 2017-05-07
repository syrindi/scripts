#####################################
#
#     Function scripts
#       -dateMaker -  create date1 to have a discrete date column to plot
#               -otherwise plotly does not plot properly
#       -monthValCalc - calcuale the monthly value from cum data
#       -dateMaker - creates date column depending on grouper
#       -dataTAbleGenerator - creates html table code from df
#       -addDateFields: main fx to derive the final data output
#       -addDateFields1: main fx to derive the final data output
#           -adds from and to params which is used in skillsDailyCollection 
#             to derive data from 2 prev years
#       -addDateFields2: main fx to derive the final data output
#           -adds from and to params which is used in skillsDailyCollection 
#             to derive data from 2 prev years
#           - adds group_by grouper and "key" (NSE, SE)
#       -findUser: changes user if specified by recognized name
#       -timeGroup: returns the var to group df by  
#       -goSource - determines platform and prepends appropriate dir 
#       -conditionTesst - tests if text meets conditons for skills
#           -specifiy the regex only - compares to the text automatically in gmail
#       -conditionTesst1 - tests if text meets conditons for skills
#           -specifiy the text (two arguments...regex and text to copre to)
#       -nextMonth.... outputs dates from week/month...etc
#       -getDate - gets the dates from above
#           -user calls getDates which calls nextMonth...etc
#       -checkForDate - checks to see if there is date in the text\
#       -dateClean - cleans Dates for date format
#           - might need to be updated
#######################################

#library(quanteda)
#library(qdap)
#library(rJava)
#library(NLP)
#library(openNLP)
library(lubridate)
library(rmarkdown)
library(stringr)
library(dplyr)
library(tidyr)
library(knitr)
library(mongolite)
library(data.table)
library(pander)
library(gmailr)
library(magrittr)
library(XLConnect)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(plotly)
library(googleVis)
library(DT)





##################################
#
#  creates html table for datatable from df
#   adds class tablex which links to the DT js library
#
#################################


## create date1 to have a discrete date column to plot
## otherwise plotly does not plot properly

dateMaker = function(df)
{
  if (grouper == "yw")
  {
    df$date1 = as.Date(paste("1", df$week, df$year, sep = "-"), format = "%w-%W-%Y")
    
  }
  
  
  if (grouper == "ym")
  {
    df$date1 = as.Date(paste("1", df$month, df$year, sep = "-"), format = "%d-%m-%Y")
    
  }
  
  if (grouper == "dos")
  {
    df$date1 = df$dos
  }
  
  if (grouper == "year")
  {
    df$date1 = df$year
  }
  return(data.frame(df))
}


### calculates the monthly numbers from cumulative val
### requires month column
monthValCalc = function(df, date,value)
{
  df$month = month(df$date)
  df$monthVal = ifelse(df$month ==1 , df[[value]], df[[value]]-lag(df[[value]]) )
  return(df)
}



#### creates a date1 column depending on the grouper

dateMaker = function(df)
{
  if (grouper == "yw")
  {
    df$date1 = as.Date(paste("1", df$week, df$year, sep = "-"), format = "%w-%W-%Y")
    
  }
  
  
  if (grouper == "ym")
  {
    df$date1 = as.Date(paste("1", df$month, df$year, sep = "-"), format = "%d-%m-%Y")
    
  }
  
  if (grouper == "dos")
  {
    df$date1 = df$dos
  }
  
  if (grouper == "year")
  {
    df$date1 = df$year
  }
  return(data.frame(df))
}



### data generator

dataTableGenerator = function(datx)
{
  
  # header and footer table tags
  tableId = paste0("Table",round(runif(1,1,23)))
  beginTable = gsub(",","",toString(sprintf("<th>%s</th>",names(datx))))
  
  beginTable = as.character(beginTable)
  
  
  ## function to create table tags for the main data
  tdExtract = function(dat)
  {
    mainData = list()
    
    for (i in 1:nrow(dat))
    {
      tds = sprintf("<td>%s</td>", dat[i,])
      mainData[[i]] = gsub(",","",sprintf("<tr>%s</tr>", toString(tds)))
      
    }
    mainDat = do.call(rbind, mainData)
    return (mainDat)
  }
  
  
  mainTable = gsub(",","",toString(tdExtract(datx)))
  mainTable = as.character(mainTable)
  
  
  tabled = paste0('<table class="tablex" class="display" cellspacing="0" width="100%">
                  <thead>
                  <tr>',
                  beginTable,
                  '</tr>
                  </thead>
                  <tfoot>
                  <tr>',
                  beginTable,
                  '</tr>
                  </tfoot>
                  <tbody>',
                  mainTable,  
                  '</tbody>
                  </table>')
  
  return(tabled)
  
}



##################################
#
# depending on grouper, selects and arranges appropriate fields
#
#################################

dateExpander = function(df)
{   
   if(grouper =="dos")
  {
    #df1 = select(df, dos, count) 
    # names(df1) =c("dos", "countx")
    df1= arrange(df, desc(dos))
  }
  
  if(grouper =="year")
  {
    #df$year = as.numeric(str_sub(datax2$yw,1,4))
    #df1 = select(df, year, count) 
    #names(df1) =c("yearx", "countx")
    df1 = arrange(df, desc(year))
  }
  
  if(grouper =="yw")
  {
    df$year = as.numeric(str_sub(df$yw,1,4))
    df$week = as.numeric(str_replace_all(df$yw,"[0-9]+-",""))
    df1 = arrange(df, desc(year),desc(week))
  }
  
  if(grouper =="ym")
  {
    df$year = as.numeric(str_sub(df$ym,1,4))
    df$month = as.numeric(str_replace_all(df$ym,"[0-9]+-",""))
    #df1 = select(df, year, month, count) 
    #names(df1) =c("yearx","monthx", "countx")
    df1 = arrange(df, desc(year),desc(month))
  }
  
  return(df1)
}




################################
#
# 
# AddDateFields
#   -adds date fields to group by
#   -creates summary table depending on sum vs lenght
#   -calls dateExpander which cleans the fields depending on grouping var
#   -input
#       -data frame
#       -fields to include in summary table (c(x,y))
#       -type of operation - sum or length
#       -includeUser - include user in filter
#         -clinicalSummary is a noUser example since laready filtered
#           - in this df, no doctor col is possible
############################



AddDateFields = function(df, fieldsToInclude,type, includeUser)
{
  type = type
  data = df
  data$dos = ymd(data$dos)
  data$week = week(data$dos)
  data$yw=paste(year(data$dos), week(data$dos), sep= "-")
  data$ym=paste(year(data$dos), month(data$dos), sep= "-")
  data$year = year(data$dos)
  data$month = month(data$dos)
  data$quarter = quarters(data$dos)
  
  
  # step1 - filter
  
  if (includeUser == "user")
  {
  data1 = filter(data, dos >from1, dos < to1, grepl(user, data$doctor))
  }
  
  if (includeUser == "noUser")
  {
    data1 = filter(data, dos >from1, dos < to1)
  }
  
  
  
  # step2 - group and summarise
  ### fieldsToInclude = list as c(x,y) the fileds to include in the summarise_each
  
  if (length(grouper)>0)
  {
    if (type == "length")
    {
      data2 = data1 %>% group_by_(grouper) %>%
        summarise_each_(funs(length(.)), fieldsToInclude)
    }
    
    if (type == "sum")
    {
      data2 = data1 %>% group_by_(grouper) %>%
      summarise_each_(funs(sum(.)), fieldsToInclude)
    }
    
  } else
  {    
    data2 = data1
  }
  
  data2 = dateExpander(data2)
  
  
  return(data2)
}


### adds from and to params
AddDateFields1 = function(df, fieldsToInclude,type, includeUser, from, to, doc)
{
  type = type
  data = df
  data$dos = ymd(data$dos)
  data$week = week(data$dos)
  data$yw=paste(year(data$dos), week(data$dos), sep= "-")
  data$ym=paste(year(data$dos), month(data$dos), sep= "-")
  data$year = year(data$dos)
  data$month = month(data$dos)
  data$quarter = quarters(data$dos)
  
  if (includeUser == "user")
  {
    print(paste("user = ", user))
    data1 = filter(data, dos >from, dos < to, grepl(doc, data$doctor))
  }
  
  if (includeUser == "noUser")
  {
    data1 = filter(data, dos >from, dos < to)
  }
  
  ### fieldsToInclude = list as c(x,y) the fileds to include in the summarise_each
  if (length(grouper)>0)
  {
    if (type == "length")
    {
      data2 = data1 %>% group_by_(grouper) %>%
        summarise_each_(funs(length(.)), fieldsToInclude)
    }
    
    if (type == "sum")
    {
      data2 = data1 %>% group_by_(grouper) %>%
        summarise_each_(funs(sum(.)), fieldsToInclude)
    }
  } else
  {    
    data2 = data1
  }
  
  data2 = dateExpander(data2)
  
  
  return(data2)
}







### adds from and to params as well as groupby grouper and "key"
AddDateFields2 = function(df, fieldsToInclude,type, includeUser, from, to, doc)
{
  
  ## add date fields
  type = type
  data = df
  data$dos = ymd(data$dos)
  data$week = week(data$dos)
  data$yw=paste(year(data$dos), week(data$dos), sep= "-")
  data$ym=paste(year(data$dos), month(data$dos), sep= "-")
  data$year = year(data$dos)
  data$month = month(data$dos)
  data$quarter = quarters(data$dos)
  
  
  ## filter by user or no user
  
  if (includeUser == "user")
  {
    print(paste("user = ", user))
    data1 = filter(data, dos >from, dos < to, grepl(doc, data$doctor))
  }
  
  if (includeUser == "noUser")
  {
    data1 = filter(data, dos >from, dos < to)
  }
  
  
  ### length vs sum
  ### fieldsToInclude = list as c(x,y) the fileds to include in the summarise_each
  if (length(grouper)>0)
  {
    if (type == "length")
    {
      data2 = data1 %>% group_by_(grouper) %>%
        summarise_each_(funs(length(.)), fieldsToInclude)
    }
    
    if (type == "sum")
    {
      data2 = data1 %>% group_by_(grouper) %>%
        summarise_each_(funs(sum(.)), fieldsToInclude)
    }
    
    ### added on 4/2 to allow for grouping by key after grouper
    if (type == "sumAll")
    {
      data2 = data1 %>% group_by_(grouper, "key") %>%
        summarise_each_(funs(sum(.)), fieldsToInclude)
    }
    
  } else
  {    
    data2 = data1
  }
  
  
  ## add dateExpanson
  data2 = dateExpander(data2)
  
  
  return(data2)
}





################################
#
# if person is specfied then changes user 
# user is already defined by sender in the gamailScriptx.R
#
############################


findUser = function(sender)
{
    person = str_extract(text, '[Pp]aul|[Cc]hris|[Rr]alph|[Mm]ike')
    user = user
    
    if(!is.na(person))
    {
     if (person == "Mike"|person=="mike")
     {user = "khan"}
     if (person == "Chris"|person == "chris")
     {user = "jer"}
     if (person == "ralph"|person == "Ralph")
     {user = "ram"}
     if (person == "paul"|person == "Paul")
     {user = "lee"}
   }
  
  
  return(user)
  
}



################################
#
# returns the time group to group data by
# the df will need variables called yw, ym, year and dos
#
############################
timeGroup =  function(text)
{
  tp = "yw"
  
  if (length(grep("by week|weekly", text))>0)
  {tp = "yw"} 
  
  if (length(grep("by day|daily", text))>0)
  {tp = "dos"} 
  
  if (length(grep("by month|monthly", text))>0)
  {tp = "ym"}
  
  if (length(grep("by year|yearly", text))>0)
  {tp = "year"}
  
  
  return(tp)
}




#######################################
#
#  determines platform and appends the appropriate dir prefix
#  ex. source(goSource(x))
#  this is also listed outside ofthis script since you need a path to this file in any system
######################################

goSource = function(skills)
{
  platform = Sys.info()['sysname']
  
  if (platform == "Darwin")
  { 
    a = "~/Desktop/gmailr/"
    b = paste0(a, skills)
  } else if (platform =="Windows")
  {
    a = "C://raymondProject/gmailr/"
    b = paste0(a, skills)
  }
  return(b)
}


#######################################################
### function to determine if a text meets the conditions
### to exercise a skill
######################################################

conditionTest = function(x)
{
  # test keywords separated by columns then remove columns after unlisted
  # creates a df of if a word is in the text as T or F
  tested = list();
  a = unlist(str_split(x, " "))
  a = gsub(",","",a)
  for (i in 1:length(a))
  {
    tested[[i]] = length(grep(a[i], text, ignore.case = T))>0
  }
  testedWords = data.frame(status = do.call(rbind, tested))
  return(testedWords)
}




#######################################################
#  generic version, identigy the string to compare to 
######################################################



conditionTest1 = function(x, text)
{
  # test keywords separated by columns then remove columns after unlisted
  # creates a df of if a word is in the text as T or F
  tested = list();
  a = unlist(str_split(x, " "))
  a = gsub(",","",a)
  for (i in 1:length(a))
  {
    tested[[i]] = length(grep(a[i], text, ignore.case = T))>0
  }
  testedWords = data.frame(status = do.call(rbind, tested))
  return(testedWords)
}






##################################
#
#  converts date references to dates
#  returns the first date of each time period
#     - but month returns first and last aay
#  day/week/month/year
#
##################################

today = function(text)
{
  a = Sys.Date()
  return(a)
}


tomorrow = function(text)
{
  a = Sys.Date()
  b = seq(as.Date(a), length = 2, by = "1 day")[2]
  return(b)
}


yesterday = function(text)
{
  a = Sys.Date()
  b = seq(as.Date(a), length = 2, by = "-1 day")[2]
  return(b)
}



lastWeek = function(text)
{
  a = Sys.Date()
  b = wday(a) + 7 # no of days since beg of last week
  b1 = seq(as.Date(a), length = b, by = "-1 day")[b]
  b2 = seq(as.Date(b1), length = 7, by = "+1 day")[7]
  b3 = c(b1,b2)
  return(b3)
}


nextWeek = function(text)
{
  a = Sys.Date()
  b = 9-wday(a) # no of days until beg of next week - had to use 9 to account for today
  b1 = seq(as.Date(a), length = b, by = "+1 day")[b]
  b2 = seq(as.Date(b1), length = 7, by = "+1 day")[7]
  b3 = c(b1,b2)
  return(b3)
}


thisWeek = function(text)
{
  a = Sys.Date()
  b = wday(a) # days since past Sunday
  c1 = seq(as.Date(a), length = b, by = "-1 day")[b]
  c2 = seq(as.Date(c1), length = 7, by = "+1 day")[7]
  c3 = c(c1,c2)
  return(c3)
}

lastMonth = function(text)
{
  a = Sys.Date()
  b = seq(as.Date(a), length = 2, by = "-1 month")[2]
  c = unlist(str_split(b,"-"))
  d = paste(c[1],c[2],"01", sep = "-")
  e = seq(as.Date(d), length = 2, by = "1 month")
  return(e)
}


nextMonth = function(text)
{
  a = Sys.Date()
  b = seq(as.Date(a), length = 2, by = "+1 month")[2]
  c = unlist(str_split(b,"-"))
  d = paste(c[1],c[2],"01", sep = "-")
  d1 = seq(as.Date(d), length = 2, by = "+1 month")
  return(d1)
}


thisMonth = function(text)
{
  a = Sys.Date()
  b = unlist(str_split(a,"-"))
  c = paste(b[1],b[2],"01", sep = "-")
  d = seq(as.Date(c), length = 2, by = "+1 month")
  
  return(d)
}


thisYear = function(text)
{
  a = Sys.Date()
  b = unlist(str_split(a,"-"))
  c = paste(b[1],"01","01", sep = "-")
  d = seq(as.Date(c), length = 2, by = "+1 year")
  return(d)
}


lastYear = function(text)
{
  a = Sys.Date()
  b = seq(as.Date(a), length = 2, by = '-1 year')[2]
  c = unlist(str_split(b,"-"))
  d = paste(c[1],"01","01", sep = "-")
  e = seq(as.Date(d), length = 2, by = "+1 year")
  
  return(e)
}



nextYear = function(text)
{
  a = Sys.Date()
  b = seq(as.Date(a), length = 2, by = '+1 year')[2]
  c = unlist(str_split(b,"-"))
  d = paste(c[1],"01","01", sep = "-")
  e = seq(as.Date(d), length = 2, by = "+1 year")
  return(e)
}



lastXWeeks = function(text)
{
  a = Sys.Date()
  num = str_extract(text,numbers)
  nNum = as.numeric(numLut[num])
  
  if(is.na(num))
  {
    num = as.numeric(str_extract(text,"[0-9]+"))
    nNum = num
  }
  b = wday(a) + 7*nNum # no of days since beg of last week
  b1 = seq(as.Date(a), length = b, by = "-1 day")[b]
  b2 = seq(as.Date(b1), length = b, by = "+1 day")[b]
  b3 = c(b1,b2)
  return(b3)
}


NextXWeeks = function(text)
{
  a = Sys.Date()
  num = str_extract(text,numbers)
  nNum = as.numeric(numLut[num])
  
  if(is.na(num))
  {
    num = as.numeric(str_extract(text,"[0-9]+"))
    nNum = num
  }
  #nNum+1 to take it to the end pf the nth week, not beginning
  b =  7*(nNum+1) # no of days since beg of last week
  b1 = seq(as.Date(a), length = b, by = "+1 day")[1]
  b2 = seq(as.Date(a), length = b, by = "+1 day")[b]
  b3 = c(b1,b2)
  return(b3)
}


lastXMonths = function(text)
{
  a = Sys.Date()
  num = str_extract(text,numbers)
  nNum = as.numeric(numLut[num])
  
  if(is.na(num))
  {
    num = as.numeric(str_extract(text,"[0-9]+"))
    nNum = num
  }
  # nNUm+1 to account for this month
  b = seq(as.Date(a), length = nNum+1, by = "-1 month")
  b1 = b[length(b)]
  day(b1) = 01
  b2 = b[1]  # takes it the first day pf n+1 month 
 # day(b2) = 01
  b3 = c(b1,b2)
  return(b3)
}



nextXMonths = function(text)
{
  a = Sys.Date()
  num = str_extract(text,numbers)
  nNum = as.numeric(numLut[num])
  
  if(is.na(num))
  {
    num = as.numeric(str_extract(text,"[0-9]+"))
    nNum = num
  }
  # nNUm+2 to take it the first day of n+1 month foe filtering through of n months
  b = seq(as.Date(a), length = nNum+2, by = "+1 month")
  b1 = b[length(b)]
  day(b1) = 01
  b2 = b[1]
  day(b2) = 01
  b3 = c(b2,b1)
  return(b3)
}



lastXDays = function(text)
{
  a = Sys.Date()
  num = str_extract(text,numbers)
  nNum = as.numeric(numLut[num])
  
  if(is.na(num))
  {
    num = as.numeric(str_extract(text,"[0-9]+"))
    nNum = num
  }
  # nNUm+1 to account for this month
  b = seq(as.Date(a), length = nNum+1, by = "-1 day")
  b1 = b[length(b)-1]
  b2 = b[1]
  b3 = c(b1,b2)
  return(b3)
}



lastXNumDays = function(text)
{
  a = Sys.Date()
  num = str_extract(text,numbers)
  nNum = as.numeric(numLut[num])
  
  if(is.na(num))
  {
    num = as.numeric(str_extract(text,"[0-9]+"))
    nNum = num
  }
  # nNUm+1 to account for this month
  b = seq(as.Date(a), length = nNum+1, by = "-1 day")
  b1 = b[length(b)]
  b2 = b[2]
  b3 = c(b1,b2)
  return(b3)
}





nextXDays = function(text)
{
  a = Sys.Date()
  num = str_extract(text,numbers)
  nNum = as.numeric(numLut[num])
  
  if(is.na(num))
  {
    num = as.numeric(str_extract(text,"[0-9]+"))
    nNum = num
  }
  # nNUm+1 to account for this month
  b = seq(as.Date(a), length = nNum+1, by = "+1 day")
  b1 = b[length(b)-1]
  b2 = b[1]
  b3 = c(b2,b1)
  return(b3)
}



lastXYears = function(text)
{
  a = Sys.Date()
  num = str_extract(text,numbers)
  nNum = as.numeric(numLut[num])
  
  if(is.na(num))
  {
    num = as.numeric(str_extract(text,"[0-9]+"))
    nNum = num
  }
  # nNUm+1 to account for this month
  b = seq(as.Date(a), length = nNum+1, by = "-1 year")
  b1 = b[length(b)-1]
  day(b1) = 01
  b2 = b[1]
  day(b2) = 01
  b3 = c(b1,b2)
  return(b3)
}


nextXYears = function(text)
{
  a = Sys.Date()
  num = str_extract(text,numbers)
  nNum = as.numeric(numLut[num])
  
  if(is.na(num))
  {
    num = as.numeric(str_extract(text,"[0-9]+"))
    nNum = num
  }
  # nNUm+1 to account for this month
  b = seq(as.Date(a), length = nNum+1, by = "+1 year")
  b1 = b[length(b)-1]
  day(b1) = 01
  b2 = b[1]
  day(b2) = 01
  b3 = c(b2,b1)
  return(b3)
}



## needed for the lastXWeeks and similar functions
## num is used yo construct pattern on grepl
#numModifiers = "next|last"   # not used 
#timeModifiers = "day|week|month|year"    # not used
#numNodifier = str_extract(text, numModifiers)# not used
#timeModifier = str_extract(text, timeModifiers)# not used


numbers = "\bone\b|\btwo\b|\bthree\b|\bfour\b|\bfive\b|\bsix\b|\bseven\b|\beight\b|\bnine\b|\bten\b"
numLut = c("one"=1, "two"=2, "three"=3, "four"=4, "five"=5, "six"=6, "seven"=7, "eight"=8, "nine"=9,"ten"=10, "thiry" = 30, "sixty"=60, "ninety"=90)

num = str_extract(text,numbers)
if(is.na(num))
{
  num = as.numeric(str_extract(text,"[0-9]+"))
}



##################################
#
#  gets the actual date(s) from the time functions above
#     - i.e. getDate("last month)
#         -funcction lastMonth will output actual date from last month
##################################


getDate = function(text)
{
textx = gsub(" ","",text)

## pat1-3 for identifying special cases where user dictates the actual date
pat1a <- '[0-9]{1,2}[/-]+[0-9]{1,2}[/-]+[0-9]{2}' #2/2/12
pat1b <- '[0-9]{1,2}[/-]+[0-9]{1,2}[/-]+[0-9]{4}'  #2/12/2014
pat1c = '[0-9]{4}[/-]+[0-9]{1,2}[/-]+[0-9]{2}' #2014/2/12

# january, 2014
pat2 = 'jan.*[,]?[0-9]{4}|feb.*[,][0-9]{4}|mar.*[,][0-9]{4}|apr.*[,][0-9]{4}|may.*[,][0-9]{4}|jun.*[,][0-9]{4}|jul.*[,][0-9]{4}|augu.*[,][0-9]{4}|sept.*[,]?[0-9]{4}|oct.*[,][0-9]{4}|nov.*[,][0-9]{4}|dec.*[,][0-9]{4}'

# january 12 ,2015
pat3 = 'jan.*[0-9]{1,4}|feb.*[0-9]{1,4}|mar.*[0-9]{1,4}|apr.*[0-9]{1,4}|may.*[0-9]{1,4}|jun.*[0-9]{1,4}|jul.*[0-9]{1,4}|sep.*[0-9]{1,4}|oct.*[0-9]{1,4}|nov.*[0-9]{1,4}|dec.*[0-9]{1,4}'

  
if (length(grep("today", text))>0)
{
  datex = today(text)     

  } else if (length(grep("tomorrow", text))>0)
{
  datex = tomorrow(text)     

  } else if (length(grep("yesterday", text))>0)
{
  datex = yesterday(text)     

  } else if (length(grep("thisweek", textx))>0)
{
  datex = thisWeek(text)     

  } else if (length(grep("nextweek", textx))>0)
{
  datex = nextWeek(text)     

  } else if (length(grep("lastweek", textx))>0)
{
  datex = lastWeek(text)     

  } else if (length(grep("lastmonth", textx))>0)
{
  datex = lastMonth(text)     

  } else if (length(grep("thismonth", textx))>0)
{
  datex = thisMonth(text)     

  } else if (length(grep("nextmonth", textx))>0)
{
  datex = nextMonth(text)     

  } else if (grepl("nextyear", textx))
  {
    datex = nextYear(text)     

    } else if (grepl("thisyear", textx))
  {
    datex = thisYear(text)     
  
    } else if (grepl("lastyear", textx))
  {
    datex = lastYear(text)   
  } else if(grepl(paste0("last[0-9]+year"), textx))
  {
    datex = lastXYears(text)   
  } else  if (grepl(paste0("[l|p]ast", num, "day"), textx))
  {
    datex = lastXDays(text)
    
  } else if(grepl(paste0("[l|p]ast", num, "week"), textx))
  {
    datex = lastXWeeks(text)
    
  } else if(grepl(paste0("last[0-9]+week"), textx))
  {
    datex = lastXWeeks(text)
  }
  else  if(grepl(paste0("[l|p]ast", num, "month"), textx))
  
  {
    datex = lastXMonths(text)
    
  } else if(grepl("last[0-9]+month", textx))
  {
    datex = lastXMonths(text)
  }
  else  if(grepl(paste0("[l|p]ast", num, "year"), textx))
  {
    datex = lastXYears(text)
    
  } else if(grepl("last[0-9]+day", textx))
  {
    datex = nextXDays(text)
    
  } else if(grepl("next[0-9]+day", textx))
  {
    datex = nextXDays(text)
  } else  if(grepl(paste0("next", num, "day"), textx))
  {
    datex = nextXDays(text)
    
  } else if(grepl("next[0-9]+week", textx))
  {
    datex = NextXWeeks(text)
  } else  if(grepl(paste0("next", num, "week"), textx))
  {
    datex = NextXWeeks(text)
    
  } else if(grepl("next[0-9]+month", textx))
  {
    datex = nextXMonths(text)
  } else  if(grepl(paste0("next", num, "month"), text))
  {
    datex = nextXMonths(text)
  } else if(grepl("next[0-9]+year", textx))
  {
    datex = nextXYears(text)
  } else if(grepl(paste0("next", num, "year"), textx))
  {
    datex = nextXYears(text)
  } else if (grepl(pat1a, textx))  #2/21/18 but also T for 2/22/2014
{
    datex1 = str_extract(textx, pat1a)
    datex2 = datex1[!is.na(datex1)]
    a = seq(mdy(datex2), length = 2, by = "-1 days")[2]
    b = seq(mdy(datex2), length = 2, by = "+1 days")[2]
    datex = c(a,b)

    } else if (grepl(pat1b, textx))
    {
      datex1 = str_extract(textx, pat1b)  # 2/22/2014
      datex2 = datex1[!is.na(datex1)]
      a = seq(mdy(datex2), length = 2, by = "-1 days")[2]
      b = seq(mdy(datex2), length = 2, by = "+1 days")[2]
      datex = c(a,b)
      
    }else if (grepl(pat1c, textx))
    {
      datex1 = str_extract(textx, pat1c)  #2014/2/12
      datex2 = datex1[!is.na(datex1)]
      a = seq(ymd(datex2), length = 2, by = "-1 days")[2]
      b = seq(ymd(datex2), length = 2, by = "+1 days")[2]
      datex = c(a,b)
      
    } else if (grepl(pat2, textx))   # doesn't work
{
    datex1 = str_extract(textx, pat2)
    datex2 = datex1[!is.na(datex1)]
    a = seq(mdy(datex2), length = 2, by = "-1 days")[2]
    b = seq(mdy(datex2), length = 2, by = "+1 days")[2]
    datex = c(a,b)
} else if (grepl(pat3, textx))
{
  datex1 = str_extract(textx, pat3)
  datex2 = datex1[!is.na(datex1)]
  a = seq(mdy(datex2), length = 2, by = "-1 days")[2]
  b = seq(mdy(datex2), length = 2, by = "+1 days")[2]
  datex = c(a,b)
} else
  {
    datex = lastYear(text)
  }
  return(datex)
}



##################################
#
#  check to see if date element in text
#  returns T if date elems present else F
#
##################################

checkForDate = function(text)
  {
  a = length(grep("janu|febru|marc|april|may|jun|jul|augus|sept|octob|novem|decem", text, ignore.case = T))>0
  
  return(a)
  }


#################################
### rotates first and second name
#################################

rotate  =function(x)
{
  s1 = unlist(str_split(x, ","))
  s3 = s1[2] 
  s4 = s1[1]
  str1 = paste(s3, s4)
  return(str1)
}




### extracts name elements from chart names in mdi
extractNameFromChart = function(n)
{
  a = unlist(str_split(n, "_"))
  b = length(a)
  c = b-1
  d = a[-c(c:b)]
  named = list()
  
  for(i in 1:length(d))
  {
    named[[i]] = paste(d[i], d[i-1])
  }
  named1 = unlist(do.call("rbind", named)[-1])
  return(toString(named1))
}






####################################
## searches for character in all of the column
## used in searching for appt date in all columns in schedule.R
####################################

searcher = function(x)
{
  co = ncol(x)
  for(j in 1:co)
  {
    a = grep(x, unlist(data[,j]))
    if (length(a)>0)
    {
      loc = grep(x, unlist(data[,j]), ignore.case = T)
      loc1 = c(loc, loc1)
    }
    k = k +1
    #print(loc1)
  }
  loc2= unique(unlist(loc1))
  return(loc2)
}







searcher1 = function(tern, df)
{
  co = ncol(x)
  for(j in 1:co)
  {
    a = grep(x, unlist(data[,j]))
    if (length(a)>0)
    {
      loc = grep(x, unlist(data[,j]), ignore.case = T)
      loc1 = c(loc, loc1)
    }
    k = k +1
    #print(loc1)
  }
  loc2= unique(unlist(loc1))
  return(loc2)
}




####################################
### function to clean date for gmailR
####################################

dateClean = function(text)
{
  months = read.csv("~/Desktop/gmailr/months.csv")
  
  monthsID = "January|February|March|April|May|Jun|July|August|September|October|November|December"
  monthName = tolower(str_extract(tolower(text), tolower(monthsID)))
  month = filter(months, grepl(monthName, months$month))[,"monthNum"]
  month = as.numeric(month)
  month1 = month #defined from text
  
  
  today1 = Sys.Date()
  monthToday = month(today1)
  year = year(today1)
  year1 = year-1
  
  
  # if month is 1 then subtract year and change month to dec
  if (month1==0)
  {
    year = year-1
    year1 = year-1 #
    month1 = 12
  }
  
  # if month has not happened this year, then change year to last year  
  if (monthToday-month1<0)
  {
    year = year-1
    year1 = year-1 #
  }
  
  
  # determine days in a month
  if (month1 %in% c(1,3,5,7,10,12))
  {
    endOfMonth = 31
  } else
  {
    endOfMonth = 30
  }
  
  # accomodate for feb
  if (month1=="2")
  {
    endOfMonth = 28
  }
  
  # add leading 0 to single character months (1-9)
  if (nchar(month1)==1)
  {
    month1 = paste0(0, month1)
  }
  
  
  return(c(month1, year, year1, endOfMonth))
}



