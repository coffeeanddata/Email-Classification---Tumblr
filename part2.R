setwd("~/Dropbox/Data_Science/R Projects/email project/")
library(stringr)
library(ggplot2)
load('TrainingMessages.rda')


#List of functions
normalization = function(dat, paraData = dat) {
  paraData = paraData[!is.na(paraData)]
  paraMean = mean(paraData)
  paraSD = sd(paraData)
  (dat-paraMean)/paraSD
}


#Spam or Ham
SpamTrue = grepl('SpamAssassinTraining/spam',names(emailData))

#easy vs hard
easy_hard = gsub('.*SpamAssassinTraining/',"", names(emailData))
easy_hard = gsub("/.*", "", easy_hard)

numRecipients = sapply(emailData,function(x) str_count(x$header$To, '@') )
numRecipients = unlist(ifelse(numRecipients == 0, 1, numRecipients) )

#html files
htmlFiles = sapply(emailData, function(x) 
  sum(grepl("</html>", c(x$body, x$attachments), ignore.case = TRUE)) )

htmlFiles = ifelse(htmlFiles >0, TRUE, FALSE)


getNull = sapply(emailData, function(x) is.null(x$header$Subject) )
ReSubject = sapply(emailData, function(x)  grepl("Re:", x$header$Subject))
ReSubject[getNull] = F
ReSubject = unlist(ReSubject)

first_str =
  "viagra|pounds|free|guarantee|million|dollar|credit|risk|FINANCIAL|Proposal|debt|"
second_str = 
  "prescription|generic|drug|money|back|card|Paid|weight|loss|FREEDOM|Investment|pay|"
third_str =
  "MORTGAGE|Win|FUTURE|Save|Life|\\$|!"

main_str = paste0(c(first_str,second_str,third_str), collapse ="")
findSpamWords = sapply(emailData, function(x) 
  grepl(main_str, x$header$Subject, ignore.case = T))
findSpamWords[getNull] = NA
findSpamWords = unlist(findSpamWords)


#Percentage of words captialized
perCap = function(rawemail){
  rawbody = rawemail$body #finds body
  x = str_count(rawbody, '[a-z]|[A-Z]')
  #checks is lenght of body(only letters) != 0
  if (x != 0){
    y = str_count(rawbody, '[A-Z]') #checks if any of capitalze
    y/x  #finds the ratio of cap/lower
  }else{ 0} #if no body or no length in body
}


percentageCap = sapply(emailData, function(x) mean(perCap(x)))
#percentageCap = normalization(percentageCap)
emailFrame = data.frame(isSpam = SpamTrue, 
                        NR = numRecipients, 
                        isRe = ReSubject,
                        SpamWords = findSpamWords,
                        findHTML = htmlFiles,
                        PerCapWords = percentageCap)
rownames(emailFrame) = 1:nrow(emailFrame)


head(emailFrame,10)
save(emailFrame, file = "emailFrame.rda")

permuateData = sample(1:nrow(emailFrame))
emailFrame = emailFrame[permuateData,]

head(emailFrame)

ggplotTheme = theme(axis.text = element_text(colour = 'black',face = c('bold'),size = 9),
                    title = element_text(face = 'bold',family = 'Helvetica'))
m = ggplot(emailFrame)+ ggplotTheme+
  guides(fill=guide_legend(title="is the Email Spam?"),
         colour = guide_legend(title = "is the Email Spam?"))


#####

m+geom_point(aes(1:nrow(emailFrame),PerCapWords, colour = factor(isSpam)))+
  labs(title = "Capitalization to Lowercase Ratio Scatter Plot",
       x = 'Email', y = 'Capitalized / Lowercase')


#####
m+geom_point(aes(NR,PerCapWords, colour = factor(isSpam)))+
  labs(title = 'Ratio of Capital to Lower vs. Number of Recipients  Scatter Plot',
       x = 'Number of Recipients', y = "Capitalization to Lowercase Ratio" )
  


####
m+geom_bar(aes(PerCapWords, fill = factor(isSpam)))+scale_fill_brewer(palette = 11)+
  labs(title =  "Capitalization to Lowercase Ratio Bar Plot",
       x = "Capitalization to Lowercase Ratio")
  
m2 = ggplot(emailFrame[emailFrame$PerCapWords >.12,])+ggplotTheme
m2+geom_bar(aes(PerCapWords, fill = factor(isSpam)))+scale_fill_brewer(palette = 11)+
  labs(title ="Capitalto to Lowercase Ratio (Ratio Greater than .12) Bar Plot",
       x = "Capitalization to Lowercase Ratio")





m+geom_bar(aes(isRe, fill = factor(isSpam)))+scale_fill_brewer(palette = 18)+
  labs(title ="Did Subject contain a Re: (Reply) Bar Plot",
       x = 'is the Email a reply?')


m+geom_bar(aes(findHTML, fill = factor(isSpam)))+scale_fill_brewer(palette = 18)+
  labs(title = 'Does the email contain HTML Content bar Plot',
       xlab = 'HTML email?')



