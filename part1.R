setwd("~/Dropbox/Data_Science/R Projects/email project/")

#Getting files
LocName = "SpamAssassinTraining"
FolderLoc = list.files(LocName, full.names = TRUE)
file_location = list.files(FolderLoc, full.names = TRUE)


#This function input: location of a specfic email
OpeningEmail = function(Loc) {
  openFile = file(Loc, open = 'rt')
  #open and read file
  ReadFile = readLines(openFile)
  close(openFile)
  #enter the file context to analyze
  enterFile(ReadFile)
}


#Input will be the opened file context
enterFile  = function(openedFile){
  #find the first space
  #many automated emails "first blank space" will be the ending of the header
  first_space = which(openedFile == "" )[1]
  #here if we dont find space, we just return back the file and preserve its context
  if(is.na(first_space)) {
    returnItem = list(header = list(noName =  "no header"),
                      body = openedFile, 
                      attachments = list(noName = 'no attachments'))
    return(returnItem)
  }

  
  NotHeader = openedFile[first_space: length(openedFile)]
  #separate everything from header
  rawheader = openedFile[1:first_space-1]
  
  #Because header has a lot of important contect
  #we can open a connection and structure it automatically
  #in a format we can analyze it using text conntection
  rawConnection = textConnection(rawheader)
  file_header = read.dcf(rawConnection, all = TRUE)
  close(rawConnection) 
  
  #find whether it will have body
  Check_Context(file_header,NotHeader)
  
}

#input: header and eveyrthing other than header
Check_Context = function(Header, not_header){
  #find key element in header called content type
  ContType = Header$'Content-Type'
  #if we have boundary we might have multiple parts in the email
  #like body and attachments
  anyBoundary = grepl("boundary=",ContType,ignore.case = T )
  if(length(anyBoundary) == 0) anyBoundary = FALSE
  
  #if we have any boundary, lets separate this data into specific content type
  if(anyBoundary){
    #emails have keys that separate content type, so lets find it!
    Key = gsub("^.*boundary=|\"" ,"",Header$'Content-Type')
    
    #call the function that will find our attachments
    tempItem = getAttachments(Key, not_header)
    
    #obtaining our raw body and raw attachments
    rawBody = tempItem$Context[tempItem$findBody]
    rawAttachments = tempItem$Context[!tempItem$findBody]
    
    #cleaning up our body into on big string
    MessageBody = paste0(rawBody, collapse = "\n")
    #this will be returned: header, body, and attachments
    list(header = Header, body = MessageBody, attachments =  rawAttachments)
  }
  else{
    #if no boundary key, then just return the rest as a body
   MessageBody = paste0(not_header, collapse = "\n")
   #if there isn't any key, then we return a body and header
   list(header = Header, body = MessageBody)
  }
}

#input: boundary key (if any), and content
getAttachments = function(BoundaryKey,Content){
  #find where our key (if any) is lcoated in the content
  find_key_in_Content = grepl(BoundaryKey, Content)
  #cumsum is great to split data based on specific (TRUE and FALSE)
  #our true will be where it finds our Key
  #so for every key we will split that content into another list element
  splitContent = split(Content,cumsum(find_key_in_Content))
  
  #finding our email boy
  bodyContent = grepl("content-Type.*text/plain", splitContent, ignore.case = TRUE)
  
  list(Context = splitContent, findBody = bodyContent)
}

emailData = sapply(file_location, OpeningEmail)
save(emailData, file = "TrainingMessages.rda")
