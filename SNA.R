# change the file directory in line: 129

library(readr)
library(stringr)
library(sna)

###############################################################################################################
# Open File Folders and Extract Data From Folders                                                             #                   #
###############################################################################################################


#### Function to open maildir, iterate over people and extract data from folders then store data in someplace.
myfunction <- function(filename){
    ## TF is main absolute direction
    TF <-  filename
    ## list.file return the subfolder name inside the TF(main folder)
    SF <- list.files(TF,recursive=T)
    length(SF);
    
    ## initializeing the variables
    messageid <- c()
    date <- c()
    from <- c()
    to <- c()
    subject <- c()
    cc <- c()
    
    ## Go through all folders and store the data
    for (i in 1:length(SF)) {
        ## Iterated build all subdirection
        direction <- paste(TF,SF[i],sep="/")
        ## Filter the file name is not "1" 
        if (gregexpr("/1$", SF[i])[[1]][1] == -1) {
            next
        }
        
        ## Read one email from the file by given direction   
        contents <- read_file(direction)
        
        ########### Delete the emails which without "To" #######
        
        position.from <- gregexpr("\r\nFrom:", contents)[[1]][1]
        position.to <- gregexpr("\r\nTo", contents)[[1]][1]
        position.subject <- gregexpr("\r\nSubject:", contents)[[1]][1]
        
        ## if "To" is between "From" and "Subject", then store. Otherwise, value for this email is NA
        if (position.to > position.from & position.to <position.subject)   
        {
            
            ## Store MessageID into messageid by caculating the position
            initial.position.messageid <- gregexpr("Message-ID: <", contents)[[1]][1] + nchar("Message-ID: <")
            final.position.messageid <-  gregexpr(">\r\nDate", contents)[[1]][1] - 1
            messageid[i] <- substr(contents,initial.position.messageid, final.position.messageid)
            
            ## Store Date into date
            initial.position.date <- gregexpr("Date: ", contents)[[1]][1] + nchar("Date: ")
            final.position.date <-  gregexpr("\r\nFrom:", contents)[[1]][1] - 1
            date[i] <- substr(contents,initial.position.date, final.position.date)
            
            
            
            ## Store From into from
            initial.position.from <- gregexpr("From: ", contents)[[1]][1] + nchar("From: ")
            final.position.from <-  gregexpr("\r\nTo", contents)[[1]][1] - 1
            from[i] <- substr(contents,initial.position.from, final.position.from)
            
            
            ## Store To into to
            initial.position.to <- gregexpr("\r\nTo: ", contents)[[1]][1] + nchar("\r\nTo: ")
            final.position.to <-  gregexpr("\r\nSubject:", contents)[[1]][1] - 1
            to[i] <- substr(contents,initial.position.to, final.position.to)
            
            
            
            ## Store Subject into subject. The position of subject can be located by either before "Cc" or before "Mime-Version" 
            if (gregexpr("\r\nCc:", contents)[[1]][1] == -1)
            {
                ## Subject
                ## Locate the subject by "Mime-Version" 
                initial.position.subject <- gregexpr("\r\nSubject: ", contents)[[1]][1] + nchar("\r\nSubject: ")
                final.position.subject <-  gregexpr("Mime-Version:", contents)[[1]][1] - 1
                subject[i] <- substr(contents,initial.position.subject, final.position.subject)
                
                ## Cc
                ## if there is no Cc next to subject, store the Cc as -1
                cc[i] <- -1
            } else  {
                ## Subject
                ## if there is Cc after subject, store Cc and locate subject by the location of Cc
                initial.position.subject <- gregexpr("\r\nSubject: ", contents)[[1]][1] + nchar("\r\nSubject: ")
                final.position.subject <-  gregexpr("\r\nCc:", contents)[[1]][1] - 1
                subject[i] <- substr(contents,initial.position.subject, final.position.subject)
                
                ## Store Cc
                initial.position.cc <- gregexpr("\r\nCc: ", contents)[[1]][1] + nchar("\r\nCc: ")
                final.position.cc <-  gregexpr("\r\nMime-Version:", contents)[[1]][1] - 1
                cc[i] <- substr(contents,initial.position.cc, final.position.cc)
            } 
            
        }
        
        ## Filter the email which does not have "To"
        else{
            next
        } 
        
    } 
    ## for loop go through every email end
    
    ## Delete the rows' value are NA (No "To") 
    nalist = which(is.na(messageid)==TRUE)
    from = from[-nalist]
    to = to[-nalist]
    messageid = messageid[-nalist]
    date = date[-nalist]
    subject = subject[-nalist]
    
    ## Create a master table to store all names, messages, dates, subjects from email subsets
    userTable0 = data.frame(messageid,date,subject,to,from,stringsAsFactors = FALSE)
    
    
    return(userTable0)
}

## Call the function to extracte and store data into userTable

userTable=myfunction(filename="~/Documents/15fall/6907_bigdata_analysis/Homework1/EmailSubset/maildir")
subject=c()
for (i in 1:nrow(as.matrix(userTable$subject))){
    if (gregexpr("Mime-Version:", userTable$subject[i])[[1]][1] == -1){
        subject[i]=userTable$subject[i]
    } else {
        final.position.subject <-  gregexpr("Mime-Version:", userTable$subject[i])[[1]][1] - 1
        subject[i] <- substr(userTable$subject[i],1, final.position.subject)
    }
}
write.table(subject, "~/Documents/Dataincubator/emailsubject.txt", sep="\t")

###############################################################################################################
# Processiong of cleaning the data and storing them into one matrix                                           #                         #                   #
###############################################################################################################

num=1
messageid2=c()
date2=c()
from2=c()
to2=c()
subject2=c()

## If messageid, date, from, subject, to have multiple receipients, split them and store them seperatedly into multiple rows
for (y in 1:nrow(userTable))
{
    
    for (x in 1:length(strsplit(userTable$to[y],",")[[1]]))
    {
        messageid2[num] = userTable$messageid[y]
        date2[num]= userTable$date[y]
        from2[num]=userTable$from[y]
        subject2[num]=userTable$subject[y]
        to2[num]=str_trim(strsplit(userTable$to[y],",")[[1]][x])
        num = num+1
    }
}

## Create usertable2 to store the processed data
userTable2 = data.frame(messageid2,date2,from2,to2,subject2)


## Delete email which does not end with "@enron.com"

q = 1
noenron = c()
for (p in 1:nrow(userTable2))
{
    
    if (gregexpr("@enron.com",userTable2$from2[p]) == -1 || gregexpr("@enron.com",userTable2$to2[p]) == -1 )
    {
        noenron[q] = p
        q = q+1
    }
}

## Sotre the filtered data into userTable3
userTable3 = userTable2[-noenron,]




## Remove duplicate emails, get unique names of all email lists
allEmails= append(as.vector(userTable3$to2),as.vector(userTable3$from2))
uniqueNames = unique(allEmails)


## Change the way of name displayed : only keep the names 
for (t in 1:length(uniqueNames)) {
    
    uniqueNames[t] = gsub("@enron.com","",uniqueNames[t])
}


## Create empty matrix with all usernames 
mat <- matrix(data=0,nrow=length(uniqueNames),ncol=length(uniqueNames), dimnames = list(uniqueNames,uniqueNames))

## Iterate through userTable3 and find from -> to communications
## Column 3 of the userTable3 dataframe are the "From's"
## Column 4 of the userTable3 dataframe are the "To's"
## For each row, if A emailed to B, the value for AB=1. 
## If B emailed to A, the value for BA=1
for (z in 1:nrow(userTable3)) {
    
    matfrom = gsub("@enron.com","",toString(userTable3[z,3]))
    matto = gsub("@enron.com","",toString(userTable3[z,4]))
    if (matfrom[1] != matto[1]) { 
        mat[matfrom[1],matto[1]] = 1 
    }
}


###############################################################################################################
##  calculation for key values ##
###############################################################################################################


## degree: number of connection
## Sum the outdegree for every people in the matrix
mat.outdegree <- rowSums(mat)
## other way to calculate outdegree
## mat.outdegree2 = degree(mat, cmode="outdegree")

## Sum the indegree for every people in the matrix
mat.indegree <- colSums(mat)

## other way to calculate indegree
## mat.indegree2 <- degree(mat, cmode="indegree")

## save the data into file "inoutdegree.csv"
mat.inoutdegree <- cbind(mat.indegree, mat.outdegree)
## write.table(mat.inoutdegree,"~/Documents/6907_bigdata_analysis/Homework1/inoutdegree.csv",sep = ",")

##############################################################################
## maximum path length
mat.maximumpath = mat %*% mat

## find the maximum length of path
max(mat.maximumpath)
which(mat.maximumpath == max(mat.maximumpath), arr.ind = TRUE)
uniqueNames[1325]

## save the data into file "maximum_path.csv"
#write.table(mat.maximumpath,file = "~/Documents/6907_bigdata_analysis/Homework1/maximum_path.csv", sep = ",")

## see  "find max of shortest path" at the end
##############################################################################

## betweenness:number of shortest paths a person on
## by default the diagonal should not be treated as valid data.
mat.betweenness = round(as.matrix(betweenness(mat)),0)
rownames(mat.betweenness) = uniqueNames
mat.betweenness[which.max(mat.betweenness),]

## save the data into file "betweenness.csv"
## write.table(mat.betweenness,file = "~/Documents/6907_bigdata_analysis/Homework1/betweenness.csv", sep = ",")


## prestige:Calculate the Vertex Prestige Scores
## cmode: "indegree"(default), "indegree.rownorm", "indegree.rowcolnorm", 
## "eigenvector", "eigenvector.rownorm", "eigenvector.colnorm", "eigenvector.rowcolnorm", 
## "domain", or "domain.proximity".
mat.prestige = as.matrix(prestige(mat,cmode="domain"))
rownames(mat.prestige) = uniqueNames
mat.prestige[which.max(mat.prestige),]
## save the data into file "prestige.csv"
## write.table(mat.prestige,file = "~/Documents/6907_bigdata_analysis/Homework1/prestige.csv", sep = ",")


## centrality
## eigenvector centrality: leading eigenvector of sociaomatrix
eigencentrality = as.matrix(evcent(mat,rescale = TRUE))
View(eigencentrality)
## central person in the graph
rownames(eigencentrality) = uniqueNames
eigencentrality[which.max(eigencentrality),]

## save the data into file "eigencentrality.csv"
## write.table(eigencentrality,file = "~/Documents/6907_bigdata_analysis/Homework1/eigencentrality.csv", sep = ",")

###############################################################################################################
# Create graph based on centralityvalues                                                                      #          #
###############################################################################################################


gplot(mat, displaylabels = TRUE)

detach("package:sna", unload=TRUE)
library(igraph)

graph_data = userTable3[,3:4]
graph_1 = graph.data.frame(graph_data, directed = TRUE)
# Make sure each of the 2 columns is read as a character
graph_data$to = as.character(graph_data$from2)
graph_data$from = as.character(graph_data$to2)

net=graph.adjacency(mat,mode="directed",weighted=TRUE,diag=FALSE) #the only difference between this and the weighted network code is that mode="directed"

plot.igraph(net,
            vertex.size=5,
            vertex.color="red",
            vertex.label=V(net)$name,
            vertex.label.degree=pi/2,
            vertex.label.cex=1, 
            layout=layout.fruchterman.reingold, 
            vertex.label.color="black",
            edge.color="black",
            edge.width=E(net)$weight/3, 
            edge.arrow.size=0.05,
            edge.arrow.width=1)


##  find max of shortest path
mat.shorestpath=shortest.paths(net,v = V(net),to = V(net),mode = "all",weights = NULL)
mat.shorestpath=replace(mat.shorestpath,mat.shorestpath==Inf,NA)
which(mat.shorestpath == max(mat.shorestpath[which(!is.na(mat.shorestpath), TRUE)]), arr.ind = TRUE)

detach("package:igraph", unload=TRUE)
