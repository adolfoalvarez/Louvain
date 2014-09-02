initialnodes <- rownames(A)
nodes <- data.frame(nodes = initialnodes, rows= 1:length(initialnodes), community=1:length(initialnodes))
mylist <- split(nodes$rows,f=nodes$community) #At the beginning we have as communities as nodes
i <- 1
j <- 1
com <- mylist[[j]]

myfun <- function(com, A=A,step=i){
  Sin <- sum(A[union(i,com),union(i,com)], na.rm=T)
  Stot <- sum(A[,union(i,com)], na.rm=T)
  ki <- sum(A[i,], na.rm=T)
  kiin <- sum(A[i,com], na.rm=T)
  m <- sum(A, na.rm=T)  
  dq1 <- (((Sin+kiin)/2*m)-((Stot+ki)/(2*m))^2)
  dq2 <- Sin/(2*m)-(Stot/(2*m))^2-(ki/(2*m))^2
  dq <- dq1-dq2  
  return(dq)
}

which.max(sapply(mylist,myfun, A=A, step=i)) #node i joins to community j