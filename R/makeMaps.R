makeMaps <- function(chipname1, chipname2){
  require(paste(chipname1,"probe",sep=""), character.only=TRUE)
  require(paste(chipname2,"probe",sep=""), character.only=TRUE)

  probe1 <- get(paste(chipname1,"probe", sep=""))
  seq1 <- probe1$sequence
  index1 <- xy2indices(probe1$x, probe1$y, cdf=paste(chipname1,"cdf",sep=""))

  probe2 <- get(paste(chipname2,"probe", sep=""))
  seq2 <- probe2$sequence
  index2 <- xy2indices(probe2$x, probe2$y, cdf=paste(chipname2,"cdf",sep=""))

  Index <- match(seq1,seq2)
  Index1 <- which(!is.na(Index))
  Index2 <- Index[Index1]

  chipmap <- cbind(index1[Index1], index2[Index2])
  colnames(chipmap) <- c(chipname1, chipname2)
  return(chipmap)
}
