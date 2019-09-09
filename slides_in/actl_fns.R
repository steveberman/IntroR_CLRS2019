# actl_fns.R
# basic triangular manipulations


### function: lastDiag
### purpose: get last diagonal of a triangle, in vector form
### this is done by taking the tail non-NA value for each row
lastDiag <- function(tri) {
  apply(tri, MARGIN=1, FUN=function(x) tail(na.omit(x),1))
}


### function: ataTri
### purpose: get age-to-age triangle based on an input triangle
### this is done by taking the tail non-NA value for each row
ataTri <- function(tri) {
  sapply(2:ncol(tri), function(x) tri[,x]/tri[,x-1])
}

### function: avgStr
### purpose: get unweighted A-A all year avg
avgStr <- function(aaTri) {
  apply(aaTri, MARGIN=2, FUN=mean, na.rm=TRUE)
}

### function: avgWtd
### purpose: get weighted A-A on a loss triangle
avgWtd <- function(tri) {
  aa <- ataTri(tri)
  tri_x_last <- tri[, 1:(ncol(tri)-1)]
  wtd <- aa * tri_x_last
  # mask for values to consider
  wtd_cell <- (!is.na(wtd))*1
  return(apply(wtd, MARGIN=2, FUN=sum, na.rm=TRUE) / 
           apply(wtd_cell * tri_x_last, MARGIN=2, FUN=sum, na.rm=TRUE))
}


tri <- insurer43$rptdTri

print(lastDiag(tri))
aa <-ataTri(tri)
print(aa)
print(avgStr(aa))

tri
avgWtd(tri)
