A77 = read.csv("A77.csv")
A80 = read.csv("A80.csv")
ASE = read.csv("ASE.csv")
BSE = read.csv("BSE.csv")
A77 = A77[,-1]
A80 = A80[,-1]
ASE = ASE[,-1]
BSE = BSE[,-1]
colnames(A77) = c(1,2,3,4,5)
colnames(A80) = c(1,2,3,4,5)
colnames(ASE) = c(1,2,3)
colnames(BSE) = c(1,2,3,4,5)
rownames(A77) = c(0,1,2,3,4,5)
rownames(A80) = c(0,1,2,3,4,5)
rownames(ASE) = c(0,1,2,3)
rownames(BSE) = c(0,1,2,3,4,5)

build.w = function(data){
  n = apply(type, 2, sum, na.rm=TRUE)
  
}