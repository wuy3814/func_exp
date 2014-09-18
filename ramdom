## A failed attempt
b<-matrix(nrow=12, ncol=7)
for (i in 1:12) {
  pool<-1:28
  if (i==1){b[i,]<-sample(pool,7) }
  else {
    z<-as.matrix(as.data.frame(table(b)))
    z<-matrix(as.numeric(z),ncol=2)
    test<-z[z[,2]==3,]
    test<-matrix(test, ncol=2)
    if (nrow(test)==0) {b[i,]<-sample(pool, 7)}
    else {b[i,]<-sample(pool[!(pool %in% test)],7)} 
   }}
## this method proves not viable

# The easiest way!
# 28*3/7=12 hence the 12 viewer
# or for a whole list 28, 7 is a factor, meaning that it takes 4 persons to finish
# a round, each viewing 7. This multiplied by total 3 times, then 12 persons in need
total<-28
per<-7
a<-sample(1:total)
b<-sample(1:total)
c<-sample(1:total)
d<-c(a,b,c)
z<-t(matrix(d, nrow=per))


# Fu Dongqin Issue: restructure the vector of observation so that it may constitute
# a matrix. Suppose a is the input vector
a<-read.table("s.txt")
a<-as.matrix(a)
b<-t(matrix(a, nrow=6))
write.csv(b,"output.csv")
