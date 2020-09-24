M<-2 #scaling
n<-10000 #number of iterations
final_sample<-rep(NA,n)
accept<-0
while (accept<n){
    y<-runif(1,0,pi/2) #get one random number between 0 and pi/2 as the value to input in the functions
    u<-runif(1,0,1)  #another random number between 0 and 1 to compare if it is larger or smaller than y
    if (M*u<=sin(y)){  #comparison, if the value of M*u is less or equal than the target distribution, store it

        accept<-accept+1 #if the value falls under the desired pdf, increase the counter by 1
        final_sample[accept]<-y #store the accepted value

    }
}
p<-seq(from=0,to=pi/2,0.01)
w<-sin(p)

hist(final_sample,freq=F)
lines(p,w,col="red")

#######

#http://www.ericfrazerlock.com/Rejection_Sampling_Rcode1.r
#http://ericfrazerlock.com/Rejection_Sampling.pdf
#https://datasciencechalktalk.com/2019/09/22/understanding-rejection-sampling-method/
#https://www.di.fc.ul.pt/~jpn/r/PRML/chapter11.html

M<-2 #envelope
n<-10000 #number of iterations
final_sample<-rep(NA,n)
accept<-0
for (i in 1:n){
    y<-runif(1,0,pi/2) #get one random number between 0 and pi/2 as the value to input in the functions
    u<-runif(1)  #another random number between 0 and 1 to compare if it is larger or smaller than y
    if (u<=sin(y)/M*1){

        accept<-accept+1 #if the value falls under the desired pdf, increase the counter by 1
        final_sample[accept]<-y #store the accepted value

    }
}
p<-seq(from=0,to=pi/2,0.01)
w<-sin(p)

hist(final_sample,freq=F)
lines(p,w,col="red")
cat("Efficiency is",accept/n)

##############
rejection <- function(f, M, g, rg,n) {
    naccepts <- 0
    result.sample <- rep(NA, n)

    while (naccepts < n) {
        y <- rg(1)
        u <- runif(1)

        if ( u <= f(y) / (M*g(y) )) {
            naccepts <- naccepts + 1
            result.sample[naccepts] = y
        }
    }

    result.sample
}

f  <- function(x) sin(x)     # pdf of Beta(2,2), maximum density is 1.5
g  <- function(x) x/x           # g(x) = 1 but in vectorized version
rg <- function(n) runif(n,0,pi/2)  # uniform, in this case
M  <- 1

result <- rejection(f, M, g,rg, 10000)

print(length(result[result <= 0.8])/10000)
hist(result,freq=FALSE)
lines(y,sin(y),type="l")

