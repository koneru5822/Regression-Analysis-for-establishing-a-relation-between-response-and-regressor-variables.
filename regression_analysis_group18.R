#Reaing the data
data1=read.csv("Salaries.csv")

#Slicing the data for the year 2014
data=data1[which(data1$Year>=2014),]

# CONVERTING CHAR TO NUMERIC DATA
data$OvertimePay=as.numeric(data$OvertimePay)
data$OtherPay=as.numeric(data$OtherPay)
data$Benefits=as.numeric(data$Benefits)
data$BasePay=as.numeric(data$BasePay)

#DEALING WITH MISSING VALUES
data[data <= 0] <- NA #Converting all zeroes to NA
ompleterecords <- na.omit(data)  
data=data[complete.cases(data[ , 4:7]),] #Deleting all the rows that have no data in specified columns

#INITIALIZING REGRESSOR AND RESPONSE 
regressor=data$OtherPay+data$OvertimePay+data$Benefits
response=data$BasePay

#DELETING OUTLINERS
temp1=!regressor %in% boxplot.stats(regressor)$out
regressor<- regressor[temp1]
response<- response[temp1]

temp2=!response %in% boxplot.stats(response)$out
regressor<- regressor[temp2]
response<- response[temp2]


#NORMALISATION
x=regressor
regressor=((x- min(x)) /(max(x)-min(x)))
x=response
response=((x- min(x)) /(max(x)-min(x)))


#CALCULATIONS
n=length(regressor)

y_mean=0
x_mean=0
for(i in 1:n){
  x_mean=x_mean+regressor[i]
  y_mean=y_mean+response[i]
}
y_mean=y_mean/n
x_mean=x_mean/n

y=mean(response)
x=mean(regressor)
 
Sxy=0
Sxx=0
Syy=0
x_2=0
sigma_xy=0
sigma_x2y=0
sigma_x3y=0
sigma_x_3=0
sigma_x_4=0
sigma_x_5=0
sigma_x_6=0
for(i in 1:length(regressor)){
  Sxy=Sxy+(regressor[i]-x_mean)*(response[i]-y_mean)
  Sxx=Sxx+(regressor[i]-x_mean)*(regressor[i]-x_mean)
  Syy=Syy+(response[i]-y_mean)*(response[i]-y_mean)
  x_2=x_2+regressor[i]*regressor[i]
  sigma_xy=sigma_xy+regressor[i]*response[i]
  sigma_x2y=sigma_x2y+regressor[i]*regressor[i]*response[i]
  sigma_x3y=sigma_x3y+regressor[i]*regressor[i]*regressor[i]*response[i]
  sigma_x_3=sigma_x_3+(regressor[i]*regressor[i]*regressor[i])
  sigma_x_4=sigma_x_4+(regressor[i]*regressor[i]*regressor[i]*regressor[i])
  sigma_x_5=sigma_x_5+(regressor[i]**5)
  sigma_x_6=sigma_x_6+(regressor[i]**6)
}


#SIMPLE LINEAR REGRESSION 

b=Sxy/Sxx
a=y_mean-b*x_mean

y_pred=a+b*regressor
 
 
#relation = lm(response~regressor)
#print(summary(relation))

error=response-y_pred
SSE=0
SST=0
t=response-y_mean

SSE_temp=Syy-((Sxy*Sxy)/Sxx)
for(i in 1:length(error)){
  SSE=SSE+(error[i]*error[i])
  
  SST=SST+(t[i]*t[i])
}

R_sq=1-(SSE_temp/SST)

#relation = lm(response~regressor)
#print(relation)
#print(summary(relation))

#print(summary(error))


#SIMPLE NON-REGRESSION WITH DEGREE 2

z=matrix(c(n,x_mean*n,x_2,
           x_mean*n,x_2,sigma_x_3,
           x_2,sigma_x_3,sigma_x_4),3,3,byrow=TRUE)
z2=c(y_mean*n,sigma_xy,sigma_x2y)

z_inv=solve(z)
sol=z_inv %*% z2

y_pred_2=sol[1]+(sol[2]*regressor)+(sol[3]*(regressor**2))

error=response-y_pred_2
SSE=0
SST=0
t=response-y_mean

for(i in 1:length(error)){
  SSE=SSE+(error[i]*error[i])
  
  SST=SST+(t[i]*t[i])
}

R_sq_2=1-(SSE/SST)
print(R_sq_2)
#x=lm(response ~ poly(regressor, 2, raw = TRUE))
#print(x)
#print(summary(x))


#SIMPLE NON-REGRESSION WITH DEGREE 3
z=matrix(c(n,x_mean*n,x_2,sigma_x_3,
           x_mean*n,x_2,sigma_x_3,sigma_x_4,
           x_2,sigma_x_3,sigma_x_4,sigma_x_5,
           sigma_x_3,sigma_x_4,sigma_x_5,sigma_x_6),4,4,byrow=TRUE)
z2=c(y_mean*n,sigma_xy,sigma_x2y,sigma_x3y)

z_inv=solve(z)

sol=z_inv %*% z2

y_pred_3=sol[1]+(sol[2]*regressor)+(sol[3]*(regressor**2))+(sol[4]*(regressor**3))

error=response-y_pred_3
SSE=0
SST=0
t=response-y_mean

for(i in 1:length(error)){
  SSE=SSE+(error[i]*error[i])
  
  SST=SST+(t[i]*t[i])
}

R_sq_3=1-(SSE/SST)

print(R_sq_3)
#x=lm(response ~ poly(regressor, 3, raw = TRUE))
#print(x)
#print(summary(x))


#LINEAR REGRESSION
#scatter.smooth(data$BasePay,data$OtherPay,xlab="BasePay",ylab="OvertimePay")
#plot(density(data$OtherPay), main='Density Plot: Otherpay',ylab='Frequency', sub=paste((data$OtherPay)))

#plot(density(data$OvertimePay), main='Density Plot: Overtime pay',ylab='Frequency', sub=paste((data$OvertimePay)))
#plot(density(data$Benefits), main='Density Plot: Benefits',ylab='Frequency', sub=paste((data$Benefits)))
plot(density(regressor), main='Density Plot: Regressor',ylab='Frequency', sub=paste((regressor)))
scatter.smooth(regressor)
#boxplot(regressor,main='Regressor_', sub=paste('Outliner rows: '))
#boxplot(response,main='Response_', sub=paste('Outliner rows: '))

plot(regressor,y_pred,xlab="BasePay",ylab="Total")
plot(regressor,y_pred_2,xlab="BasePay",ylab="Total")

plot(regressor,y_pred_3,xlab="BasePay",ylab="Total")


