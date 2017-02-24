## Starting 04-02-2017
## Model we have y(t) = 1.5 + 2.0t + epsilon_(t);
 
## Function for LSE will return least Sqaure Estimates 
lse=function(dsgn_mat,resp_var){
  if(!is.matrix(dsgn_mat)){
    dsgn_mat=as.matrix(dsgn_mat)
  }
  if(!is.vector(resp_var)){
    resp_var=as.vector(resp_var)
  }
  est_coef=solve(t(dsgn_mat)%*%dsgn_mat)%*%t(dsgn_mat)%*%resp_var
  return(as.numeric(est_coef))
}

gen_lse=function(dsgn_mat,resp_var,sigma){
  if(!is.matrix(dsgn_mat)){
    dsgn_mat=as.matrix(dsgn_mat)
  }
  if(!is.vector(resp_var)){
    resp_var=as.vector(resp_var)
  }
  sigma_inv=solve(sigma)
  est_coef=solve(t(dsgn_mat)%*%sigma_inv%*%dsgn_mat)%*%t(dsgn_mat)%*%sigma_inv%*%resp_var
  return(as.numeric(est_coef))
}


q1=function(n,r,true_a,true_b,prntile){
  results=list()
  est_coefs=list()
  mean_bias=list()
  mean_sqrd_error=list()
  percentile_points_for_a=list()
  percentile_points_for_b=list()
  true_coef_mat=data.frame(true_a=c(rep(true_a,r)),true_b=c(rep(true_b,r)))
  true_coef_mat=as.matrix(true_coef_mat)
  for(i in 1:length(n)){
    est_coef=matrix(,nrow = r,ncol = 2)
    est_coef_names=paste0("est_coef_for_n=",n[i])
    mean_bias_names=paste0("mean_bias_for_n=",n[i])
    mean_sqr_names=paste0("mean_sqrd_error_for_n=",n[i])
    percnt_names_a=paste0("percentile_points_for_a_for_n=",n[i])
    percnt_names_b=paste0("percentile_points_for_b_for_n=",n[i])
    for(j in 1:r){
      dsgn_mat_1=c(rep(1,n[i]))
      dsgn_mat_2=c(1:n[i])
      dsgn_mat=data.frame(dsgn_mat_1,dsgn_mat_2)
      dsgn_mat=as.matrix(dsgn_mat)
      epsn=rnorm(n[i],0,1)
      resp_vec=true_a*dsgn_mat[,1]+true_b*dsgn_mat[,2]+epsn
      est_coef[j,]=lse(dsgn_mat,resp_vec)
    }
    est_coefs[[est_coef_names]]=est_coef
    bias_mat=est_coef-true_coef_mat
    bias_2_mat=bias_mat^2
    mean_bias_vec=colMeans(bias_mat)
    mean_sqrd_vec=colMeans(bias_2_mat)
    mean_bias[[mean_bias_names]]=mean_bias_vec
    mean_sqrd_error[[mean_sqr_names]]=mean_sqrd_vec
    vec_for_a=est_coef[,1]
    ordrd_vec_for_a=sort(vec_for_a)
    perctl_vec_for_a=ordrd_vec_for_a[c(r*prntile)]
    percentile_points_for_a[[percnt_names_a]]=perctl_vec_for_a
    vec_for_b=est_coef[,2]
    ordrd_vec_for_b=sort(vec_for_b)
    perctl_vec_for_b=ordrd_vec_for_b[c(r*prntile)]
    percentile_points_for_b[[percnt_names_b]]=perctl_vec_for_b
  }
  results=list(est_coefs=est_coefs,mean_bias=mean_bias,mean_sqrd_error=mean_sqrd_error,percentile_points_for_a=percentile_points_for_a,percentile_points_for_b=percentile_points_for_b)
  return(results)
}
x1=q1(c(10,20,30,50),1000,1.5,2,c(0.05,0.1,0.9,0.95))
x1$mean_bias
x1$mean_sqrd_error
x1$percentile_points_for_a
x1$percentile_points_for_b


# Answer 1(b)
rand_vc=rnorm(100,0,1)
vc_1=rep(1,100)
vc_2=c(1:100)
rspnc_var=1.5*vc_1+2.0*vc_2+rand_vc
dsn_mat=as.matrix(data.frame(vc_1,vc_2))
reslt=lm(rspnc_var~vc_2)
reslt
summary(reslt)

q1_c=function(n,r,true_a,true_b,prntile){
  results=list()
  est_coefs=list()
  mean_bias=list()
  mean_sqrd_error=list()
  percentile_points_for_a=list()
  percentile_points_for_b=list()
  true_coef_mat=data.frame(true_a=c(rep(true_a,r)),true_b=c(rep(true_b,r)))
  true_coef_mat=as.matrix(true_coef_mat)
  for(i in 1:length(n)){
    est_coef=matrix(,nrow = r,ncol = 2)
    est_coef_names=paste0("est_coef_for_n=",n[i])
    mean_bias_names=paste0("mean_bias_for_n=",n[i])
    mean_sqr_names=paste0("mean_sqrd_error_for_n=",n[i])
    percnt_names_a=paste0("percentile_points_for_a_for_n=",n[i])
    percnt_names_b=paste0("percentile_points_for_b_for_n=",n[i])
    for(j in 1:r){
      dsgn_mat_1=c(rep(1,n[i]))
      dsgn_mat_2=c(1:n[i])
      dsgn_mat=data.frame(dsgn_mat_1,dsgn_mat_2)
      dsgn_mat=as.matrix(dsgn_mat)
      epsn=c()
      for(k in 1:n[i]){
        epsn[k]=rnorm(1,0,k)
        
      }
      resp_vec=true_a*dsgn_mat[,1]+true_b*dsgn_mat[,2]+epsn
      est_coef[j,]=lse(dsgn_mat,resp_vec)
    }
    est_coefs[[est_coef_names]]=est_coef
    bias_mat=est_coef-true_coef_mat
    bias_2_mat=bias_mat^2
    mean_bias_vec=colMeans(bias_mat)
    mean_sqrd_vec=colMeans(bias_2_mat)
    mean_bias[[mean_bias_names]]=mean_bias_vec
    mean_sqrd_error[[mean_sqr_names]]=mean_sqrd_vec
    vec_for_a=est_coef[,1]
    ordrd_vec_for_a=sort(vec_for_a)
    perctl_vec_for_a=ordrd_vec_for_a[c(r*prntile)]
    percentile_points_for_a[[percnt_names_a]]=perctl_vec_for_a
    vec_for_b=est_coef[,2]
    ordrd_vec_for_b=sort(vec_for_b)
    perctl_vec_for_b=ordrd_vec_for_b[c(r*prntile)]
    percentile_points_for_b[[percnt_names_b]]=perctl_vec_for_b
  }
  results=list(est_coefs=est_coefs,mean_bias=mean_bias,mean_sqrd_error=mean_sqrd_error,percentile_points_for_a=percentile_points_for_a,percentile_points_for_b=percentile_points_for_b)
  return(results)
}
x_c=q1_c(c(10,20,30,50),1000,1.5,2,c(0.05,0.1,0.9,0.95))
x_c$mean_bias
x_c$mean_sqrd_error
x_c$percentile_points_for_a
x_c$percentile_points_for_b


q1_c_2=function(n,r,true_a,true_b,prntile){
  results=list()
  est_coefs=list()
  mean_bias=list()
  mean_sqrd_error=list()
  percentile_points_for_a=list()
  percentile_points_for_b=list()
  true_coef_mat=data.frame(true_a=c(rep(true_a,r)),true_b=c(rep(true_b,r)))
  true_coef_mat=as.matrix(true_coef_mat)
  for(i in 1:length(n)){
    est_coef=matrix(,nrow = r,ncol = 2)
    est_coef_names=paste0("est_coef_for_n=",n[i])
    mean_bias_names=paste0("mean_bias_for_n=",n[i])
    mean_sqr_names=paste0("mean_sqrd_error_for_n=",n[i])
    percnt_names_a=paste0("percentile_points_for_a_for_n=",n[i])
    percnt_names_b=paste0("percentile_points_for_b_for_n=",n[i])
    for(j in 1:r){
      dsgn_mat_1=c(rep(1,n[i]))
      dsgn_mat_2=c(1:n[i])
      dsgn_mat=data.frame(dsgn_mat_1,dsgn_mat_2)
      dsgn_mat=as.matrix(dsgn_mat)
      epsn=c()
      for(k in 1:n[i]){
        epsn[k]=rnorm(1,0,k)
        
      }
      resp_vec=true_a*dsgn_mat[,1]+true_b*dsgn_mat[,2]+epsn
      sigma=diag(c(1:n[i])^2)
      est_coef[j,]=gen_lse(dsgn_mat,resp_vec,sigma)
    }
    est_coefs[[est_coef_names]]=est_coef
    bias_mat=est_coef-true_coef_mat
    bias_2_mat=bias_mat^2
    mean_bias_vec=colMeans(bias_mat)
    mean_sqrd_vec=colMeans(bias_2_mat)
    mean_bias[[mean_bias_names]]=mean_bias_vec
    mean_sqrd_error[[mean_sqr_names]]=mean_sqrd_vec
    vec_for_a=est_coef[,1]
    ordrd_vec_for_a=sort(vec_for_a)
    perctl_vec_for_a=ordrd_vec_for_a[c(r*prntile)]
    percentile_points_for_a[[percnt_names_a]]=perctl_vec_for_a
    vec_for_b=est_coef[,2]
    ordrd_vec_for_b=sort(vec_for_b)
    perctl_vec_for_b=ordrd_vec_for_b[c(r*prntile)]
    percentile_points_for_b[[percnt_names_b]]=perctl_vec_for_b
  }
  results=list(est_coefs=est_coefs,mean_bias=mean_bias,mean_sqrd_error=mean_sqrd_error,percentile_points_for_a=percentile_points_for_a,percentile_points_for_b=percentile_points_for_b)
  return(results)
}
x_c_2_1=q1_c_2(c(10,20,30,50),1000,1.5,2,c(0.05,0.1,0.9,0.95))
x_c_2_1$mean_bias
x_c_2_1$mean_sqrd_error
x_c_2_1$percentile_points_for_a
x_c_2_1$percentile_points_for_b


########## Answer 2
install.packages("boot")
library(boot)
install.packages("smoothmest")
library(smoothmest)

vec_1=1.5*rep(1,10)
vec_2=2*c(1:10)
rand_vec=rlaplace(10, m=0, s=sqrt(5/2))
## I am giving s=sqrt(5/2) as variance in the case of laplace distribution is 2*s^2
response_vec=vec_1+vec_2+rand_vec

a1=runif(5000,-2,4)
b1=runif(5000,-2,6)
a_b=as.matrix(data.frame(a1,b1))
Liklhd_fun1=function(b){
  sum1=0
  for(i in 1:10){
    sum1=sum1+abs(response_vec[i]-b[1]-b[2]*i)
  }
  lik=(1/sqrt(10))^10*exp(-sqrt(2/5)*sum1)
  return(lik)
}
fun_value=function(value){
  values=c(0,nrow(value))
  for(j in 1:nrow(value)){
    values[j]=Liklhd_fun1(as.vector(value[j,]))
  }
  return(values)
  
}
y1=fun_value(a_b)
library(rgl)
plot3d(x=a1,y=b1,z=y1,col="blue",xlab = "a",ylab = "b",zlab = "Value of Likelihood Function")
a_b[which(y1==max(y1)),] ##2.028089 1.886935 

L=c(rep(1,20),0,0,0,0)
A_1=rep(c(1,-1,rep(0,20)),10)
A_2=matrix(A_1,ncol = 20,byrow = T)
A_2=A_2[-11,]
A=data.frame(A_2,rep(1,10),-rep(1,10),c(1:10),-c(1:10))
A=as.matrix(A)
simplex(a=L,A3=A,b3=response_vec)

Liklhd_fun=function(b){
  sum1=0
  for(i in 1:10){
    sum1=sum1+abs(response_vec[i]-1.5-b*i)
  }
  lik=(1/sqrt(10))^10*exp(-sqrt(2/5)*sum1)
  return(lik)
}
curve(Liklhd_fun, from=-5, to=5, n=1000,xlab = "b",ylab = "Value of Likelihood Function")
fun_value1=function(value){
  values=c(0,length(value))
  for(j in 1:length(value)){
    values[j]=Liklhd_fun(as.vector(value[j]))
  }
  return(values)
  
}
b_2=seq(-2,6,length.out = 3000)
lik_value=fun_value1(b_2)
plot(y=lik_value,x=b_2, xlab = "b",ylab = "Value of Likelihood Function")
b_2[which(lik_value==max(lik_value))] ##1.977326
