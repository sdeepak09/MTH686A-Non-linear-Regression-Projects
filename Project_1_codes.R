## Starting 04-02-2017
## Model we have y(t) = 1.5 + 2.0t + epsilon_(t);
epsn=rnorm(1000,0,1)
a=c(rep(1,1000))
a=1.5*a
b=c(1:1000)
b=2*b
y=a+b+epsn
dsgn_mat_1=c(rep(1,1000))
dsgn_mat_2=c(1:1000)
dsgn_mat=data.frame(dsgn_mat_1,dsgn_mat_2)
dsgn_mat=as.matrix(dsgn_mat)
est_coef=solve(t(dsgn_mat)%*%dsgn_mat)%*%t(dsgn_mat)%*%y

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