## Starting 04-02-2017
## Model we have y(t) = 1.5 + 2.0t + epsilon_(t);

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




