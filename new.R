library(imager)
# load image
image_1 = load.image(file.choose())
image_2 = load.image(file.choose())

# get image Dimensions 
dim_image_1 = dim(image_1)

# set Blur and Threshold on image
image_1_blur = isoblur(image_1,1)
image_1_threshold = threshold(image_1_blur,"auto")

# to get our target 
image_1_vec = c(image_1)
image_1_vec_copy = c(image_1)
image_1_threshold_vec = c(image_1_threshold)
n = length(image_1_vec)

for(i in 1:n) {
  if(image_1_threshold_vec[i] != 0 ){
    image_1_vec[i] = image_1_vec[i]
  }else{
    image_1_vec[i] = NaN
    }
}

Q1 = quantile(image_1_vec,  probs = c(.65),na.rm = T)
Q2 = quantile(image_1_vec,  probs = c(.95),na.rm = T)
lower = Q1*1.5
upper = Q2*1.5
image_1_vec_copy = c(image_1)
for(i in 1:n) {
  if(image_1_vec_copy[i] <= lower | image_1_vec_copy[i] >= upper ){
    image_1_vec_copy[i] = 0
  }else{
    image_1_vec_copy[i] = image_1_vec_copy[i]
  }
}
image_1_vec_copy = as.cimg(image_1_vec_copy,x=dim_image_1[1],y=dim_image_1[2])%>% plot
# cut target (segmentation)
pixst = image_1_vec_copy != 0
bbox_im =bbox(pixst)
highlight(bbox_im)
ex1 = crop.bbox(image_1,bbox_im)%>%plot
ex2 = crop.bbox(image_2,bbox_im)%>%plot

# methode functions

CE = function(x){
  x = c(x)
  n=length(x)
  a<-sort(x)
  U=c()
  U[1]=a[1]
  for (j in 2:n) {
    U[j]=a[j]-a[j-1]
    
  }
  
  s=0
  for (j in 1:(n-1)) {
    s=s+U[j+1]*(j/n)*(log(j/n))
    
  }
  ce=-s
  return(ce)
}
k = function(x,y){
  x = c(x)
  n=length(x)
  a<-sort(x)
  b<-sort(y)
  N=c()
  for (j in 0:n) {
    N[j]=sum(x<=b[j]) 
  }
  X = list()
  for(k in 1:(n-1)){
    fit = b[k:(k+1)]
    c = a>fit[1] & a<= fit[2]
    cc = a[c]
    X[[k]] = cc
  }  
  SUM = 0
  for(j in 1:(n-1)){
    s = (N[j+1]*b[j+1]) - (N[j]*b[j])
    ss =0
    for(r in 1:(N[j+1]-N[j])){
      if((N[j+1]-N[j])==0){
        ss = ss + 0}else{
          ss = X[[j]][r] + ss
        }
    }
    SUM = ( (s-ss)*log(j/n) )+ SUM
  }
  
  g = -(1/n)*SUM
  return(g)
}

M1 = mean(c(ex1),na.rm = T)
M2 = mean(c(ex2),na.rm = T)
ckl1 = k(ex1,ex2) - CE(ex1) + M1 - M2
cKl2 = k(ex2,ex1) - CE(ex2) + M2 - M1
mx = data.frame("mean"=c(M1,M2),"method"=c(ckl1,cKl2),"var"=c(var(ex1),var(ex2))
  ,row.names = c("image 1","image 2"))

# results
lo = (ckl1>cKl2)&(M1>M2) | (var(ex1)>var(ex2))&(M1>M2)
tst=ifelse(lo,"improvement","deterioration")
print(tst)

# plot images
image_1%>% plot
image_1_blur%>% plot
image_1_threshold%>% plot
as.cimg(image_1_vec_copy,x=dim_image_1[1],y=dim_image_1[2])%>% plot
