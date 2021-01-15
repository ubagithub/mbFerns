#    R part of Multi_Branch Decission Tree
#
#    Copyright 2018  Ulavappa B Angadi
#
#    This file is part of Multi_Branch Decission Tree R package.
#
#Multi_Branch Decission Tree  is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License .
#Multi_Branch Decission Tree  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

mbferns<-function(x, ...)
 { UseMethod("mbferns")}

mbferns.default <- function(x, y, nf=2, fsize=4, bnum=4, ...) {
  nr<-nrow(x)
  nc<-ncol(x)
  xm=matrix(0,nrow=nr, ncol=nc)
  cnames=colnames(x)
  
  if (fsize>30) stop (" fern depth (fsize) must be less that 20")
  
  if (!(bnum>1 & bnum<9)) stop ( "Branch number (bnum) must be between 2 to 8")
  
  if(!is.matrix(x) & !is.numeric(x))
  {
    if(!is.data.frame(x)) 
      stop("x must be a data frame or numeric matrix")
    else
    {
    for (j in 1:nc)
    {
      a=which(is.na(x[,j]) == TRUE)
      if (length(a)>0)
      {levels(x[,j])=c(levels(x[,j]),0)
       x[,j][is.na(x[,j])] <-0
      }
      
      if (is.factor(x[,j]) | is.numeric(x[,j])) 
      xm[,j]=as.numeric(x[,j])
      else stop("all columns must be numeric or factor ")
    }
    }
  }
  if(length(y)!=nr) stop(" Size of x and y must match.")
  c<-length(unique(y))
  if (is.factor(y))
  {
    ylist=levels(y)
#   c<-length(levels(y))
  }
  else if (is.numeric(y))
  {  ylist=as.numeric(sort(unique(y) ))  }
  else
  {  ylist=as.character(sort(unique(y))) }
  
   y=factor(y,labels=c(0:(c-1)))
   y=as.numeric(levels(y)[as.integer(y)])
  
  if (nf<(nc/fsize))
  {nf=nc/fsize + 1;}
  
  ws=nf*c*(2^(fsize*ceiling(log2(bnum))))+10
  cat("\n entering into c64")
library(dotCall64)
  cat( nr, nc, c, nf, ws)
#  xx <- .C("mbrfern", as.double(xm), as.integer(y), as.integer(nc),  as.integer(nr), as.integer(c),  as.integer(nf), as.integer(fsize), as.integer(bnum), xmin1=double(nc), xmax1=double(nc), flist1=integer(nf*fsize), pc=double(c), w=integer(ws), maxz=integer(nf*c), maxzloc=integer(nf*c))
  xx <- .C64("mbrfern", SIGNATURE=c("double", "integer", "integer",  "integer", "integer",  "integer", "integer", "integer", "double", "double", "integer","double", "integer","integer","integer"), xm=xm, y=y, nc=nc, nr=nr, c=c, nf=nf, fsize=fsize, bnum=bnum, xmin1=double(nc), xmax1=double(nc), flist1=integer(nf*fsize), pc=double(c), w=integer(ws), maxz=integer(nf*c), maxzloc=integer(nf*c))
  maxzz=matrix(data=xx$maxz, nrow=c, ncol=nf, byrow=TRUE)
  maxzzloc=matrix(data=xx$maxzloc, nrow=c, ncol=nf, byrow=TRUE)
  model<-list(nc=xx[[3]], c=xx[[5]], nf=xx[[6]], fsize=xx[[7]], bnum=xx[[8]], min=xx$xmin1, max=xx$xmax1, flist=xx$flist1, w=xx$w, ylist=ylist, pc=xx$pc, maxz=maxzz, maxzloc=maxzzloc, colnames=cnames)
 
  class(model)<-"mbferns"

   return(model)
  
}

#******************************************************
# Prediction with developed model
#******************************************************

  predict.mbferns <- function(object, x, ...) {
    
  if(!("mbferns"%in%class(object))) stop("model must be of a mbferns model")
  if(is.null(x)) stop("x is missing")
  
 # nc=object$nc
#  c=object$c
 # nf=object$nf
 # fsize=object$fsize
 # bnum=object$bnum
 # max=object$max
# min=object$min
 # flist=object$flist
 # w=object$w
 # ylist=object$ylist
 # pc=object$pc
  
  
  nr<-nrow(x)
  
  xm=matrix(data=0,nrow=nr, ncol=object$nc)
  
    
  if(object$nc!=ncol(x)) stop(" Size of columns/variables must match in training and testing data. ")
  
  
  if(!is.matrix(x) & !is.numeric(x))
  {
    if(!is.data.frame(x)) 
      stop("x must be a data frame or numeric matrix")
    else
    {
      for (j in 1:object$nc)
      {
        a=which(is.na(x[,j]) == TRUE)
        if (length(a)>0)
          {levels(x[,j])=c(levels(x[,j]),0)
           x[,j][is.na(x[,j])] <-0
         }
       
        if (is.factor(x[,j])) 
          xm[,j]=as.numeric(x[,j]) 
        else
          if (is.numeric(x[,j]))
            xm[,j]=as.numeric(x[,j]) 
        else stop("all columns must be numeric or factor ")
      }
    }
  }
library(dotCall64)
cat ("\n entering c64-1")
 # w1dens <- .C("mbrfern_predict", as.double(xm),  as.integer(object$nc),  as.integer(nr), as.integer(object$c),  as.integer(object$nf),  as.integer(object$fsize), as.integer(object$bnum), as.double(object$min), as.double(object$max), as.integer(object$flist), as.integer(object$w), as.double(object$pc), ye=integer(nr), prob=double(nr*object$c) )
w1dens <- .C64("mbrfern_predict", SIGNATURE=c( "double",  "integer",  "integer", "integer", "integer",  "integer", "integer", "double", "double", "integer", "integer", "double", "integer", "double"), xm=xm, nc=object$nc, nr=nr, c=object$c, nf=object$nf, fsize=object$fsize, bnum=object$bnum, min=object$min, max=object$max, flist=object$flist, w=object$w, pc=object$pc, ye=integer(nr), prob=double(nr*object$c))
  # library(plyr)
  w1dens$ye=mapvalues(w1dens$ye, from=c(0:(object$c-1)), to=c(object$ylist))
  w1dens$ye=factor(w1dens$ye)
  pr=matrix(w1dens$prob, nrow=nr, ncol=object$c, byrow = TRUE)
 # kf=matrix(w1dens$key_fern, nrow=c, ncol=nf)
  y<-model<-list(ey=w1dens$ye, prob=pr)
 return(y)
}


#******************************************************
# treeDraw with developed model
#******************************************************


print.mbferns<-function(object, ...){
  
  if(!("mbferns"%in%class(object))) stop("model must be of a mbferns model")
  # function(d, b, p, labels1, listf, tcount, title, filename, ...) 
  #if(is.null(x)) stop("x is missing")
  wp=object$w
  c=object$c
  nf=object$nf
  fsize=object$fsize
  d=fsize+1
  b=object$bnum
  xmax=object$max
  xmin=object$min
  flist=object$flist
  
   b1=ceiling(log2(b));
   zn=2^(b1*fsize);
   zn1=2^(b1*fsize-1);
  
  #d=4
  #b=2
  
  for (cc in 1:c)
  {
    for (nfnf in 1:nf)
    {
      p=object$maxzloc[cc,nfnf]
      
      filename=paste("Tree_Class-",cc,"_Fern-", nfnf,".png",sep = "" )
      image.width=3200
      image.height=3200
      rangestr=""
      png(filename=filename, width=image.width,height=image.height)
      
      #  stcount=toString(tcount)
      stcount=toString(object$maxz[cc,nfnf])
      stcount=paste("C-",cc,":",stcount, sep = "")
      #library("plotrix", lib.loc="~/R/win-library/3.0")
      bsize=ceiling(log2(b))
      bb=as.numeric(intToBits(p))
      bb1=rev(bb[1:((d-1)*bsize)])
      bb1[((d-1)*bsize)+1]=0
      bb1[(d*bsize)]=0
      w=b^(d-1)
      xstart =as.array(0,dim=(d+1))
      xincr=as.array(0,dim=(d+1))
      xstart[1]=1
      incr=(b-1)/2
      ii=c(1:d)
      for (i in ii)
      {
        xstart[i]=b^(d-1)/(b^(i-1)*2)+.5
        xincr[i]=b^(d-i)
      }
      plot(c(0,w), c(0,d), type = "n", xlab="", ylab="", xaxt="n", yaxt="n", axes=F,frame.plot=F )
      
      prevx=0
      
      y=c(0:d)
      z=c(1:b)
      for (y1 in y)
      {
        x=c(1:b^(y1))
        xx1=xstart[y1+1]
        xx2=xstart[y1]
        j=1
        test=toString(object$colnames[flist[d-y1]+1])
        test1=toString(object$colnames[flist[y1]+1])
        test22=toString(y1+1)
        text(0, y1+1,test )
    #    text(1, y1+1,test22 )
        
        if (y1>0)
        {
          bb11=bb1[((y1-1)*bsize+1):((y1)*bsize)]
          bb2=paste(bb11, collapse="")
          bb3=strtoi(bb2, base=2)
          diff=xmax[flist[y1]+1]-xmin[flist[y1]+1]
          range=paste(format(1/b*bb3*diff+xmin[flist[y1]+1],digits=3),format(1/b*(bb3+1)*diff+xmin[flist[y1]+1], digits=3), sep = "-")
         # range=paste(format(1/b*bb3,digits=3),format(1/b*(bb3+1), digits=3), sep = "-")
          rangestr=paste( rangestr, "  ", test1,":",range, ",", sep = "")
          
          prevx=prevx*b+bb3
        }
        else
        {
          ratio=1/b
          r1=0.0
          w1=w/10
          w2=w1+w1/1.5
          for (z1 in z)
          {
            r2=r1+ratio
            rr2=format(r2,digits=2)
            rr1=format(r1, digits=2)
            r3=paste(rr1,rr2, sep="-")
            # xloc=(xstart[y1+1]-b*w1)+w2*z1
            xloc=xstart[2]+(z1-1)*xincr[2]
            text(xloc, (d-y1-.7), r3)
            r1=r2
          }
        }
        
        for ( x1 in x)
        {
          draw.circle(xx1,d-y1,.25)
          if (y1>0)
          {
            
            
            if (x1==(prevx+1))
            {segments(xx1, d-y1 , xx2, d-(y1-1), col= 'red')
             
             if (y1==(d-1))
             { rangestr=paste("Class-",cc,"_Fern-", nfnf, "=",rangestr, sep = "")
               text(450, 0.75, rangestr)
             }  
            }
            else
            {segments(xx1, d-y1 , xx2, d-(y1-1), col= 'blue')}
          }
          if (j==b)
          {xx2=xx2+xincr[y1] 
           j=1}
          else
          {j=j+1}
          xx1=xx1+xincr[y1+1]
        }
      }
    
    for ( x1 in (1:(zn1)))
    {
      x2=wp[(cc-1)*nf*zn+(nfnf-1)*zn+x1]
      x3=(cc-1)*nf*zn+(nfnf-1)*zn+x1
      if (x2>0)
      {test22=toString(x2)
       test222=toString(x3)
       test123=paste(test222, test22,sep=":" )
       if (x1 %%2 ==1)
      text (x1, 0.9, test22)
      else
      text (x1, 0.8, test22) 
      }
    }
     # text(1, d,test22 )
      text (10,d-0.1,rangestr )
     # test22=toString(stcount)
      text (10, d-0.2, stcount)
    
      
      
      dev.off()
        cat("\n",stcount, rangestr)
      #  model1<-list(desc=rangestr)
      #  class(model1)<-"drawtree"
      #  return(model1)
    }
    
  }
}

 




