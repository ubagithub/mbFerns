# mbFerns
mbFerns-package {mbFerns}	R Documentation
Multi Branch Ferns based Naive Bayesian classification program

Description

This package is integration of best techniques to single classification package, such as multi branch ferns for generating of key features from training dataset and Naive Bayesian probabilistic model to generic algorithm for general purpose classification problems.

Details

Package:	 mbFerns
Type:	 Package
Version:	 1.0.1
Date:	 2018-06-22
License:	 GPL
Multi-Barnach Ferns is a generic classifier for general classification problems. This is applicable exclusively for general purpose classification and pattern analysis problems. This program outperformed by generating key features with frequencies using normalization of features values between 0 and 1 and forming multi branch ferns by multiple paths with normalized values. This is generalized semi Naive Bayesian ferns algorithm for general purpose and satisfied the assumption that features are mutually independent. This program showed significant improvements in classification accuracy and speed.

Usage:
model=mbferns(x, y [, num_ferns, fern_depth, num_branch]) 
pred=predict(model, x)
Author(s)

Ulavappa B Angadi ub.angadi@icar.gov.in

References

~~ Literature or other references for background information ~~ ~~ Optionally other standard keywords, one per line, from file ~~ ~~ KEYWORDS in the R documentation directory ~~

See Also

~~ Optional links to other man pages, e.g. ~~

Examples

library(mbFerns)
library("mlbench")
data(DNA)
data1=DNA
data1<-na.omit(data1)
data1$Id<-NULL
nr=nrow(data1)
nc=ncol(data1)

index <- 1:nr

percent1=90
trainindex <- sample(index, trunc(length(index)*(percent1/100)))
testindex=setdiff(index, trainindex)

x1<-data1[trainindex,1:nc-1]
y1<-data1[trainindex,nc]


x2<-data1[testindex,1:nc-1]
y2<-data1[testindex,nc]

extra_ferns_num=1

fern_depth=2

num_branch=4


num_ferns=as.integer(nc/fern_depth)+extra_ferns_num

cat(" Training started....")

# default value of num_ferns= number of features/ fern_depth, 
# default value fern_depth=4
# default value num_branch =4
model1=mbferns(x1, y1, num_ferns, fern_depth, num_branch)

cat(" Tesing started......")

ye<-predict(model1, x2)


cat(" Confussion matrix ....")
aa1<-table(ye$ey, y2)
print(model1)
