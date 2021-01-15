//  Developed by U B Angadi

// R part of Multi_Branch Decission Tree
//
//   Copyright 2018  Ulavappa B Angadi
//
//  This file is part of Multi_Branch Decission Tree R package.
//
// Multi_Branch Decission Tree  is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License 
// Multi_Branch Decission Tree  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.


//#include <R.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <malloc.h>
#include <R.h>
#include <R_ext/Utils.h>
#include <Rmath.h>

//#include <bits.h>


void mean_variance_min_max( double *a, int n, double *mean1, double *std1, double *min, double *max  )
{
    *min = a[0];
    *max = a[0];
   double sumx = 0;
   double sumxx =0;
   int i;

 for (i=0;i<n;i++)
   {sumx =sumx+a[i];
    sumxx =sumxx+a[i]*a[i]; 
    if (a[i] > *max)
       {
           *max = a[i];
       }
       else if (a[i] < *min)
       {
           *min = a[i];
       }
    } 
    
    *mean1=sumx/n;
    *std1 = sqrt(sumxx/n - pow(*mean1,2));
} 





double max_index( double *a, int n )
{
  
   int index=0, i;
   double max=a[0];
   
     for (i=1;i<n;i++)
   {
       if (a[i] > max)
       {max = a[i]; index=i;}
    }

   return (index) ;
} 


int compare(const void * a, const void * b)
{
     if (*(double*)a > *(double*)b)
    return 1;
  else if (*(double*)a < *(double*)b)
    return -1;
  else
    return 0;  
}

void swap(double *xp, double *yp)
{
    double temp = *xp;
    *xp = *yp;
    *yp = temp;
}

void bubbleSort(double arr[], int ind[], int n)
{
   int i, j, k;
   for (i = 0; i < n-1; i++)      
        for (j = 0; j < n-i-1; j++) 
           if (arr[j] < arr[j+1])
            {  swap(&arr[j], &arr[j+1]);
              k=ind[j];
              ind[j]=ind[j+1];
              ind[j+1]=k;
            }
}

void mbrfern(double *x, int *y, int *nc1, int *nr1, int *c1, int *nf1, int *fsize1, int *bn, double *xmin1, double *xmax1, int *flist1, double *pc, unsigned int *w1, unsigned int *maxvalue, unsigned int *maxlocation )
{
  unsigned int nc=*nc1, nr=*nr1, c=*c1, nf=*nf1, fsize=*fsize1, b=*bn;

 //printf( " nr=%d nc=%d c=%d nf=%d \n", nr, nc, c, nf);

  FILE *ptr_file;
  ptr_file =fopen("output_train.txt", "w");
  fprintf(ptr_file," k, f, z, add, w, nk \n");  

//  unsigned int fsize=5;
  unsigned int b1=ceil(log2(b));         // 1, 2, 3
//   int b2=pow(2,b1); // 2, 4, 8
  

  if (nf<(nc/fsize))
    {nf=nc/fsize + 1;}
  
  
  
  double  *xmin=0, *xmax=0, *xstd=0;
  unsigned int *flist=0, maxz=0 ;
  int *ind=0, *nk=0;
  unsigned int j, i, f;
  double *xx=0;
  
  
   flist = (unsigned int*) malloc(nf*fsize*sizeof(unsigned int)); 
   xmax = (double*) malloc(nc*sizeof(double)); 
   xmin = (double*) malloc(nc*sizeof(double)); 
   xstd = (double*) malloc(nc*sizeof(double)); 
   ind = (int*) malloc(nc*sizeof(int));
   nk = (int*) calloc(c,sizeof(int));
   xx = (double*) malloc(nr*sizeof(double));
   
  // unsigned int  *maxvalue=0, *maxlocation=0;
 //  maxvalue= (unsigned int*) malloc(nc*nf*sizeof(unsigned int));
  // maxlocation=(unsigned int*) malloc(nc*nf*sizeof(unsigned int));

/*for (i=0; i<nr; i++)
  { printf ( "y=%d \n",*(y+i)); } */



 

 // for(j=0; j<nf4; j++)
 // { printf( " fern %d = %d", j, *(flist+j) ) ; }

/* for(i=0;i<nr; i++)
 {for(j=0; j<nc; j++)
  {printf(" \n x[%d,%d]=%f", i, j, *(x+j*nr+i));}
   printf(" \n ");
  } */
 
 double minx, maxx, meanx,std1;
 
   for(j=0; j<nc; j++)
        {   
         for(i=0;i<nr; i++)
          {*(xx+i)= *(x+j*nr+i);
  	  // printf(" \nx[%d, %d]=%f", i, j, *(xx+i)); 
		   } 
            
       /*     min_max(xx, nr, &minx, &maxx);
             *(xmin+j)=minx;
            *(xmax+j)=maxx; 
            
          *(xmin+j)=min1(xx,nr);
          *(xmax+j)=max1(xx,nr); */
          
          mean_variance_min_max(xx, nr, &meanx, &std1, &minx, &maxx);
       //      *(xmin+j)=minx;
            *(xstd+j)=std1; 
            *(xmin+j)=minx;
            *(xmax+j)=maxx;
            *(ind+j)=j;
             xmin1[j]=*(xmin+j); 
             xmax1[j]=*(xmax+j);
            
         }
 
 
 
 /*  for(j=0; j<nc; j++)
  { 
    fprintf( ptr_file," \n %f %f %f %d", *(xmin+j), *(xmax+j), *(xstd+j), *(ind+j) ) ;
     xmin1[j]=*(xmin+j); 
     xmax1[j]=*(xmax+j); 
     }  */

//qsort(xstd, nc, sizeof(double), compare);
bubbleSort(xstd, ind, nc);

/* for(j=0; j<nc; j++)
  { fprintf( ptr_file," \n %f %f %f %d", *(xmin+j), *(xmax+j), *(xstd+j),  *(ind+j) ) ;
     xmin1[j]=*(xmin+j); 
     xmax1[j]=*(xmax+j); }  */
     

// making ferns
/*
   for(j=0; j<nc; j++)
  { *(flist+j)=j;  
   flist1[j]=j;} */

 

for(j=0; j<nc; j++)
  { *(flist+j)=*(ind+j);  
   flist1[j]=*(ind+j);}

 int nf4= nf*fsize;

 // time_t t;
 // srand((unsigned) time(&t));

  for (f=nc; f<nf4; f++)
  { 
  //  *(flist+f)=rand()%nc;
       GetRNGstate();
     *(flist+f)= (int) ((unif_rand() * (double) nc));
      PutRNGstate();
     flist1[f]=*(flist+f);
  }
  
//unsigned int j1;


//int p28=pow(2,8);
unsigned int cnf,k, j2, z;
unsigned int zn=pow(2,(b1*fsize));

cnf=c*nf*zn;
double ax1;
unsigned int z1,fid;

//printf("cnf=%d \n", cnf);

// w = (unsigned int*) calloc(cnf, sizeof(unsigned int));
 
 //w1=(int*) malloc(cnf*sizeof(int));
 
 // w=array(0, dim=c(c, nf, 2^(b*fsize)) )

 for(i=0; i<=cnf; i++)
  { *(w1+i)=0;  }

  
  for (i=0; i<nr; i++)
  { k= (*(y+i));
    nk[k]++;
 //   j1=0;
    for (f=0; f<nf; f++)
    { j2=0;
      z=0;
     
      for (j=0; j<fsize; j++)
      {   ax1=0;
          fid=*(flist+f*fsize+j);
         // printf(" %d %d %d %d ", fid, i ,f, j);
          ax1=(*(x+fid*nr+i)-*(xmin+fid))/(*(xmax+fid)-*(xmin+fid));
            

          if (ax1<=0) { ax1=0; }
          if (ax1>=1) { ax1=0.99999; }
      
        z1=(int)(ax1*b)<<j2*b1;
        z=z|z1;
        j2=j2+1;
    //   fprintf(ptr_file,"j=%d ax=%f  z1=%d z=%d x=%f xmin=%f, xmax=%f\n",j, ax1, z1, z, *(x+fid*nr+i), *(xmin+fid), *(xmax+fid) );
       }
     
    //  z=z+1;
    
    //  printf(" i=%d k=%d f=%d z=%d add=%d ",i, k, f, z, k*nf*256+f*256+z);
      
      *(w1+k*nf*zn+f*zn+z)+=1;
  //    w1[k*nf*zn+f*zn+z]=*(w+k*nf*zn+f*zn+z);
      
    //  fprintf(ptr_file,"%d, %d, %d, %d, %d, %d \n",i, k, f, z, k*nf*zn+f*zn+z, w1[k*nf*zn+f*zn+z] );
   
 
    }
 }

// fprintf(ptr_file,"\n----------------------------------------------------------------%ld, %d, %d, %d, %d\n", sizeof(int), c ,nf, zn, cnf);

 for (k=0; k<c; k++)
 {
  for (f=0; f<nf; f++)
  { maxz=0;
        for(z=0;z<zn;z++)
        {if (*(w1+k*nf*zn+f*zn+z)>maxz)
        {
        maxz=*(w1+k*nf*zn+f*zn+z);
        maxvalue[k*nf+f]= maxz;
        maxlocation[k*nf+f]=z;
        }
        fprintf(ptr_file,"%d, %d, %d, %d, %d, %d\n", k, f, z, k*nf*zn+f*zn+z, *(w1+k*nf*zn+f*zn+z), nk[k] ); 
        }
       // fprintf(ptr_file,"%d, %d, %d, %d, \n", k, f, *(maxlocation+k*nf+f), *(maxvalue+k*nf+f) ); 
  }
       
 } 
 
  fclose(ptr_file); 

for (k=0;k<c; k++)
 pc[k]=(double) nk[k]/nr;
 
 
 //  return 0;
 
 free(flist);
 free(xx);
 free(xmin);
 free(xmax);
 free(ind);
 free(nk);
 free(xstd);
 
  }

// ********************************************************************************************//

void mbrfern_predict(double *x, int *nc1, int *nr1, int *c1, int *nf1, int *fsize1, int *bn, double *xmin, double *xmax, int *flist, int *w, double *pc, int *ye, double *prob)
{
 unsigned int nr=*nr1, c=*c1, nf=*nf1, fsize=*fsize1, b=*bn;

   unsigned int b1=ceil(log2(b));  
 //  unsigned int b2=pow(2,b1); // 2, 4, 8

  //int fsize=5;
  
   double *prob_fk=0, *prob_fkn=0,  *prob_ik;

   prob_fk = (double*) calloc((nf+1)*(c+1),sizeof(double));
   prob_fkn = (double*) calloc((nf+1)*(c+1),sizeof(double));
   prob_ik = (double*) calloc((nr+1)*(c+1),sizeof(double));

  
  unsigned int  fid, z,z1, zn, i, j, k, f, j2, total_wkgz;

  zn=pow(2,(b1*fsize));


  double ax1, totalprob_fkn=1, *xx;

  xx = (double*) malloc((nr+1)*(c+1)*sizeof(double));
  
 /* FILE *ptr_file;
  ptr_file =fopen("output_test.txt", "w");
  fprintf(ptr_file,"k, f, z, add, w \n"); */
  
  /* FILE *ptr_file1;
  ptr_file1 =fopen("output_test_1.txt", "w");
  

  printf( " Testing started %d %d %d %d \n", nr, nc, c, nf);

  for(j=0; j<nf*4; j++)
  { printf( " fern %d = %d ", j, flist[j] ) ; }

  for(j=0; j<nc; j++)
  { printf( "\n %f %f ", xmin[j], xmax[j] ) ; } */


  


  
 for (i=0; i<nr; i++)
  { 
 //   j1=0;
     for (k=0;k<c;k++)
        prob_ik[i*c+k]=1;
    
    for (f=0; f<nf; f++)
    { j2=0;
      z=0;
     
      for (j=0; j<fsize; j++)
      {   ax1=0;
          fid=flist[f*fsize+j];
        //  printf(" %d %d %d %d ", fid, i ,f, j);
          ax1=(*(x+fid*nr+i)-xmin[fid])/(xmax[fid]-xmin[fid]);

          if (ax1<=0) { ax1=0; }
          if (ax1>=1) { ax1=0.99999; }
      
        z1=(int)(ax1*b)<<j2*b1;
        z=z|z1;
        j2=j2+1;
   //    printf("\n ax2=%f  z12=%d 2z=%d ", ax1, z1, z);
       } // end of j lop
     
     // z=z+1;
      
     // int maxz;
      
      totalprob_fkn=0;
      for (k=0; k<c; k++)
      { total_wkgz=1;
        for(z1=0;z1<zn;z1++)
        {total_wkgz=total_wkgz + w[k*nf*zn+f*zn+z1]; 
         
        }
        prob_fk[f*c+k]= (double)(w[k*nf*zn+f*zn+z]+1)/total_wkgz ;
     //   printf("\n total=%d add=%d w=%d  prob_fk[%d, %d]= %f \n", total_wkgz, k*nf*256+f*256+z, w[k*nf*256+f*256+z], f, k, prob_fk[f*c+k]); 
		    totalprob_fkn=totalprob_fkn+prob_fk[f*c+k];
        
  //      fprintf(ptr_file,"%d, %d, %d, %d, %d, %d, %d, %f \n", i, k, f, z, k*nf*zn+f*zn+z, w[k*nf*zn+f*zn+z], total_wkgz, prob_fk[f*c+k] );
      
      }
      
     
  /*  for (k=1;k<=c; k++)
      {prob_ik[i*c+k]=1;
       totalprob_fkn=totalprob_fkn+prob_fk[f*c+k];} */
      
    //totalprob_fkn=sum(prob_fk[f,]
    
//     fprintf(ptr_file1, "\n %d ",f);
      for (k=0;k<c;k++)
      {prob_fkn[f*c+k]=(prob_fk[f*c+k]/totalprob_fkn);
 //  	   fprintf(ptr_file, "nprob_fk[%d, %d] =%f \n", f, k, prob_fkn[f*c+k]);
 //      fprintf(ptr_file1, "%f ",prob_fkn[f*c+k]);
       prob_ik[i*c+k]=  prob_ik[i*c+k]*prob_fkn[f*c+k] ;
       *(xx+k)= prob_ik[i*c+k];
	    }
      
     // prob_fkn[f,]=(prob_fk[f,]/totalprob_fkn)

    }   // end of f loop
    
    double total_k=0;
    
/*    
    fprintf(ptr_file1, "\n %d ",i+1);
    for (k=0; k<c; k++)
    {
      fprintf(ptr_file1,"%e ", *(xx+k)  ); 
    } 
    
     fprintf(ptr_file1, "\n %d ",i+1);
    for (k=0; k<c; k++)
    {
      fprintf(ptr_file1,"%f ", pc[k]  ); 
    } 
    
    fprintf(ptr_file1, "\n %d ",i+1); */
    
    
    for (k=0; k<c; k++)
    {
      *(xx+k)=*(xx+k)*pc[k];
      total_k=total_k+*(xx+k);
//     fprintf(ptr_file1,"%e ", *(xx+k)  ); 
    } 
//    fprintf(ptr_file1, " \n %d ",i+1);
    for (k=0; k<c; k++)
    {
      *(xx+k)=*(xx+k)/total_k;
    //  fprintf(ptr_file1,"%f ", *(xx+k)  ); 
      *(prob+i*c+k)=*(xx+k);
    } 
    
    
    ye[i]= max_index(xx,c);
  //  fprintf(ptr_file1,"%d ", ye[i]  );
  }
  
  
  
 //ye = (int*) malloc((nr+1)*sizeof(int));
 
// fprintf(ptr_file, " ___________________________________________________________\n");
 
 /* for (i=0; i<nr; i++)
  { 
   for(k=0; k<c; k++)
    {*(xx+k)= prob_ik[i*c+k];
 //    fprintf(ptr_file," prob_ik[%d, %d]= %f \n", i, k, prob_ik[i*c+k]);
    }
     ye[i]= max_index(xx,c);
  //   fprintf(ptr_file, " y[%d]=%d \n", i, ye[i] ); 
  } */
  
  /*
  int maxz;
  
  for (k=0; k<c; k++)
 {
    key_ferns[f*c+k]=0;    
  for (f=0; f<nf; f++)
     {   maxz=0;
        for(z=0;z<zn;z++)
       { if (w[k*nf*zn+f*zn+z]>0)
  //      fprintf(ptr_file,"%d, %d, %d, %d, %d\n", k, f, z, k*nf*zn+f*zn+z, w[k*nf*zn+f*zn+z] );
        if ( maxz < w[k*nf*zn+f*zn+z])
            { maxz = w[k*nf*zn+f*zn+z];
              *(key_ferns+f*c+k) = z;      
               
            }
       }
     }
 } */ 
//  fclose(ptr_file);
//  fclose(ptr_file1);

free(prob_fk);
free(prob_fkn);
free(prob_ik);
free(xx);
  }

/*
int main()
{
 
double aa[10]= {1, 2, 33, 4, 5, 6, 7, 8, 91, 10} ;

double mx, mn;

mx=max1(aa, 10);

mn=min1(aa, 10);

double *a=0;

a = (double*) malloc(10*10*sizeof(double));

int *b=0;
b = (int*) malloc(10*sizeof(int));
int i,j;

for (i=0; i<10; i++)
 for(j=0; j<10; j++)
{
 *(a+i*10+j)=i*10+j+1;
 *(b+i)=i%2+1;
}
*(b+1)=1;
*(b+4)=1;

FILE* file = fopen("breastcancer.csv", "r"); 
// should check the result 
char line[256];

if (file == NULL){
       printf("Error! opening file");

       // Program exits if the file pointer returns NULL.
       exit(1);
   }

    while (fgets(line, sizeof(line), file)) {
        // note that fgets don't strip the terminating \n, checking its
        //   presence would allow to handle lines longer that sizeof(line) 
        printf("%s", line); 

    }

   fclose(file); 

   char buf1[] ="abc/qwe/ccd";
   char *buf2;
   buf2 = (char*) calloc(1, sizeof(char));

   int k=0, x=0;
   
    while (i<15)
    {
      if (buf1[i]=='/')
      {printf("%c \n", buf1[i]);
      j=0; 
      k++;}
      else
     { 
    // char mychar = buf1[i];
      x++;
      realloc(buf2, sizeof(char)*x);
     // strcat(buf2, mychar);
       } 
     i++;
    }




for (i=0; i<10; i++)
{
 for(j=0; j<10; j++)
{
 printf(" %f ", *(a+i*10+j));
}
 printf("\n b=%d \n", *(b+i));

}

int *ww, p28, *flist2;

p28=pow(2,8);
int cnf, j2, z;
cnf=4*4;

double *xmax2, *xmin2;

//ww = (int*) malloc(cnf*p28*sizeof(int));


mbrfern(a, b, 10, 10, 4, 4, xmin2, xmax2, flist2, ww );

printf ("%s %f %f", "ok", mx, mn );



return 0;

}
 */



