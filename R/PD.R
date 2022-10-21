
#' Polycross Designs
#'
#' @param v v is number of genotypes
#' @param type type used for generating designs
#'@author
#'
#'1) Cini Varghese, Division of Design of Experiments, ICAR-IASRI, New Delhi.
#'
#'2) Seema Jaggi, Agricultural Education Division, ICAR, Krishi Anusandhan Bhawan - II, Pusa, New Delhi.
#'
#'3) Eldho Varghese, Fishery Resources Assessment & Economics Division, ICAR-CMFRI, Kochi.
#'
#'4) Ashutosh Dalal, Division of Design of Experiments, ICAR-IASRI, New Delhi.
#'
#'5) Arpan Bhowmik, Division of Design of Experiments, ICAR-IASRI, New Delhi.

#' @return This function generates polycross designs using various methods for a given number of genotypes (v).
#' @export
#'@description This package contains function for generating polycross designs using nine methods available in literature.
#'
#'type1: v(>2) such that (v+1) is a prime number;
#'
#'type1 is a series of polycross designs generated using the method given by Olesen and Olesen (1973).
#'
#'type2: v (>2) is taken such that (v+1) is a prime number;
#'
#'type2 is a series of completely balanced polycross designs generated using the method given by Olesen (1976).
#'
#'type3: v(>2) is any integer;
#'
#'type3 is a series of polycross designs with complete neighbour balance generated using the method given by Morgan (1988).
#'
#'type4: v=5,7,9 or 11.
#'
#'type4 is a series of balanced polycross designs given by Morgan(1988).
#'
#'type5: v (>2) is taken in such a way that (v+1) is a prime number;
#'
#'type5 is a series of Neighbour-balanced polycross designs for v genotypes (where v+1 is prime) for polycross trials as given in Varghese et al. (2015).
#'
#'type6: v (>4) is any odd number;
#'
#'type6 is a series of Neighbour-balanced polycross designs for v genotypes (where v is an odd number) for polycross trials as given in Varghese et al. (2015).
#'
#'type7: v(>2)= 2*m, where, m is an odd positive integer;
#'
#'type7 is a series of Neighbour-restricted block designs for polycross trials as given in Varghese et al. (2015).
#'
#'type8: v = 4*m, where m is any positive integer;
#'
#'type8 is a series of Neighbour-restricted row–column designs for polycross trials as given in Varghese et al. (2015).
#'
#'type9: v is a prime number but (v-1) should be a multiple of 3;
#'
#'type9 is a series of Polycross designs for directional wind system for polycross trials as given in Varghese et al. (2015).
#' @examples
#'library(PolycrossDesigns)
#'PD(6,"type7")
#'@references
#'
#'1) Morgan (1988a)<doi: 10.1007/BF00025112>."Polycross Designs with Complete Neighbour Balance".
#'
#'2) Morgan (1988b)<doi:10.1111/j.2517-6161.1988.tb01714.x>."Balanced Polycross Designs".
#'
#'3) Olesen  and Olesen (1973)<https://doi.org/10.1007/BF00036647>. "A Polycross Pattern Formula".
#'
#'4) Olesen (1976)<https://doi.org/10.1007/BF00041582>."A Completely Balanced Polycross Design".
#'
#'5) Varghese et al. (2015)<doi:10.1080/02664763.2015.1043860>. " Experimental Designs for Open Pollination in Polycross Trials".

PD<-function(v,type){
  pme<-function(m){
    set<-c(2:(m/2))
    if(any(m%%set==0)){
      FALSE
    }else{
      TRUE
    }
  }

  ################
  if(type=="type7"){
    m=v/2
    if(v%%2==0 && m%%2!=0){

      i=1
      r<-matrix(1:(m),nrow=1,ncol=(m))
      r1<-matrix(1:(m),nrow=1,ncol=(m))
      while(i<=(m-1)){
        r2<-(r1+i)
        r<-rbind(r,r2)
        i=i+1
      }
      array1<-r%%(m)
      array1[array1==0]<-m

      ###############array2
      array2<-array1
      for(i in 2:m){
        array2[i,]<-array2[i-1,]+2
      }
      array2<-array2%%(m)
      array2[array2==0]<-m
      #####final design
      array2<-array2+m
      final<-matrix(,nrow=m,ncol=0)
      for(i in 1:m){
        final<-cbind(final,array1[,i],array2[,i])
      }
      final<-cbind(final[,2*m],"|",final,"|",final[,1])
      message("Neighbour Restricted Block Design for Polycross Trials (Varghese et al.,2015)")
      cat("\n")
      prmatrix(final,rowlab=rep("",m),collab=rep("",((2*m)+4)),quote=FALSE)
      cat("\n")
      print(c("Number of genotypes (v)=",2*m),quote=F)
      print(c("Number of blocks =",m),quote=F)
      print(c("Number of replication of genotypes =",m),quote=F)
      print(c("Block size =",2*m),quote=F)
      cat("\n")
      message("Note: Design along with border plots at either ends")
    }else{
      print("Please enter an even number, v(>2) where m(=v/2) is an odd number",quote=F)
    }
  }
  ##################################################
  if(type=="type8"){
    #Neighbour-restricted row–column designs
    if(v%%4!=0){
      print("Please enter a number which is a multiple of 4 ",quote=F)
    }else{
      message("Neighbour Restricted Row Column Design for polycross trials (Varghese et al.,2015)")
      cat("\n")
      m=v/4
      i=0
      j=1
      l=1
      aryno=1
      while(i<=(m-1)){
        j=1
        r1<-c()
        while(j<=(v/2)){
          if((1-j+(v-(2*i)))>v/2){
            x<-c(j,(1-j+(v-(2*i))))
          }else{
            x<-c(j,(1-j+(v-(2*i))+v/2))
          }
          r1<-c(r1,x)
          j=j+1
        }
        r1<-as.matrix(r1)
        r2<-c(r1[2:v],r1[1])
        r2<-as.matrix(r2)
        array1<-cbind(r1,r2)
        array1<-rbind(array1[v,],rep("-",times=2),array1,rep("-",times=2),array1[1,])
        array1<-as.matrix(array1)

        `colnames<-`(array1,NULL)
        print(c("Array", aryno))
        prmatrix(array1,rowlab=rep("",v+4),collab=rep("",2),quote=FALSE)
        cat("\n")
        aryno=aryno+1
        ######array2
        i=i+1
      }

      cat("\n")
      print(c("Number of genotypes (v)=",v),quote=F)
      print(c("Number of arrays =",v/4),quote=F)
      print(c("Number of replication of genotypes =",v/2),quote=F)
      print(c("Number of rows within each array =",v),quote=F)
      print(c("Number of columns within each array =",2),quote=F)

      cat("\n")
      message("Note: Design along with border plots at either ends")
    }

  }
  ###################################
  if(type=="type5"){
    #Neighbour-balanced polycross designs for v genotypes (where v+1 is a prime number)

    if(as.integer(pme(v+1))>0){

      message("Neighbour Balanced Polycross Design for v genotypes (Varghese et al.,2015)")
      cat("\n")
      aryno=1
      k<-c()
      for(i in 0:(v/2-1)){
        k<-c(k,2*i+1)
      }

      for(i in k){
        s1=i
        r1<-c()
        while(s1<=i*v){
          r1<-c(r1,s1)
          s1=s1+i
        }
        array<-matrix(,nrow=v,ncol=v)
        array[1,]<-r1%%(v+1)
        for(l in 2:v){
          array[l,]<-array[1,]+array[l-1,]
        }
        array<-array%%(v+1)
        print(c("Array",aryno))
        print(array)
        cat("\n")
        aryno=aryno+1
      }
      cat("\n")
      print(c("Number of genotypes (v)=",v),quote=F)
      print(c("Number of arrays =",v/2),quote=F)
      print(c("Number of replication of genotypes =",v*(v/2)),quote=F)
      print(c("Number of rows within each array=",v),quote=F)
      print(c("Number of columns within each array =",v),quote=F)
    }else{
      print("Please enter a even number v(>2) where v+1 is a prime number",quote=F)
    }


  }


  ##################

  if(type=="type6"){

    #Neighbour-balanced polycross designs for v genotypes (where v is a odd number)

    vv=(v-1)
    if(vv%%2==0 && v>4){
      message("Neighbour Balanced Polycross Design for v genotypes (Varghese et al.,2015)")
      cat("\n")
      g<-function(i,j,v){
        m=(((-1)^((v+1)/2))*((v+1)/2))%%v
        if(j<=m){
          return(((-1)^(i))*(as.integer(j/2)))

        }
        if(j>m && j<=(v-1)){
          return(((-1)^(i))*(as.integer((j+1)/2)))
        }
        if(j==v){
          return(v)
        }
      }
      ################
      arrayelements<-c()
      for(i in 1:v){
        for(r in 1:(v-2)){
          for(c in 1:v){
            cc=(g(r,r,v)+g(c,c,v)+i)%%v
            cc[cc==0]<-v
            arrayelements<-c(arrayelements,cc)
          }
        }
      }
      arrayelements<-matrix(arrayelements,nrow=v*(v-2),byrow=T)
      i=1
      m=1
      arry<-list()
      while(i<=nrow(arrayelements)){
        j=i+(v-3)

        arry<-(arrayelements[i:j,])
        print(c("Array",m))
        print(arry)
        cat("\n")
        i=i+(v-2)
        m=m+1

      }
      cat("\n")
      print(c("Number of genotypes (v)=",v),quote=F)
      print(c("Number of arrays =",v),quote=F)
      print(c("Number of replication of genotypes =",v*(v-2)),quote=F)
      print(c("Number of rows within each array =",(v-2)),quote=F)
      print(c("Number of columns within each array =",v),quote=F)

    }else{
      print("Please enter an odd number (v>4)",quote=F)
    }

  }
  #####################################
  if(type=="type9"){
    ###Polycross designs for directional wind system
    #prime    #(v-1) multiple of 3
    ######
    if(as.integer(pme(v))>0 && ((v-1)%%3)==0){
      message("Polycross Design for Directional Wind System (Varghese et al.,2015)")
      cat("\n")
      aryno=1
      ###########
      if(v==7){
        diff<-c(3,4)
      }else{
        o=3
        diff<-c(3,4)
        while(o<=((v-1)/3)){
          #diff<-c()
          if(o%%2!=0){
            x<-diff[o-1]+5
          }else{
            x<-diff[o-1]+1
          }
          diff<-c(diff,x)
          o=o+1
        }
      }
      ###############
      ###odd number generate
      s=0
      odd<-c()
      while(s<=(((v-1)/2)-1)){
        x<-2*s+1
        odd<-c(odd,x)
        s=s+1
      }
      ###########
      odd1<-odd
      s1=1
      while(s1<=(((v-1)/3))){
        s2=1
        odd<-odd1
        array<-matrix(,nrow=length(odd1),ncol=0)
        while(s2<=(v-1)){
          odd<-odd+diff[s1]
          array<-cbind(array,odd)
          array<-array%%v
          array[array==0]<-7
          s2=s2+1
        }
        array<-cbind(odd1,array,odd1)
        colnames(array)<-NULL
        print(c("Array",aryno))
        final<-cbind(array[,1:v],"|",array[,v+1])
        prmatrix(final,rowlab=rep("",nrow(final)),collab=rep("",ncol(final)),quote=FALSE)
        cat("\n")
        s1=s1+1
        aryno=aryno+1
      }
      cat("\n")
      print(c("Number of genotypes (v)=",v),quote=F)
      print(c("Number of arrays =",(v-1)/3),quote=F)
      print(c("Number of replication of genotypes =",((v-1)/2)*((v-1)/3)),quote=F)
      print(c("Number of rows within each array =",(v-1)/2),quote=F)
      print(c("Number of columns within each array =",v),quote=F)
      cat("\n")
      message("Note: Design along with right end border plots")
    }else{
      print("Please enter a prime number(v) where (v-1) is multiple of 3.",quote=F)
    }

  }
  ##################

  if(type=="type1"){
    #Olesen and Olesen method (for n clones but n+1 prime)
    if(as.integer(pme(v+1))>0){
      row<-matrix(,nrow=v,ncol=v)
      r1<-matrix(1:v,nrow=1,ncol=v)
      row[1,]<-r1
      for(i in 2:v){
        row[i,]<-(row[1,]+row[i-1,])
      }
      message("Neighbour Balanced Polycross Design (Olesen and Olesen,1973)")
      cat("\n")
      print(row%%(v+1))
      cat("\n")
print(c("Number of clones (n)=",v),quote=F)
print(c("Number of replication of clones=",v),quote=F)
print(c("Number of rows =",v),quote=F)
print(c("Number of columns =",v),quote=F)
cat("\n")
    }else{
      print("Please enter a number (v>2) where (v+1) is prime",quote=FALSE)
    }
  }
  ##################

  #######################################
  if(type=="type3"){
    if(v>2){
    #Morgan method for Odd numbers + even numbers
    vv=(v-1)
    if(vv%%2==0 && v>=3){
      message("Completely Neighbour Balanced Polycross Design (Morgan,1988)")
      cat("\n")
      g<-function(i,j,v){
        m=(((-1)^((v+1)/2))*((v+1)/2))%%v
        if(j<=m){
          return(((-1)^(i))*(as.integer(j/2)))

        }
        if(j>m && j<=(v-1)){
          return(((-1)^(i))*(as.integer((j+1)/2)))
        }
        if(j==v){
          return(v)
        }
      }
      ################
      arrayelements<-c()
      for(i in 1:v){
        for(r in 1:(v)){
          for(c in 1:v){
            cc=(g(r,r,v)+g(c,c,v)+i)%%v
            cc[cc==0]<-v
            arrayelements<-c(arrayelements,cc)
          }
        }
      }
      arrayelements<-matrix(arrayelements,nrow=v*v,byrow=T)
      i=1
      m=1
      arry<-list()
      while(i<=nrow(arrayelements)){
        j=i+(v-1)
        arry<-(arrayelements[i:j,])
        print(c("Array",m))
        print(arry)
        cat("\n")
        i=j+1
        m=m+1
        #m=m+1
      }

    }
    # else{
    #   print("Please enter an integer number where v(>2)",quote=F)
    # }
    ######################################
    #############

    #Morgan method for even numbers
    vv=(v-2)
    if(vv%%2==0 && v>=3){
      h<-function(i,j,v){
        return((((-1)^(i))*(as.integer(j/2)))%%v)
      }
      ################
      arrayelements<-c()
      for(i in 1:v){
        for(r in 1:(v)){
          for(c in 1:v){
            cc=(h(r,r,v)+h(c,c,v)+i)%%v
            cc[cc==0]<-v
            arrayelements<-c(arrayelements,cc)
          }
        }
      }
      arrayelements<-matrix(arrayelements,nrow=v*v,byrow=T)
      i=1
      m=1
      arry<-list()
      while(i<=nrow(arrayelements)){
        j=i+(v-1)
        arry<-(arrayelements[i:j,])
        print(c("Array",m))
        print(arry)
        cat("\n")
        i=j+1
        m=m+1
        #m=m+1
      }

      # }else{
      #
      # }
    }
    cat("\n")
    print(c("Number of clones (n)=",v),quote=F)
    print(c("Number of arrays =",v),quote=F)
    print(c("Number of rows within each array =",v),quote=F)
    print(c("Number of columns within each array =",v),quote=F)
    cat("\n")
    }else{
      print("Please enter an integer number where v(>2)",quote=F)
    }
  }
  #####################
  if(type=="type2"){
    #olesen 1976  method (v+1) is prime

    if(as.integer(pme(v+1))>0 && v>2){
      message("Completely Balanced Polycross Design (Olesen,1976)")
      cat("\n")
      for(i in 1:v){
        mat<-matrix(0,nrow=v,ncol=v)
        x<-seq(from=i, to=(i*v),by=i)
        mat[1,]<-x
        for(j in 2:v){
          mat[j,]<-(mat[1,]+mat[j-1,])
        }
        mat<-mat%%(v+1)
        print(c("Array", i))
        print(mat)
        cat("\n")
      }
      cat("\n")
      print(c("Number of clones (n)=",v),quote=F)
      print(c("Number of arrays =",v),quote=F)
      print(c("Number of replication of clones=",v*v),quote=F)
      print(c("Number of rows within each array=",v),quote=F)
      print(c("Number of columns within each array=",v),quote=F)

    }else{
      print("Please enter a number (v>2) where (v+1) is prime", quote = F)
    }

  }
  ##################
  if(type=="type4"){
    ####Morgan new method (prime)
    xx<-c(5,7,9,11)
    if(any(xx==v)){
      message("Balanced Polycross Design (Morgan,1988)")
      cat("\n")
      if(v==5){
        mat<-matrix(c(0,1,3,1,2,4,3,4,1),nrow=3,byrow=T)

      }
      ##################
      if(v==7){
        mat<-matrix(c(0,1,6,3,1,2,0,4,3,4,2,6,6,0,5,2),nrow=4,byrow=T)
      }
      ##################
      if(v==9){
        mat<-matrix(c(0,1,3,7,4,1,2,4,8,5,6,7,0,4,1,4,5,7,2,8,7,8,1,5,2),nrow=5,byrow=T)

      }
      ############
      if(v==11){
        mat<-matrix(c(0,1,4,9,5,3,1,2,5,10,6,4,9,10,2,7,3,1,7,8,0,5,1,10,3,4,7,1,8,6,8,9,1,6,2,0),nrow=6,byrow=T)

      }
      for(i in 1:v){
        print(c("Array", i))
        zx<-(mat+(i-1))%%v
        zx[zx==0]<-v
        print(zx)
        cat("\n")
      }

      cat("\n")
      print(c("Number of genotypes (v)=",v),quote=F)
      print(c("Number of rows within each array =",nrow(mat)),quote=F)
      print(c("Number of columns within each array =",ncol(mat)),quote=F)
    }else{
      print("Please enter a number,v=5,7,9 or 11",quote=F)
    }
  }

}

