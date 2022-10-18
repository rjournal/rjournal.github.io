#---------------------------------------------------------------------------#
#  The following e2p function returns the i-th elementary symmetric         #
#  polynomial e_i in terms in terms ofthe power sum symmetric polynomials   # 
#  p1,..., pi,  using (44) and the MFB function.                            #
#---------------------------------------------------------------------------#

library(kStatistics)

e2p <-
  function(n=0) {
    v<-MFB(n,1);                              # Call the MFB Function
    v<-MFB2Set( v );                          # Expression to vector
    for (j in 1:length(v)) {
      # ----- read -----------[ fix block ]---#-----------------------------#
      c <- as.character(v[[j]][2]);           # coefficient
      x <-              v[[j]][3] ;           # variable
      i <-              v[[j]][4] ;           # subscript
      k <-       strtoi(v[[j]][5]);           # power
      # ----- change -------------------------#-----------------------------#
      if (x=="f") {
        c<-paste0(c,"*( (-1)^",n,")");
        x<-"";
        i<-"";
      }
      else if (x=="g") {
        c<-paste0(c,"*((-factorial(",strtoi(i)-1,"))^",k,")");
        x<-paste0("(p",i,ifelse(k>1,paste0("^",k),""),")");
        i<-"";k<-1;
      }
      # ----- write ---------[ fix block ]----#-----------------------------#
      v[[j]][2] <- c;
      v[[j]][3] <- x;
      v[[j]][4] <- i;
      v[[j]][5] <- k;
      # --------------------------------------#-----------------------------#
    }
    noquote(paste0("1/",factorial(n),"(",Set2expr(v), " )"));
  }
 #------------
 # Example 25
 #------------
 e2p(4)