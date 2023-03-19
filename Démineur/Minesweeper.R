game <- function(nb_rows,nb_cols,prob){
  
  # Initialisation
  
  w=1;
  dev.new(10, 10)
  wsize=dev.size('px');
  dpi=wsize[1]/10
  dev.off()
  
  # Création d'une fenètre de jeu :
  
  dev.new(width = 650 / dpi, height = 650 / dpi / nb_cols * (8 + nb_rows))
  
  # Configuration de la zone de plot, titre et instruction : 
  
  par(mar=c(1,1,1,1))
  plot(c(0,nb_cols*w),c(0-5,(nb_rows+2)*w),type='n',ann=FALSE , axes=FALSE)
  str_instruct='Clic gauche pour creuser.\nClic droit pour mettre un drapeau'
  text(0,-4,str_instruct,font=1,adj=c(0,0),cex=1.2)
  text(nb_cols/2,(nb_rows +2),'Démineur',font=2, cex=2.5)
  text(nb_cols/2*0.995,(nb_rows+2 )*0.995,'Démineur',font=2, cex=2.5,col='black')
  
  # Préparation des bombes :
  
  theta=seq(1,100)/100 *2*3.1415935;
  curIrclex=cos(theta);
  curIrcley=sin(theta);
  rm(theta)
  
  # Création de la grille :
  
  plotij<-function(i,j,col){
    
    i=i-1;
    j=j-1;
    r=0.1*w;
    
    if (col=='close'){
      polygon(c(i*w +r ,i*w +r,(i+1)*w -r,(i+1)*w -r ), 
              c(nb_rows*w-j*w - r,nb_rows*w-(j+1)*w +r,nb_rows*w-(j+1)*w +r ,nb_rows*w-j*w -r),
              col='#BBBBBB',border=NA )    
      polygon(c(i*w ,i*w,i*w+r, i*w+r, (i+1)*w-r,(i+1)*w ), 
              c(nb_rows*w-j*w ,nb_rows*w-(j+1)*w, nb_rows*w-(j+1)*w +r ,nb_rows*w-j*w -r,nb_rows*w-j*w -r,nb_rows*w-j*w ),
              col='#F8F8F8',border=NA ) 
      polygon(c(i*w ,(i+1)*w,(i+1)*w, (i+1)*w-r, (i+1)*w-r,i*w+r ), 
              c(nb_rows*w-(j+1)*w ,nb_rows*w-(j+1)*w, nb_rows*w-j*w  ,nb_rows*w-j*w-r, nb_rows*w-(j+1)*w+r,nb_rows*w-(j+1)*w +r),
              col='#999999',border=NA )   
    } else if (col=='open'){
      polygon(c(i*w  ,i*w ,(i+1)*w ,(i+1)*w  ), 
              c(nb_rows*w-j*w ,nb_rows*w-(j+1)*w ,nb_rows*w-(j+1)*w  ,nb_rows*w-j*w ),
              col='#BBBBBB',border='#777777' )    
    } else if (col=='bomb'){
      polygon(c(i*w  ,i*w ,(i+1)*w ,(i+1)*w  ), 
              c(nb_rows*w-j*w ,nb_rows*w-(j+1)*w ,nb_rows*w-(j+1)*w  ,nb_rows*w-j*w ),
              col='#BBBBBB',border='#777777' )    
      polygon( ((i+1)-0.5)*w  + curIrclex * w/4  , (nb_rows-(j+1)+0.5)*w  + curIrcley * w/4, col='black')
      lines( c( ((i+1)-0.5)*w, ((i+1)-0.5)*w+w/3) , c( (nb_rows-(j+1)+0.5)*w ,(nb_rows-(j+1)+0.5)*w + w/3 ))
      lines( c( ((i+1)-0.5)*w+w/3, ((i+1)-0.5)*w+w/3 +w/12  ) , c( (nb_rows-(j+1)+0.5)*w + w/3,(nb_rows-(j+1)+0.5)*w + w/3 -w/12 ) )
    } else if (col=='curbomb'){
      polygon(c(i*w  ,i*w ,(i+1)*w ,(i+1)*w  ), 
              c(nb_rows*w-j*w ,nb_rows*w-(j+1)*w ,nb_rows*w-(j+1)*w  ,nb_rows*w-j*w ),
              col='#FF0000',border='#777777' )   
      polygon( ((i+1)-0.5)*w  + curIrclex * w/4  , (nb_rows-(j+1)+0.5)*w  + curIrcley * w/4, col='black')
      lines( c( ((i+1)-0.5)*w, ((i+1)-0.5)*w+w/3) , c( (nb_rows-(j+1)+0.5)*w ,(nb_rows-(j+1)+0.5)*w + w/3 ))
      lines( c( ((i+1)-0.5)*w+w/3, ((i+1)-0.5)*w+w/3 +w/12 ) , c( (nb_rows-(j+1)+0.5)*w + w/3,(nb_rows-(j+1)+0.5)*w + w/3 -w/12 ) )
    } else if (col=='down'){
      polygon(c(i*w  ,i*w ,(i+1)*w ,(i+1)*w  ), 
              c(nb_rows*w-j*w ,nb_rows*w-(j+1)*w ,nb_rows*w-(j+1)*w  ,nb_rows*w-j*w ),
              col='#BBBBBB',border='#AAAAAA' )    
    } else if (col=='flag'){
      polygon(c(i*w +r ,i*w +r,(i+1)*w -r,(i+1)*w -r ), 
              c(nb_rows*w-j*w - r,nb_rows*w-(j+1)*w +r,nb_rows*w-(j+1)*w +r ,nb_rows*w-j*w -r),
              col='#BBBBBB',border=NA )    
      polygon(c(i*w ,i*w,i*w+r, i*w+r, (i+1)*w-r,(i+1)*w ), 
              c(nb_rows*w-j*w ,nb_rows*w-(j+1)*w, nb_rows*w-(j+1)*w +r ,nb_rows*w-j*w -r,nb_rows*w-j*w -r,nb_rows*w-j*w ),
              col='#F8F8F8',border=NA )   
      polygon(c(i*w ,(i+1)*w,(i+1)*w, (i+1)*w-r, (i+1)*w-r,i*w+r ), 
              c(nb_rows*w-(j+1)*w ,nb_rows*w-(j+1)*w, nb_rows*w-j*w  ,nb_rows*w-j*w-r, nb_rows*w-(j+1)*w+r,nb_rows*w-(j+1)*w +r),
              col='#999999',border=NA )   
      
      xc=(i+0.5)*w - w/6;
      yc=(nb_rows-(j+1)+0.5)*w;
      a=w/2;
      b=w/9;
      c=w/4;
      
      polygon(c( xc  ,xc, xc+a, xc+a  ), 
              c( yc+b+c, yc+b,yc+b,yc+b+c ),
              col='#FF0000',border=NA )   
      
      d=w/4;
      lines( c( xc,xc ) , c(yc+b+c,yc-d) )
      e=w/3;
      yc=yc-d
      f=w/7;
      xc=(i+0.5)*w 
      
      polygon(c( xc-e  ,xc -e, xc+e, xc+e  ), 
              c( yc, yc-f,yc-f,yc ),
              col='#000000',border=NA )   
    }  
  }

  plotbutton <- function (xloc,yloc,w1,w2,r,str) {
    
    polygon(c(xloc +r ,xloc +r,xloc+w1 -r,xloc+w1 -r ), 
            c(yloc - r,yloc-w2 +r,yloc-w2 +r ,yloc -r),
            col='#BBBBBB',border=NA )
    polygon(c(xloc ,xloc,xloc+r, xloc+r, xloc+w1-r,xloc+w1 ), 
            c(yloc ,yloc-w2, yloc-w2 +r ,yloc  -r,yloc  -r,yloc  ),
            col='#F0F0F0',border=NA )  
    polygon(c(xloc ,xloc+w1,xloc+w1, xloc+w1-r, xloc+w1-r,xloc+r ), 
            c(yloc-w2 ,yloc-w2, yloc  ,yloc -r, yloc-w2+r,yloc-w2 +r),
            col='#999999',border=NA )  
    text(xloc+w1/2-0.01,yloc-w2/2-0.01,str,font=1,cex=1.2,col='#000000')
    text(xloc+w1/2,yloc-w2/2,str,font=1,cex=1.2,col='#FF0000')
  }
  
  # Initialisations des variables :
  
  img=matrix(as.numeric(runif(nb_cols*nb_rows) > prob ),nrow=nb_rows);
  num=img*0;
  
  # Comptage des bombes :
  
  for (i in 1:nb_rows){
    for (j in 1:nb_cols){ 
      num[i,j]= sum( img[ max((i-1),1):min((i+1),nb_rows), max((j-1),1):min((j+1),nb_cols)]) 
      }
  left=img*0;
  right=img*0;
  process0=img*0;
  taskFailure=0;
  taskFinished=0;
  }
  
  for (i in 1:nb_cols){
    for (j in 1:nb_rows){
      plotij(i,j, 'close')  
    }
  }
  curI=0;
  curJ=0;
  curButton=0;
  
  clickRestart=FALSE;
  clickQuit=FALSE
  bnum=paste('Bombes Restante :', formatC(sum(img)-sum(right),format='d',digits=3),'  '  ) ;
  legend(nb_cols/2, -3.5,bnum,cex=1.2,text.col="blue", box.col="red",bg="yellow")   
  
  # Configuration Bouton : 
   
  w1=5;
  w2=2;
  r=0.3
  
  plotbutton(nb_cols*3/4,-1,w1,w2,r,'Quitter')
  plotbutton(nb_cols*2/4,-1,w1,w2,r,'Recommencer')
  
  mousedown <-function(button,x,y){
    
    x=grconvertX(x, "ndc", "user")
    y=grconvertY(y, "ndc", "user")
    cat("Buttons ", paste(button, collapse=" "), " at ", x, y, "\n")  
    
    if ( !(x < (nb_cols*w) & x > 0 & y < (nb_rows*w) & y > 0 ) ){
      x0=nb_cols/2
      y0=-1
      w1=5;
      w2=2;
      r=0.3
      
      # Bouton recommencer :
      
      if ( x > (x0+r) & x < (x0+w1-r) & y < (y0-r) & y > (y0-w2+r)){           
        clickRestart<<-TRUE
        xloc=x0
        yloc=y0            
        polygon(c(xloc +r ,xloc +r,xloc+w1 -r,xloc+w1 -r ), 
                c(yloc - r,yloc-w2 +r,yloc-w2 +r ,yloc -r),
                col='#AA9999',border=NA )
        text(xloc+w1/2-0.01,yloc-w2/2-0.01,'Restart',font=1,cex=.7,col='#000000')
      }
      x0=nb_cols*3/4
      y0=-1
      w1=5;
      w2=2;
      r=0.3
      
      # Bouton quitter :
      
      if ( x > (x0+r) & x < (x0+w1-r) & y < (y0-r) & y > (y0-w2+r)){
        clickQuit<<-TRUE
        xloc=x0
        yloc=y0            
        polygon(c(xloc +r ,xloc +r,xloc+w1 -r,xloc+w1 -r ), 
                c(yloc - r,yloc-w2 +r,yloc-w2 +r ,yloc -r),
                col='#AA9999',border=NA )
        text(xloc+w1/2-0.01,yloc-w2/2-0.01,'Quit',font=1,cex=.7,col='#000000')
      }
      return(NULL)
    }
    
    if (taskFailure ==1 | taskFinished==1){return(NULL)}
    curI<<- ceiling(x/w);
    curJ<<- ceiling((nb_rows*w-y)/w);
    curButton<<-button;
    
    if (left[curJ,curI] == 0 & button[1] !=1){ 
      plotij(curI,curJ,'down')
    }
    return(NULL)
  }
  
  mouseup <-function(button,x,y){
    
    x=grconvertX(x, "ndc", "user")
    y=grconvertY(y, "ndc", "user")
    button=curButton;
    cat("Buttons ", paste(button, collapse=" "), " at ", x, y, "\n")
    
    # Bouton Recommencer
    
    if (clickRestart){
      clickRestart<<-FALSE
      
      # Initialisation des variables :
      
      img<<-matrix(as.numeric(runif(nb_cols*nb_rows) > prob ),nrow=nb_rows);
      num<<-img*0;
      
      # Comptage des Bombes : 
      
      for (i in 1:nb_rows){
        for (j in 1:nb_cols){ num[i,j]<<- sum( img[ max((i-1),1):min((i+1),nb_rows)      ,  max((j-1),1):min((j+1),nb_cols)     ]) 
        }
      }
      
      left<<-img*0;
      right<<-img*0;
      process0<<-img*0;
      taskFailure<<-0;
      taskFinished<<-0;
      
      for (i in 1:nb_cols){
        for (j in 1:nb_rows){
          plotij(i,j, 'close' )  
        }
      }
      
      curI<<-0;
      curJ<<-0;
      curButton<<-0;
      
      w1=5;
      w2=2;
      r=0.3
      plotbutton(nb_cols*3/4,-1,w1,w2,r,'Quitter')
      plotbutton(nb_cols*2/4,-1,w1,w2,r,'Recommencer')
      
      bnum=paste('Bombes Restante :', formatC(sum(img)-sum(right),format='d',digits=3),'  '  ) ;
      legend(nb_cols/2, -3.5,bnum,cex=1.2,text.col="blue", box.col="red",bg="yellow")   
      
      return(NULL)
    }
    if (clickQuit){
      dev.off()
    }
    if ( !(x < (nb_cols*w) & x > 0 & y < (nb_rows*w) & y > 0 )){      
      return(NULL)
    }
    if (taskFailure ==1 | taskFinished==1){return(NULL)}
    
    i= ceiling(x/w);
    j= ceiling((nb_rows*w-y)/w);
    
    if (i!=curI |j !=curJ ){
      cat('bbbb\n')
      if (left[curJ,curI] ==0){ 
        plotij(curI,curJ,'close')
        if (right[curJ,curI] ==0){ 
          plotij(curI,curJ,'close')
          }
        else if (right[curJ,curI] ==1){
          plotij(curI,curJ,'flag')
          }
      }
      return(NULL)
    }
    if (button[1]==0){
      if (right[j,i]==1 ){
        plotij(i,j,'flag')
        return(NULL)
        }
      if ( left[j,i]==0 ){
        if (img[j,i]==0){
          plotij(i,j,'open'); plottext(i,j)
          left[j,i]<<-1;
          
          if (num[j,i] ==0){
            process0[j,i]<<-1;
            hitzero(i,j)
            }
        }
        else{
          idx=which(img==1);
          rowlist= row(img)[idx];
          collist= col(img)[idx];
          for (n in 1:length(idx)){ 
            cat('aa ', rowlist[n],collist[n], '\n')
            plotij(collist[n],rowlist[n],'bomb')
          }
          plotij(i,j,'curbomb')
          taskFailure<<-1;
          text(nb_cols/2,nb_rows/2.,'Game over!',font=2,cex=3.5,col='red')
        }
      }
    }
    else if (button[1]==2){
      if (left[j,i]==0){
        if (right[j,i]==0){
          plotij(i,j,'flag')
          right[j,i]<<-1;
          if( all(img==right)){
            taskFinished<<-1;
            text(nb_cols/2,nb_rows/2.,'Victoire !',font=2,cex=3.5, col='green')
          }
          bnum=paste('Bombes Restante :', formatC(sum(img)-sum(right),format='d',digits=3),'  '  ) ;
          legend(nb_cols/2, -3.5,bnum,cex=1.2,text.col="blue", box.col="red",bg="yellow")   
        }
        else{
          plotij(i,j,'close')
          right[j,i]<<-0;
          bnum=paste('Bombes Restante :', formatC(sum(img)-sum(right),format='d',digits=3),'  '  ) ;
          legend(nb_cols/2, -3.5,bnum,cex=1.2,text.col="blue", box.col="red",bg="yellow")  
        }
      }
    }
    else if (button[1]==1){
      if (left[j,i]==1){
        hitflag(i,j)
      }
    }
    return(NULL)
  }

  plottext<-function (i,j){
    if (num[j,i] > 0){
      idx=num[j,i];
      idx=min(3,idx);
      col=c('blue','green','red')
      text( (i-0.5)*w ,(nb_rows-j+0.5)*w, toString(num[j,i]),col=col[idx], font=2 )
    }
  }

  hitzero <- function(i,j){    
    for(M in -1:1){
      for (N in -1:1){
        curJ=(j+M);
        curI=(i+N);
        if (M ==0 & N==0){next}
        if (curJ < 1 | curJ > nb_rows | curI <1 | curI > nb_cols){next}
        if (right[curJ,curI] ==0){
          plotij(curI,curJ,'open') ; plottext(curI,curJ)       
          left[curJ,curI]<<-1;       
        }
        if (num[curJ,curI]==0 &  process0[curJ,curI]==0){
          process0[curJ,curI]<<-1;
          hitzero(curI,curJ)}
      }
    }   
  }
  
  hitflag <- function (i,j){    
    imgsub=img[ max(j-1,1):min(j+1,nb_rows) ,  max(i-1,1):min(i+1,nb_cols)        ]
    rightsub=right[ max(j-1,1):min(j+1,nb_rows) ,  max(i-1,1):min(i+1,nb_cols)        ]
    if (all(imgsub==rightsub) & max(imgsub) == 1)
    {
      for(M in -1:1){
        for (N in -1:1){
          curJ=(j+M);
          curI=(i+N);
          if (M ==0 & N==0) {next}
          if (curJ < 1 | curJ > nb_rows | curI <1 | curI > nb_cols) {next}
          if (right[curJ,curI] == 0){
            plotij(curI,curJ,'open') ; plottext(curI,curJ)       
            left[curJ,curI]<<-1;       
          }
        }
      }
    }
  }
  getGraphicsEvent("Démineur",
                   onMouseDown = mousedown,
                   onMouseUp= mouseup,)
}

#########################################################################################################

# Gestion de la difficulté : 

Minesweeper <- function(n){
  if (n == 5) {
    game(10,20,0.01) # Luck
  } else if (n == 4){
    game(10,20,0.7) # Expert
  } else if (n == 3){
    game(10,20,0.8)  # hard
  } else if (n == 2){
    game(10,20,0.9) # Medium
  } else {
    game(10,20,0.95) # Easy
  }
}
Minesweeper(1)
