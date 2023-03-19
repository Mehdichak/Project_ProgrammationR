mygame <- function(ny,nx,prob){
  
  # Paramètre (reste égal à 1)
  
  w=1;
  
  # Initialisation
  
  dev.new(10, 10)
  wsize=dev.size('px');
  dpi=wsize[1]/10
  dev.off()
  
  # Création d'une fenètre de jeu :
  
  sysName=Sys.info()['sysname']
  {
    if (toupper(substr(sysName,1,3)) == 'LIN' ){
      dev.new(width=650/dpi, height=650/dpi/nx*(8+ny))
    }
    else if (toupper(substr(sysName,1,3)) == 'DAR'){
      dev.new(width=650/dpi, height=650/dpi/nx*(8+ny))
    }
    else {
      dev.new(width=650/dpi, height=650/dpi/nx*(8+ny))
    }
  }
  
  # Configuration de la zone de plot
  
  par(mar=c(1,1,1,1))
  plot(c(0,nx*w),c(0-5,(ny+2)*w),type='n',ann=FALSE , axes=FALSE)
  str_instruct='Clic gauche pour creuser.\nClic droit pour mettre un drapeau'
  text(0,-4,str_instruct,font=1,adj=c(0,0),cex=1.2)
  text(nx/2,(ny +2),'Démineur',font=2, cex=2.5)
  text(nx/2*0.995,(ny+2 )*0.995,'Démineur',font=2, cex=2.5,col='red')
  
  
  # Préparation des bombes :
  
  theta=seq(1,100)/100 *2*3.1415935;
  curIrclex=cos(theta);
  curIrcley=sin(theta);
  rm(theta)
  
  ##################################################################################################################################
  
  plotij<-function(i,j,col){
    
    i=i-1;
    j=j-1;
    r=0.1*w;
    
    if (col=='close'){
      polygon(c(i*w +r ,i*w +r,(i+1)*w -r,(i+1)*w -r ), 
              c(ny*w-j*w - r,ny*w-(j+1)*w +r,ny*w-(j+1)*w +r ,ny*w-j*w -r),
              col='#BBBBBB',border=NA )    
      polygon(c(i*w ,i*w,i*w+r, i*w+r, (i+1)*w-r,(i+1)*w ), 
              c(ny*w-j*w ,ny*w-(j+1)*w, ny*w-(j+1)*w +r ,ny*w-j*w -r,ny*w-j*w -r,ny*w-j*w ),
              col='#F8F8F8',border=NA ) 
      polygon(c(i*w ,(i+1)*w,(i+1)*w, (i+1)*w-r, (i+1)*w-r,i*w+r ), 
              c(ny*w-(j+1)*w ,ny*w-(j+1)*w, ny*w-j*w  ,ny*w-j*w-r, ny*w-(j+1)*w+r,ny*w-(j+1)*w +r),
              col='#999999',border=NA )   
    } else if (col=='open'){
      polygon(c(i*w  ,i*w ,(i+1)*w ,(i+1)*w  ), 
              c(ny*w-j*w ,ny*w-(j+1)*w ,ny*w-(j+1)*w  ,ny*w-j*w ),
              col='#BBBBBB',border='#777777' )    
    } else if (col=='bomb'){
      polygon(c(i*w  ,i*w ,(i+1)*w ,(i+1)*w  ), 
              c(ny*w-j*w ,ny*w-(j+1)*w ,ny*w-(j+1)*w  ,ny*w-j*w ),
              col='#BBBBBB',border='#777777' )    
      polygon( ((i+1)-0.5)*w  + curIrclex * w/4  , (ny-(j+1)+0.5)*w  + curIrcley * w/4, col='black')
      lines( c( ((i+1)-0.5)*w, ((i+1)-0.5)*w+w/3) , c( (ny-(j+1)+0.5)*w ,(ny-(j+1)+0.5)*w + w/3 ))
      lines( c( ((i+1)-0.5)*w+w/3, ((i+1)-0.5)*w+w/3 +w/12  ) , c( (ny-(j+1)+0.5)*w + w/3,(ny-(j+1)+0.5)*w + w/3 -w/12 ) )
    } else if (col=='curbomb'){
      polygon(c(i*w  ,i*w ,(i+1)*w ,(i+1)*w  ), 
              c(ny*w-j*w ,ny*w-(j+1)*w ,ny*w-(j+1)*w  ,ny*w-j*w ),
              col='#FF0000',border='#777777' )   
      polygon( ((i+1)-0.5)*w  + curIrclex * w/4  , (ny-(j+1)+0.5)*w  + curIrcley * w/4, col='black')
      lines( c( ((i+1)-0.5)*w, ((i+1)-0.5)*w+w/3) , c( (ny-(j+1)+0.5)*w ,(ny-(j+1)+0.5)*w + w/3 ))
      lines( c( ((i+1)-0.5)*w+w/3, ((i+1)-0.5)*w+w/3 +w/12 ) , c( (ny-(j+1)+0.5)*w + w/3,(ny-(j+1)+0.5)*w + w/3 -w/12 ) )
    } else if (col=='down'){
      polygon(c(i*w  ,i*w ,(i+1)*w ,(i+1)*w  ), 
              c(ny*w-j*w ,ny*w-(j+1)*w ,ny*w-(j+1)*w  ,ny*w-j*w ),
              col='#BBBBBB',border='#AAAAAA' )    
    } else if (col=='flag'){
      polygon(c(i*w +r ,i*w +r,(i+1)*w -r,(i+1)*w -r ), 
              c(ny*w-j*w - r,ny*w-(j+1)*w +r,ny*w-(j+1)*w +r ,ny*w-j*w -r),
              col='#BBBBBB',border=NA )    
      polygon(c(i*w ,i*w,i*w+r, i*w+r, (i+1)*w-r,(i+1)*w ), 
              c(ny*w-j*w ,ny*w-(j+1)*w, ny*w-(j+1)*w +r ,ny*w-j*w -r,ny*w-j*w -r,ny*w-j*w ),
              col='#F8F8F8',border=NA )   
      polygon(c(i*w ,(i+1)*w,(i+1)*w, (i+1)*w-r, (i+1)*w-r,i*w+r ), 
              c(ny*w-(j+1)*w ,ny*w-(j+1)*w, ny*w-j*w  ,ny*w-j*w-r, ny*w-(j+1)*w+r,ny*w-(j+1)*w +r),
              col='#999999',border=NA )   
      
      xc=(i+0.5)*w - w/6;
      yc=(ny-(j+1)+0.5)*w;
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
  
  ##################################################################################################################################
  
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
  
  ##################################################################################################################################
  
  # Initialisations des variables :
  
  img=matrix(as.numeric(runif(nx*ny) > prob ),nrow=ny);
  num=img*0;
  
  # Comptage des bombes :
  
  for (i in 1:ny){
    for (j in 1:nx){ 
      num[i,j]= sum( img[ max((i-1),1):min((i+1),ny), max((j-1),1):min((j+1),nx)]) 
      }
  left=img*0;
  right=img*0;
  process0=img*0;
  taskFailure=0;
  taskFinished=0;
  }
  
  for (i in 1:nx){
    for (j in 1:ny){
      plotij(i,j, 'close')  
    }
  }
  curI=0;
  curJ=0;
  curButton=0;
  
  clickRestart=FALSE;
  clickQuit=FALSE
  bnum=paste('Bombes Restante :', formatC(sum(img)-sum(right),format='d',digits=3),'  '  ) ;
  legend(nx/2, -3.5,bnum,cex=1.2,text.col="blue", box.col="red",bg="yellow")   
  
  ##################################################################################################################################
  
  w1=5;
  w2=2;
  r=0.3
  plotbutton(nx*3/4,-1,w1,w2,r,'Quitter')
  plotbutton(nx*2/4,-1,w1,w2,r,'Recommencer')
  
  ##################################################################################################################################
  
  mousedown <-function(button,x,y){
    
    x=grconvertX(x, "ndc", "user")
    y=grconvertY(y, "ndc", "user")
    cat("Buttons ", paste(button, collapse=" "), " at ", x, y, "\n")  
    
    if ( !(x < (nx*w) & x > 0 & y < (ny*w) & y > 0 ) ){
      x0=nx/2
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
        text(xloc+w1/2,yloc-w2/2,'Restart',font=1,cex=.7,col='#FF0000')
      }
      x0=nx*3/4
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
        text(xloc+w1/2,yloc-w2/2,'Quit',font=1,cex=.7,col='#FF0000')
      }
      return(NULL)
    }
    
    if (taskFailure ==1 | taskFinished==1){return(NULL)}
    curI<<- ceiling(x/w);
    curJ<<- ceiling((ny*w-y)/w);
    curButton<<-button;
    
    if (left[curJ,curI] == 0 & button[1] !=1){ 
      plotij(curI,curJ,'down')
    }
    return(NULL)
  }
  
  ##################################################################################################################################
  
  mouseup <-function(button,x,y){
    
    x=grconvertX(x, "ndc", "user")
    y=grconvertY(y, "ndc", "user")
    button=curButton;
    cat("Buttons ", paste(button, collapse=" "), " at ", x, y, "\n")
    
    # Bouton Recommencer
    
    if (clickRestart){
      clickRestart<<-FALSE # Réinitialisation de clickResart
      
      #Initialize variables to store status of the grid 
      
      img<<-matrix(as.numeric(runif(nx*ny) > prob ),nrow=ny);
      num<<-img*0;
      
      # Comptage des Bombes : 
      
      for (i in 1:ny){
        for (j in 1:nx){ num[i,j]<<- sum( img[ max((i-1),1):min((i+1),ny)      ,  max((j-1),1):min((j+1),nx)     ]) 
        }
      }
      
      left<<-img*0;
      right<<-img*0;
      process0<<-img*0;
      taskFailure<<-0;
      taskFinished<<-0;
      
      for (i in 1:nx){
        for (j in 1:ny){
          plotij(i,j, 'close' )  
        }
      }
      
      curI<<-0;
      curJ<<-0;
      curButton<<-0;
      
      w1=5;
      w2=2;
      r=0.3
      plotbutton(nx*3/4,-1,w1,w2,r,'Quitter')
      plotbutton(nx*2/4,-1,w1,w2,r,'Recommencer')
      
      bnum=paste('Bombes Restante :', formatC(sum(img)-sum(right),format='d',digits=3),'  '  ) ;
      legend(nx/2, -3.5,bnum,cex=1.2,text.col="blue", box.col="red",bg="yellow")   
      
      return(NULL)
    }
    
    if (clickQuit){
      dev.off()
    }
    
    if ( !(x < (nx*w) & x > 0 & y < (ny*w) & y > 0 )){      
      return(NULL)
    }
    
    if (taskFailure ==1 | taskFinished==1){return(NULL)}
    i= ceiling(x/w);
    j= ceiling((ny*w-y)/w);
    
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
        return(NULL)}
      
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
          text(nx/2,ny/2.,'Game over!',font=2,cex=3.5)
          text(nx/2*0.99,ny/2*0.99,'Game over !',font=2,cex=3.5,col='#FF0000')
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
            text(nx/2,ny/2.,'Victoire !',font=2,cex=3.5)
            text(nx/2*0.99,ny/2*0.99,'Victoire !',font=2,cex=3.5,col='#FF0000')
          }
          bnum=paste('Bombes Restante :', formatC(sum(img)-sum(right),format='d',digits=3),'  '  ) ;
          legend(nx/2, -3.5,bnum,cex=1.2,text.col="blue", box.col="red",bg="yellow")   
        }
        else{
          plotij(i,j,'close')
          right[j,i]<<-0;
          bnum=paste('Bombes Restante :', formatC(sum(img)-sum(right),format='d',digits=3),'  '  ) ;
          legend(nx/2, -3.5,bnum,cex=1.2,text.col="blue", box.col="red",bg="yellow")  
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
  
  ##########################################################################################################
  
  plottext<-function (i,j){
    if (num[j,i] > 0){
      idx=num[j,i];
      idx=min(3,idx);
      col=c('blue','green','red')
      text( (i-0.5)*w ,(ny-j+0.5)*w, toString(num[j,i]),col=col[idx], font=2 )
    }
  }
  
  ##########################################################################################################
  
  hitzero <- function(i,j){    
    for(M in -1:1){
      for (N in -1:1){
        curJ=(j+M);
        curI=(i+N);
        if (M ==0 & N==0){next}
        if (curJ < 1 | curJ > ny | curI <1 | curI > nx){next}
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
  
  #########################################################################################################
  
  hitflag <- function (i,j){    
    imgsub=img[ max(j-1,1):min(j+1,ny) ,  max(i-1,1):min(i+1,nx)        ]
    rightsub=right[ max(j-1,1):min(j+1,ny) ,  max(i-1,1):min(i+1,nx)        ]
    if (all(imgsub==rightsub) & max(imgsub) == 1)
    {
      for(M in -1:1){
        for (N in -1:1){
          curJ=(j+M);
          curI=(i+N);
          if (M ==0 & N==0) {next}
          if (curJ < 1 | curJ > ny | curI <1 | curI > nx) {next}
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
                   onMouseUp= mouseup,
  )
}

  #########################################################################################################

Minesweeper <- function(n){
  if (n == 5) {
    mygame(10,20,0.01) # Luck
  } else if (n == 4){
    mygame(10,20,0.7) # Expert
  } else if (n == 3){
    mygame(10,20,0.8)  # hard
  } else if (n == 2){
    mygame(10,20,0.9) # Medium
  } else {
    mygame(10,20,0.95) # Easy
  }
}