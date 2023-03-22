game <- function(nb_rows, nb_cols, prob){
  
  # Initialisation
  
  dev.new(10, 10)
  wsize = dev.size('px')
  dpi = wsize[1]/10
  dev.off()
  
  # Création d'une fenètre de jeu :
  
  dev.new(width = 650/dpi, height = 650/dpi/nb_cols * (8 + nb_rows))
  
  # Configuration de la zone de plot, titre et instruction : 
  
  par(mar = c(1, 1 ,1 ,1))
  plot(c(0, nb_cols), c(0 - 5, (nb_rows + 2)), type = 'n', ann = FALSE , axes = FALSE)
  str_instruct = 'Clic gauche pour creuser.\nClic droit pour mettre un drapeau'
  text(0, -4, str_instruct, font = 1, adj = c(0, 0), cex = 1.2)
  text(nb_cols/2, (nb_rows + 2), 'Démineur', font = 2, cex = 2.5)
  text(nb_cols/2 * 0.995, (nb_rows + 2) * 0.995, 'Démineur', font = 2, cex = 2.5)
  
  # Préparation des bombes :
  
  theta = seq(1, 100)/100 * 2 * 3.1415935
  curIrclex = cos(theta)
  curIrcley = sin(theta)
  rm(theta)
  
  # Création de la grille :
  
  plotij <- function(i, j, col){
    
    i = i - 1
    j = j - 1
    r = 0.1
    
    if (col == 'close'){ # Case fermée
      polygon(c(i + r, i + r, (i + 1) - r, (i + 1) - r), 
              c(nb_rows - j - r, nb_rows - (j + 1) + r, nb_rows - (j + 1) + r,nb_rows - j - r),
              col = '#BBBBBB', border = NA)    
      polygon(c(i, i, i + r, i + r, (i + 1) - r, (i + 1)), 
              c(nb_rows - j, nb_rows - (j + 1), nb_rows - (j + 1) + r, nb_rows - j - r, nb_rows - j - r, nb_rows - j),
              col = '#F8F8F8', border = NA) 
      polygon(c(i, (i + 1), (i + 1), (i + 1) - r, (i + 1) - r, i + r), 
              c(nb_rows - (j + 1), nb_rows - (j + 1), nb_rows - j, nb_rows - j - r, nb_rows - (j + 1) + r, nb_rows - (j + 1) + r),
              col = '#999999', border = NA)   
    } else if (col == 'open'){ # Case ouverte
      polygon(c(i, i, (i + 1), (i + 1)), 
              c(nb_rows - j, nb_rows - (j + 1), nb_rows - (j + 1), nb_rows - j),
              col = '#BBBBBB', border = '#777777')    
    } else if (col == 'bomb'){ # Case bombe
      polygon(c(i, i, (i + 1), (i + 1)), 
              c(nb_rows - j, nb_rows - (j + 1), nb_rows - (j + 1), nb_rows - j),
              col = '#BBBBBB', border = '#777777')    
      polygon(((i + 1) - 0.5) + curIrclex * 1/4, (nb_rows - (j + 1) + 0.5)  + curIrcley * 1/4, col = 'black')
      lines(c(((i + 1) - 0.5), ((i + 1) - 0.5) + 1/3), c((nb_rows - (j + 1) + 0.5), (nb_rows - (j + 1) + 0.5) + 1/3))
      lines(c(((i + 1) - 0.5) + 1/3, ((i + 1) - 0.5) + 1/3 + 1/12) , c((nb_rows - (j + 1) + 0.5) + 1/3, (nb_rows - (j + 1) + 0.5) + 1/3 - 1/12))
    } else if (col == 'curbomb'){ # Case bombe détonnée 
      polygon(c(i, i, (i + 1), (i + 1)), 
              c(nb_rows - j, nb_rows - (j + 1), nb_rows - (j + 1), nb_rows - j),
              col = '#FF0000', border = '#777777')   
      polygon(((i + 1) - 0.5) + curIrclex * 1/4, (nb_rows - (j + 1) + 0.5)  + curIrcley * 1/4, col = 'black')
      lines(c(((i + 1) - 0.5), ((i + 1) - 0.5) + 1/3), c((nb_rows - (j + 1) + 0.5), (nb_rows - (j + 1) + 0.5) + 1/3))
      lines(c(((i + 1) - 0.5) + 1/3, ((i + 1) - 0.5) + 1/3 + 1/12), c((nb_rows - (j + 1) + 0.5) + 1/3, (nb_rows - (j + 1) + 0.5) + 1/3 - 1/12))
    } else if (col == 'down'){ # Case transition
      polygon(c(i, i, (i + 1), (i + 1)), 
              c(nb_rows - j, nb_rows - (j + 1), nb_rows - (j + 1), nb_rows - j),
              col = '#BBBBBB', border = '#AAAAAA')    
    } else if (col == 'flag'){ # Case Drapeau 
      polygon(c(i + r, i + r, (i + 1) - r, (i + 1) - r), 
              c(nb_rows - j - r, nb_rows - (j + 1) + r, nb_rows - (j + 1) + r, nb_rows - j - r),
              col = '#BBBBBB', border = NA)    
      polygon(c(i, i, i + r, i + r, (i + 1) - r, (i + 1)), 
              c(nb_rows-j, nb_rows - (j + 1), nb_rows - (j + 1) + r, nb_rows - j - r, nb_rows - j - r, nb_rows - j),
              col = '#F8F8F8', border = NA)   
      polygon(c(i, (i + 1), (i + 1), (i + 1) - r, (i + 1) - r, i + r), 
              c(nb_rows - (j + 1), nb_rows - (j + 1), nb_rows - j, nb_rows - j - r, nb_rows - (j + 1) + r, nb_rows - (j + 1) + r),
              col = '#999999', border = NA)   
      
      xc = (i + 0.5) - 1/6
      yc = (nb_rows - (j + 1) + 0.5)
      a = 1/2
      b = 1/9
      c = 1/4
      
      polygon(c(xc, xc, xc + a, xc + a), 
              c(yc + b + c, yc + b, yc + b, yc + b + c),
              col = '#FF0000', border = NA)   
      
      d = 1/4
      lines(c(xc, xc), c(yc + b + c, yc - d))
      e = 1/3
      yc = yc - d
      f = 1/7
      xc = (i + 0.5) 
      
      polygon(c(xc - e, xc - e, xc + e, xc + e), 
              c(yc, yc - f, yc - f, yc),
              col = '#000000', border = NA)   
    }  
  }

  # Création Bouton :
  
  plotbutton <- function(xloc, yloc, w1, w2, r, str){
    
    polygon(c(xloc + r, xloc + r, xloc + w1 - r, xloc + w1 - r), 
            c(yloc - r, yloc - w2 + r, yloc - w2 + r, yloc - r),
            col = '#BBBBBB', border = NA)
    polygon(c(xloc, xloc, xloc + r, xloc + r, xloc + w1 - r, xloc + w1), 
            c(yloc, yloc - w2, yloc - w2 + r, yloc - r, yloc - r, yloc),
            col = '#F0F0F0', border = NA)  
    polygon(c(xloc, xloc + w1, xloc + w1, xloc + w1 - r, xloc + w1 - r, xloc + r), 
            c(yloc - w2, yloc - w2, yloc, yloc - r, yloc - w2 + r, yloc - w2 + r),
            col = '#999999', border = NA)  
    text(xloc + w1/2 - 0.01, yloc - w2/2 - 0.01, str, font = 1, cex = 1.2, col = '#000000')
    text(xloc + w1/2, yloc - w2/2, str, font = 1, cex = 1.2)
  }
  
  # Initialisation des variables :
  
  img = matrix(as.numeric(runif(nb_cols * nb_rows) > prob ), nrow = nb_rows)
  num = img * 0
  
  # Comptage des bombes :
  
  for (i in 1:nb_rows){
    for (j in 1:nb_cols){ 
      num[i, j] = sum(img[max((i - 1), 1):min((i + 1), nb_rows), max((j - 1), 1):min((j + 1), nb_cols)]) 
      }
  left = img * 0
  right = img * 0
  process0 = img * 0
  taskFailure = 0
  taskFinished = 0
  }
  
  for (i in 1:nb_cols){
    for (j in 1:nb_rows){
      plotij(i, j, 'close')  
    }
  }
  
  curI = 0
  curJ = 0
  curButton = 0
  clickRestart = FALSE
  clickQuit = FALSE
  bnum = paste('Bombes Restante :', formatC(sum(img) - sum(right), format = 'd', digits = 3))
  legend(nb_cols/2, -3.5, bnum, cex = 1.2, bg = "white")   
  
  # Configuration Bouton : 
   
  w1 = 5
  w2 = 2
  r = 0.3
  plotbutton(nb_cols * 3/4, -1, w1, w2, r, 'Quitter')
  plotbutton(nb_cols * 2/4, -1, w1, w2, r,'Recommencer')
  
  # Clic de souris :
  
  mousedown <- function(button, x, y){
    
    x = grconvertX(x, "ndc", "user")
    y = grconvertY(y, "ndc", "user")
    cat("Buttons", paste(button, collapse=" "), " at ", x, y, "\n")  
    
    if (!(x < (nb_cols) & x > 0 & y < (nb_rows) & y > 0)){
      x0 = nb_cols/2
      y0 = -1
      w1 = 5
      w2 = 2
      r = 0.3
      
      # Bouton recommencer :
      
      if (x > (x0 + r) & x < (x0 + w1 - r) & y < (y0 - r) & y > (y0 - w2 + r)){           
        clickRestart <<- TRUE
        xloc = x0
        yloc = y0            
        polygon(c(xloc + r, xloc + r, xloc + w1 - r, xloc + w1 - r), 
                c(yloc - r, yloc - w2 + r, yloc - w2 + r, yloc - r),
                col = '#AA9999', border = NA)
        text(xloc + w1/2 - 0.01, yloc - w2/2 - 0.01, 'Recommencer', font = 1, cex = .7)
      }
      x0 = nb_cols * 3/4
      y0 = -1
      w1 = 5
      w2 = 2
      r = 0.3
      
      # Bouton quitter :
      
      if (x > (x0 + r) & x < (x0 + w1 - r) & y < (y0 - r) & y > (y0 - w2 + r)){
        clickQuit <<- TRUE
        xloc = x0
        yloc = y0            
        polygon(c(xloc + r, xloc + r, xloc + w1 - r, xloc + w1 - r), 
                c(yloc - r, yloc - w2 + r, yloc - w2 + r, yloc - r),
                col = '#AA9999', border = NA)
        text(xloc + w1/2 - 0.01, yloc - w2/2 - 0.01, 'Quit', font = 1, cex = .7)
      }
      return(NULL)
    }
    
    # Case du démineur :
    
    if (taskFailure == 1 | taskFinished == 1){return(NULL)}
    
    curI <<- ceiling(x)
    curJ <<- ceiling((nb_rows - y))
    curButton <<- button
    
    if (left[curJ, curI] == 0 & button[1] != 1){ 
      plotij(curI, curJ, 'down')
    }
    return(NULL)
  }
  
  # Après Clic de souris :
  
  mouseup <- function(button, x, y){
    
    x = grconvertX(x, "ndc", "user")
    y = grconvertY(y, "ndc", "user")
    button = curButton
    cat("Buttons", paste(button, collapse = " "), " at ", x, y, "\n")
    
    # Bouton Recommencer
    
    if (clickRestart){
      clickRestart <<- FALSE
      
      # Initialisation des variables :
      
      img <<- matrix(as.numeric(runif(nb_cols * nb_rows) > prob), nrow = nb_rows)
      num <<- img * 0
      
      # Comptage des Bombes : 
      
      for (i in 1:nb_rows){
        for (j in 1:nb_cols){num[i, j] <<- sum(img[max((i - 1), 1):min((i + 1), nb_rows), max((j - 1), 1):min((j + 1), nb_cols)]) 
        }
      }
      left <<- img * 0
      right <<- img * 0
      process0 <<- img * 0
      taskFailure <<- 0
      taskFinished <<- 0
      
      for (i in 1:nb_cols){
        for (j in 1:nb_rows){
          plotij(i, j, 'close')  
        }
      }
      
      curI <<- 0
      curJ <<- 0
      curButton <<- 0
      bnum=paste('Bombes Restante :', formatC(sum(img) - sum(right), format = 'd', digits = 3)) 
      legend(nb_cols/2, -3.5, bnum, cex = 1.2, bg = "white") 
      
      # Configuration Bouton :
      
      w1 = 5
      w2 = 2
      r = 0.3
      plotbutton(nb_cols * 3/4, -1, w1, w2, r, 'Quitter')
      plotbutton(nb_cols * 2/4, -1, w1, w2, r, 'Recommencer')
      return(NULL)
    }
    if (clickQuit){ # Fermeture de fenêtre
      dev.off()
    }
    if (!(x < (nb_cols) & x > 0 & y < (nb_rows) & y > 0)){return(NULL)}
    if (taskFailure == 1 | taskFinished == 1){return(NULL)}
    
    i = ceiling(x)
    j = ceiling((nb_rows - y))
    
    if (i != curI | j != curJ){
      if (left[curJ, curI] == 0){ 
        plotij(curI, curJ, 'close')
        if (right[curJ, curI] == 0){ 
          plotij(curI, curJ, 'close')
          }
        else if (right[curJ, curI] == 1){
          plotij(curI, curJ, 'flag')
          }
      }
      return(NULL)
    }
    
    # Placer un Drapeau
    
    if (button[1] == 0){
      if (right[j, i] == 1){
        plotij(i, j, 'flag')
        return(NULL)
      }
      
      # Ouvrir une Case :
      
      if (left[j, i] == 0){
        if (img[j, i] == 0){
          plotij(i, j, 'open'); plottext(i, j)
          left[j, i] <<- 1
          if (num[j, i] == 0){
            process0[j, i] <<- 1
            hitzero(i, j)
            }
        }
        
        # Game Over :
        
        else{
          idx = which(img == 1)
          rowlist = row(img)[idx]
          collist = col(img)[idx]
          for (n in 1:length(idx)){ 
            cat('boom', rowlist[n], collist[n], '\n')
            plotij(collist[n], rowlist[n], 'bomb')
          }
          plotij(i, j, 'curbomb')
          taskFailure <<- 1
          text(nb_cols/2, nb_rows/2, 'Game over!', font = 2, cex = 3.5, col = 'red')
        }
      }
    }
    
    # Vérification de victoire :
    
    else if (button[1] == 2){
      if (left[j, i] == 0){
        if (right[j, i] == 0){
          plotij(i, j, 'flag')
          right[j, i] <<- 1
          if(all(img == right)){
            taskFinished <<- 1
            text(nb_cols/2, nb_rows/2, 'Victoire !', font = 2, cex = 3.5, col = 'green')
          }
          bnum=paste('Bombes Restante :', formatC(sum(img)-sum(right),format='d',digits=3))
          legend(nb_cols/2, -3.5,bnum,cex=1.2,bg="white")   
        }
        else{
          plotij(i, j, 'close')
          right[j, i] <<- 0
          bnum=paste('Bombes Restante :', formatC(sum(img) - sum(right), format = 'd', digits = 3))
          legend(nb_cols/2, -3.5, bnum, cex = 1.2, bg = "white")  
        }
      }
    }
    return(NULL)
  }

  # Afficher le chiffre des cases ouvertes :
  
  plottext <- function(i, j){
    if (num[j, i] > 0){
      idx = num[j, i]
      col = c('blue', 'green', 'red', 'purple', 'pink', 'yellow', 'orange', 'black')
      text((i - 0.5), (nb_rows - j + 0.5), toString(num[j, i]), col = col[idx], font = 2)
    }
  }

  # Ouverture de case adjacente sur 0 :
  
  hitzero <- function(i, j){    
    for(M in -1:1){
      for (N in -1:1){
        curJ = (j + M)
        curI = (i + N)
        if (M == 0 & N == 0){next}
        if (curJ < 1 | curJ > nb_rows | curI < 1 | curI > nb_cols){next}
        if (right[curJ, curI] == 0){
          plotij(curI, curJ, 'open'); plottext(curI, curJ)       
          left[curJ, curI] <<- 1       
        }
        if (num[curJ, curI] == 0 & process0[curJ,  curI] == 0){
          process0[curJ, curI] <<- 1
          hitzero(curI, curJ)}
      }
    }   
  }
  
  # Info Souris :
  
  getGraphicsEvent("Démineur",
                   onMouseDown = mousedown,
                   onMouseUp = mouseup)
}

#########################################################################################################

# Gestion de la difficulté : 

Minesweeper <- function(n){
  if (n == 5){
    game(10, 20, 0.01) # One Luck
  } else if (n == 4){
    game(10, 20, 0.7) # Expert
  } else if (n == 3){
    game(10, 20, 0.8)  # hard
  } else if (n == 2){
    game(10, 20, 0.9) # Medium
  } else {
    game(10, 20, 0.95) # Easy
  }
}
#Minesweeper(1) #Example 


library(shiny)

ui <- fluidPage(
  titlePanel("Démineur"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Difficulté :", value = 2, min = 1, max = 5, step = 1),
      actionButton("play", "Jouer !")
    ),
    mainPanel(
      uiOutput("gameboard")
    )
  )
)

server <- function(input, output, session) {
  output$gameboard <- renderUI({
    req(input$play)
    Minesweeper(input$n)
  })
}

shinyApp(ui, server)
