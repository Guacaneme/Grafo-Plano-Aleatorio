vertices <- (1:200)     # Grupo de vértices

x <- function(n)(n*0)
suma_aristas <- x(1:200)    # Cantidad de aristas por vértice

aristas <- list()    # Lista de aristas por vértice
# Llenar la lista de aristas con los vértices en 0; Los vértices están organizados en una matriz de 10x20
for (fila in 1:10){
    for (colum in 1:20){
        if (fila == 1 || fila == 10){
            if (colum == 1 || colum == 20){
                aristas[[length(aristas)+1]] <- c(0,0)
            }else{
                aristas[[length(aristas)+1]] <- c(0,0,0)
            }
        }else{
            if (colum == 1 || colum == 20){
                aristas[[length(aristas)+1]] <- c(0,0,0)
            }else{
                aristas[[length(aristas)+1]] <- c(0,0,0,0)
            } 
        }
    }
}

n_aristas_aleatorio <- sample(1:10,1)   # 294 ya que para un grafo plano |E| ≤ 3|V|-6

lado_aleatorio <- function(ver)(
    if (ver == 1 || ver == 20 || ver == 181 || ver == 200){
        sample(1:2,1)
    }else{
        if ((ver > 1 && ver < 20) || (ver > 181 && ver < 200)){
            sample(1:3,1)
        }else{
            if ( ver%%20 == 0 || ver%%20 == 1){
                sample(1:3,1)
            }else{
                sample(1:4,1)
            }
        }
    }
)


#verificar_arista <- function(ver,lado)()

for (n in 1:n_aristas_aleatorio){
    while (TRUE){
        ver <- sample(1:200,1)  # Vértice aleatorio al cual se le va a poner una arista
        lado <- lado_aleatorio(ver)
        
        if (aristas[[ver]][lado] == 0){
            ady <- c()
            if (ver < 200){
                ady <- c(ady,ver+1)
            }
            z <- 1
            if (ver > 1){
                ady <- c(ady,ver-1)
            }
            z <- 1
            if (ver < 181){
                ady <- c(ady,ver+20)
            }
            z <- 1
            if (ver > 20){
                ady <- c(ady,ver-20)
            }

            ady_org <- c() 
            for (v in 1:length(ady)){
                ady_org <- c(ady_org,min(ady))
                ady <- ady[!(ady == min(ady))]
            }

            
            aristas[[ver]][lado] <- ady_org[lado]
            
            ady2 <- c()
            if (ady_org[lado] < 200){
                ady2 <- c(ady2,ady_org[lado]+1)
            }
            z <- 1
            if (ady_org[lado] > 1){
                ady2 <- c(ady2,ady_org[lado]-1)
            }
            z <- 1
            if (ady_org[lado] < 181){
                ady2 <- c(ady2,ady_org[lado]+20)
            }
            z <- 1
            if (ady_org[lado] > 20){
                ady2 <- c(ady2,ady_org[lado]-20)
            }

            ady_org2 <- c() 
            for (v in 1:length(ady2)){
                ady_org2 <- c(ady_org2,min(ady2))
                ady2 <- ady2[!(ady2 == min(ady2))]
            }
            
            for (v in 1:length(ady_org2)){
                if(ady_org2[v] == ver){
                    aristas[[ady_org[lado]]][v] <- ver
                }
            }
            
            
        }
            
        
        
        break
    }
}

aristas

#plot(0,0, col="white", xlim = c(-55,55), ylim = c(-55,55), main = "Grafo Aleatorio")
#points(30*rnorm(200),30*rnorm(200), col="red")
#segments(30*rnorm(200),30*rnorm(200),30*rnorm(200),30*rnorm(200))30*rnorm(200))
