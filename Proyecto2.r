vertices <- (1:200)     # Grupo de vértices

x <- function(n)(n*0)
suma_aristas <- x(1:200)    # Cantidad de aristas por vértice(

aristas <- list()    # Lista de aristas por vértice

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

for (n in 1:n_aristas_aleatorio){
    while (TRUE){
        ver <- sample(1:200,1)  # Vértice aleatorio al cual se le va a poner una arista
        lado <- 0
        if (ver == 1 || ver == 20 || ver == 181 || ver == 200){
            lado <- sample(1:2,1)
        }else{
            if ((ver > 1 && ver < 20) || (ver > 181 && ver < 200)){
                lado <- sample(1:3,1)
            }
        }
        break
    }
}


#plot(0,0, col="white", xlim = c(-55,55), ylim = c(-55,55), main = "Grafo Aleatorio")
#points(30*rnorm(200),30*rnorm(200), col="red")
#segments(30*rnorm(200),30*rnorm(200),30*rnorm(200),30*rnorm(200))30*rnorm(200))
