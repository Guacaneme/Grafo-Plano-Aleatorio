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

n_aristas_aleatorio <- sample(1:294,1)   # 294 ya que para un grafo plano |E| ≤ 3|V|-6

# Variables para las coordenadas de los vertices
v_x <- vertices%%20+1
v_y <- c()
for (i in 1:10){
    for (j in 1:20){
        v_y <- c(v_y,i)
    }
}
# Variables para las coordenadas de las aristas 
ar_x0 <- c()
ar_y0 <- c()
ar_x1 <- c()
ar_y1 <- c()

# Función que escoge un vértice aleatorio cercano al vértice esogido al azar
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
# Ciclo para escoger las n aristas aleatorias
for (n in 1:n_aristas_aleatorio){
    while (TRUE){
        ver <- sample(1:200,1)  # Vértice aleatorio al cual se le va a poner una arista
        lado <- lado_aleatorio(ver)
        
        if (aristas[[ver]][lado] == 0){
            ady <- c()      # Variable que contiene los vértices adyacentes
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
            # Se organizan los vértices adyacentes en orden ascendente
            ady_org <- c() 
            for (v in 1:length(ady)){
                ady_org <- c(ady_org,min(ady))
                ady <- ady[!(ady == min(ady))]
            }
            # Se inserta la arista en la lista
            aristas[[ver]][lado] <- ady_org[lado]
            # Coordenadas de la arista
            ar_x0 <- c(ar_x0,v_x[ver])
            ar_y0 <- c(ar_y0,v_y[ver])
            # Ya que es un grafo no dirigido, ambos vértices tienen la misma arista
            # Se busca el vértice esgido al azar en los vértices adyacentes del otro vértice que compone la arista
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
                    # Coordenadas de la arista
                    ar_x1 <- c(ar_x1,v_x[ady_org[lado]])
                    ar_y1 <- c(ar_y1,v_y[ady_org[lado]])
                }
            }
            break
        }
    }
}


print("En este grafo plano solo pueden haber subgrafos completos de hasta 3 vertices por la forma del grafo.")

subgrafos <- list()

for (s in 1:200){
    subgrafos[[length(subgrafos)+1]] <- c(s)
}

copia_ar <- aristas
for (v in 1:200){
    for (t in 1:length(copia_ar[[v]])){
        a <- copia_ar[[v]][t]
        if (a != 0){
            subgrafos[[length(subgrafos)+1]] <- c(v,a)
            for (u in 1:1:length(copia_ar[[a]])){
                if (copia_ar[[a]][u] == v){
                    copia_ar[[a]][u] <- 0
                }
            }
            copia_ar[[v]][t] <- 0
        }
    }
}
subgrafos

plot(0,0, col="white", xlim = c(0,21), ylim = c(0,11), main = "Grafo Plano Aleatorio")
points(v_x,v_y, col="red")
segments(ar_x0,ar_y0,ar_x1,ar_y1)
