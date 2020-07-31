library(diagram)
library(markovchain)

# ----------------------------------------------------------------------------
# Ej 1
# Para el 1c: 3/p

simularCienTiradas <- function(P) {
  Sim <- sample(c("Cara", "Cruz"), 100, T, c(P, 1 - P))
  length(which(Sim == "Cara"))
}

simularALot <- function(N, P) {
  Acumulador = 0
  Uno = 0
  for (i in 1:N) {
    Sim <- simularCienTiradas(P)
    Acumulador <- Acumulador + Sim
    if (Sim == 1) {
      Uno = Uno + 1
    }
  }
  c(Uno / N, Acumulador / N)
}

tresCaras <- function(P) {
  N = 0
  Tiradas = 0
  while(N < 3) {
    Sim <- sample(c("Cara", "Cruz"), 1, T, c(P, 1 - P))
    if (Sim[1] == "Cara") {
      N = N + 1
    }
    Tiradas = Tiradas + 1
  }
  Tiradas
}

buscarTres <- function(N, P) {
  Acumulador = 0
  for (i in 1:N) {
    Sim <- tresCaras(P)
    Acumulador <- Acumulador + Sim
  }
  Acumulador / N
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 2

ej2 <- function(N, P) {
  Sim <- sample(c(1, 0), N, T, c(P, 1 - P))
  Trayecto = c(0)
  Trayecto[1] = 2 * Sim[1] -1
  for (i in 2:N) {
    Trayecto[i] <- Trayecto[i - 1] + 2 * Sim[i] - 1
  }
  Trayecto
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 3

gamblersRuinChain <- function(P, K, S, N, Simulaciones) {
  States <- as.character(0:S)
  Transition <- matrix(0, nrow = S + 1, ncol = S + 1)
  for (i in 2:S) {
    Transition[i, i+1] <- P
    Transition[i+1, i] <- 1 - P
  }
  Transition[1, 1] <- 1
  Transition[S + 1, S + 1] <- 1
  Transition[2, 1] <- 1 - P
  Transition[S + 1, S] <- 0
  mc <<- new("markovchain", states = States, transitionMatrix = Transition, name="Gamblers ruin")
  
  Arruinado = 0
  Millonario = 0
  
  for (i in 1:Simulaciones) {
   Resultado = rmarkovchain(mc, n = N, t0 = K)[N]
   if (Resultado == 0) {
     Arruinado = Arruinado + 1
   }
   if (Resultado == S) {
     Millonario = Millonario + 1
   }
  }
  c(Arruinado, Millonario) / Simulaciones
}

gamblersRuinIter <- function(P, K, S, Simulaciones) {
  Arruinado = 0
  Millonario = 0
  for (i in 1:Simulaciones) {
    Resultado = K
    Pasos = 0
  
    while (Resultado != 0 & Resultado != S) {
      Resultado = Resultado + sample(c(1, -1), 1, T, c(P, 1 - P))[1]
      Pasos = Pasos + 1
    }
    
    if (Resultado == 0) {
      Arruinado = Arruinado + 1
    }
    if (Resultado == S) {
      Millonario = Millonario + 1
    }
  }
  c(Arruinado, Millonario) / Simulaciones
}

gamblersRuinTrayectoria <- function(P, K, S) {
  Pasos = 0
  Resultado = K
  Trayectoria = c(K)
  
  while (Resultado != 0 & Resultado != S) {
    Resultado = Resultado + sample(c(1, -1), 1, T, c(P, 1 - P))[1]
    Trayectoria = c(Trayectoria, Resultado)
    Pasos = Pasos + 1
  }
  
  plot(0:Pasos, Trayectoria, type = "o", xlab = "Instante", ylab ="Cantidad de dinero", main = "Trayectoria del jugador",
       ylim = c(0, S))
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 4

# Hacer apartado b

ej4 <- function(N) {
  Total = 0
  
  for (i in 1:N) {
    Tiempo = 0
    Resultado = "Seguir"
    
    while (Resultado == "Seguir") {
      Direccion = sample(c("Izquierda", "Derecha"), 1, T, c(0.5, 0.5))[1]
      if (Direccion == "Izquierda") {
        Resultado = sample(c("Seguir", "Salir"), 1, T, c(2/3, 1/3))[1]
        if (Resultado == "Seguir") {
          Tiempo = Tiempo + 5
        } else {
          Tiempo = Tiempo + 2
          Resultado = "Salir"
        }
      } else {
        Tiempo = Tiempo + 3
      }
      
    }
    Total = Total + Tiempo
  }
  
  Total / N
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 5

ej5 <- function() {
  P <- matrix(c(0.999, 0.001, 0, 0,
                0, 0.994, 0.006, 0,
                0, 0, 0.918, 0.082,
                0, 0, 0, 1),
              nrow = 4, byrow = T,
              dimnames = list(c("S", "VIH", "SIDA", "M"), c("S", "VIH", "SIDA", "M")))
  MC <<- new("markovchain", states = rownames(P), transitionMatrix = P, name="Epidemia")
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 6

ej6 <- function() {
  P <- matrix(c(0, 0, 0, 0, 1/2, 1/2, 0, 
                1/3, 0, 1/3, 0, 0, 1/3, 0,
                0, 0, 0, 1/2, 0, 1/2, 0,
                0, 0, 0, 0, 0, 1, 0,
                1/4, 0, 0, 1/4, 0, 1/4, 1/4,
                1/2, 1/2, 0, 0, 0, 0, 0,
                1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7), nrow = 7, ncol = 7, byrow = T,
              dimnames = list(c("a", "b", "c", "d", "e", "f", "g"), c("a", "b", "c", "d", "e", "f", "g")))
  MC <<- new("markovchain", states = rownames(P), transitionMatrix = P, name="PageRank")
  pi <- c(1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7)
  rmarkovchain(MC, n = 100, t0 = sample(rownames(P), 1, T, pi)[1])
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 7

ej7 <- function(t) {
  i <- 1
  X <- c(rexp(1, 10))
  S <- c(X[1])
  
  while (S[i] <= t) {
   i <- i + 1
   X[i] <- rexp(1, 10)
   S[i] <- X[i] + S[i - 1]
   
  }
  
  if (S[i] > t) {
    X <- X[1 : i - 1]
    S <- S[1 : i- 1]
    i <- i - 1
  }
  
  rbind(X, S)
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 8

ej8 <- function() {
  init <- c(0, 0, 0, 0, 1)
  P <- matrix(c(0, 0, 0, 0, 1,
                0, 8/13, 3/13, 1/13, 1/13,
                1/16, 3/16, 3/8, 1/4, 1/8,
                0, 1/11, 4/11, 5/11, 1/11,
                0, 1/8, 1/2, 1/8, 1/4),
              nrow = 5, byrow = T,
              dimnames = list(c(90, 135, 139, 445, "No attack"), c(90, 135, 139, 445, "No attack")))
  mc <<- new("markovchain", states = rownames(P), transitionMatrix = P, name="Honeypot")
  init * (mc^2) # a)
  steadyStates(mc) # b)
}