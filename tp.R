library(diagram)
library(markovchain)

# ----------------------------------------------------------------------------
# Ej 1
# Falta calcular el c teoricamente

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