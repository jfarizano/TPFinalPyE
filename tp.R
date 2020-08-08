library(diagram)
library(markovchain)

# ----------------------------------------------------------------------------
# Ej 1

simularCienTiradas <- function(P) {
  Sim <- sample(c("Cara", "Cruz"), 100, T, c(P, 1 - P))
  length(which(Sim == "Cara"))
}

simularCienPorN <- function(N, P) {
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

tresCaras <- function(N, P) {
  Acumulador <- 0
  for (i in 1:N) {
    Veces <- 0
    Tiradas <- 0
    while(Veces < 3) {
      Sim <- sample(c("Cara", "Cruz"), 1, T, c(P, 1 - P))
      if (Sim[1] == "Cara") {
        Veces <- Veces + 1
      }
      Tiradas <- Tiradas + 1
    }
    Acumulador <- Acumulador + Tiradas
  }
  Acumulador / N
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 2

trayectoriaD <- function(N, P) {
  Sim <- sample(c(1, 0), N, T, c(P, 1 - P))
  D <- vector()
  for (i in 1:N) {
    D[i] <- 2 * Sim[i] - 1
  }
  D
}

trayectoriaS <- function(N, P) {
  D <- trayectoriaD(N, P)
  S <- c(0)
  S[1] = 2 * D[1] -1
  for (i in 2:N) {
    S[i] <- S[i - 1] + D[i]
  }
  plot(S, type = "p", xlab = "Instante", ylab = "Valor de S",  main = "Trayectoria de Sn",
       xlim = c(1, N), ylim = c(min(S), max(S)), xaxt = "n", yaxt = "n")
  axis(side = 1, seq(1, N, 1))
  axis(side = 2, at = seq(min(S), max(S), 1))
  S
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 3

calcularProbRuinaCadena <- function(P, K, S, N, Simulaciones) {
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
     Arruinado <- Arruinado + 1
   }
  }
  c(Arruinado, Millonario) / Simulaciones
}

calcularProbRuinaIter <- function(P, K, S, Simulaciones) {
  Arruinado <- 0
  Millonario <- 0
  for (i in 1:Simulaciones) {
    Resultado <- K
    Pasos <- 0
  
    while (Resultado != 0 & Resultado != S) {
      Resultado <- Resultado + sample(c(1, -1), 1, T, c(P, 1 - P))[1]
      Pasos <- Pasos + 1
    }
    if (Resultado == 0) {
      Arruinado <- Arruinado + 1
    }
  }
  Arruinado / Simulaciones
}

gamblersRuinTrayectoria <- function(P, K, S) {
  Pasos <- 0
  Resultado <- K
  Trayectoria <- c(K)
  
  while (Resultado != 0 & Resultado != S) {
    Resultado <- Resultado + sample(c(1, -1), 1, T, c(P, 1 - P))[1]
    Trayectoria <- c(Trayectoria, Resultado)
    Pasos <- Pasos + 1
  }
  
  plot(0:Pasos, Trayectoria, type = "p", xlab = "Instante", ylab ="Cantidad de dinero",
       main = "Trayectoria del jugador", ylim = c(0, S))
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 4
ratonLaboratorio <- function(N) {
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

# Para el b usar hittingProbabilities(MC)
# Para el c usar meanAbsorptionTime(MC)
# Para el d usar steadyStates(MC)

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 6

ej6 <- function() {
  P <<- matrix(c(0, 0, 0, 0, 1/2, 1/2, 0, 
                1/3, 0, 1/3, 0, 0, 1/3, 0,
                0, 0, 0, 1/2, 0, 1/2, 0,
                0, 0, 0, 0, 0, 1, 0,
                1/4, 0, 0, 1/4, 0, 1/4, 1/4,
                1/2, 1/2, 0, 0, 0, 0, 0,
                1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7), nrow = 7, ncol = 7, byrow = T,
              dimnames = list(c("a", "b", "c", "d", "e", "f", "g"), c("a", "b", "c", "d", "e", "f", "g")))
  MC <<- new("markovchain", states = rownames(P), transitionMatrix = P, name="PageRank")
  pi <- c(1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7)
  Sim <<- rmarkovchain(MC, n = 100, t0 = sample(rownames(P), 1, T, pi)[1])
  plot(table(Sim), type = "h", xlab = "Página", ylab = "Cantidad de visitas", font.lab = 2,
       ylim = c(0, 35), main = "Cantidad de visitas a las páginas en la simulación")
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
  
  X <<- X
  S <<- S
  i <<- i
  
  plot(c(0,S), (0:i), type = "s", xlim = c(0, t))
  hist(X, freq = F)
}

densidadSn <- function(n, t) {
  ((10 ^ n) * t ^ (n - 1) * exp(-10 * t)) / factorial(n-1)
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Ej 8

ej8 <- function() {
  init <<- c(0, 0, 0, 0, 1)
  P <<- matrix(c(0, 0, 0, 0, 1,
                0, 8/13, 3/13, 1/13, 1/13,
                1/16, 3/16, 3/8, 1/4, 1/8,
                0, 1/11, 4/11, 5/11, 1/11,
                0, 1/8, 1/2, 1/8, 1/4),
              nrow = 5, byrow = T,
              dimnames = list(c(90, 135, 139, 445, "No attack"), c(90, 135, 139, 445, "No attack")))
  MC <<- new("markovchain", states = rownames(P), transitionMatrix = P, name="Honeypot")
  init * (MC^2) # a)
  steadyStates(MC) # b)
}