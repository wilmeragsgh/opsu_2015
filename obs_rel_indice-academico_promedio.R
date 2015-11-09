## Lectura y visualizacion de los cupos asignados:
# ------------------------------------------------

# Libreria necesario para la lectura del documento:
library(tm)

# Lectura del documento
pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = "opsu ucv.pdf"),
                                                 language = "en",
                                                 id = "id1")

# Definicion de la longuitud minima que posee una observacion en el documento
# para poder filtrar solo los registros.
min <- 197

# Creacion de un data frame para almacenar todas las observaciones: 
opsu <- data.frame(Posicion= numeric(),Grado= numeric(),indice_academico= numeric(),Semestre= character(),Promedio= numeric(),Sede = character(),Programa = character(),stringsAsFactors = F)

# Lectura iterativa de todo el documento para obtener las variables:
# sede = Sede, prog =Programa, gra = Grado, ina = Indice Academico, sem = Semestre
# pro = Promedio.
for (reg in pdf$content){
  if (substr(reg,1,4) == 'SEDE'){
    sede <- substr(reg,15,nchar(reg))
  }
  if (substr(reg,1,8) == 'PROGRAMA'){
    prog <- substr(reg,19,nchar(reg))
  }
  if (is.na(as.numeric(substr(reg,1,4)))){}
  else{
    pos <- as.numeric(substr(reg,1,4))
    if (is.na(pos)){}
    else{
      if (nchar(reg) > min){
        odd_spc <- nchar(reg) - min
      }else{
          odd_spc <- 0
        }
      gra <- as.numeric(substr(reg,163 + odd_spc,166 + odd_spc))
      ina <- as.numeric(substr(reg,170 + odd_spc,175 + odd_spc))
      sem <- substr(reg,179 + odd_spc,186 + odd_spc)
      pro <- as.numeric(gsub(',','.',substr(reg,192 + odd_spc,198 + odd_spc)))
      opsu <- rbind(opsu,data.frame(Posicion = pos,Grado = gra,indice_academico = ina,Semestre = sem,Promedio = pro,Sede = sede,Programa = prog,stringsAsFactors = F))
      }
    }
}

# Una vez que creamos el data frame lo podemos escribir a un archivo csv:
write.csv(opsu,file = 'opsu.csv')

# O almacenarlo en un objeto .RData:
save(opsu,file ='opsu.RData')
# Si lo salvamos en un objeto, la proxima vez podemos solo cargarlo:
load('opsu.RData')

# Defnimos los cuantiles para denotar donde se encuentran la mayor cantidad de observaciones
q <- quantile(opsu$Promedio,c(0.25,0.5,0.75))
# almacenamos solo los indices de las observaciones que se encuentran entre 1 y 3 cuantil (que almacena el 50% de las observacions)
sub_ind <- which(q[1] < opsu$Promedio[opsu$Promedio < q[3]])
# Definimos un modelo lineal sobre el 1 y 3 cuantil para mostrar la tendencia de los valores que es encuentran en ese intervalo
line <- lm((opsu$indice_academico[sub_ind]- (50 * opsu$Promedio[sub_ind])/20) ~ opsu$Promedio[sub_ind], data = opsu[sub_ind,])
# Coeficientes del modelo:
coeff <- coefficients(line)
# Para filtrar por color las personas con mayor indice academico:
rbPal <- colorRampPalette(c('gray','black'))
Col <- rbPal(20)[as.numeric(cut((opsu$indice_academico- (50 * opsu$Promedio)/20),breaks = 10))]

# Visualizacion:
plot(opsu$Promedio,(opsu$indice_academico- (50 * opsu$Promedio)/20),col = Col, pch = 4, xlab = 'Promedio',ylab = 'Indice Academico(sin promedio)')
abline(v=quantile(opsu$Promedio,c(0.25,0.5,0.75)),col = 'red')
abline(line, col= 'blue')

# Por programa:
programa <- 'Derecho'
plot(opsu$Promedio[opsu$Programa ==programa],(opsu$indice_academico[opsu$Programa ==programa]- (50 * opsu$Promedio[opsu$Programa ==programa])/20),col = Col, pch = 4, xlab = 'Promedio',ylab = 'Indice Academico(sin promedio)')

# Iterativo:
for (programa in unique(opsu$Programa))
    {
        plot(opsu$Promedio[opsu$Programa ==programa],(opsu$indice_academico[opsu$Programa ==programa]- (50 * opsu$Promedio[opsu$Programa ==programa])/20),col = Col, pch = 4, xlab = 'Promedio',ylab = 'Indice Academico(sin promedio)', main = programa)
    
    }

# Ubicando cada programa en la visualizacion general
for (ele in unique(opsu$Programa)){
    plot(opsu$Promedio,(opsu$indice_academico- (50 * opsu$Promedio)/20),col = 'gray', pch = 4, main = ele,, xlab = 'Promedio',ylab = 'Indice Academico(sin promedio)')
    points(opsu$Promedio[opsu$Programa == ele],(opsu$indice_academico[opsu$Programa == ele]- (50 * opsu$Promedio[opsu$Programa == ele])/20),col = 'red', pch = 4, main = ele)
}

# La unica distribucion que se diferencia del resto es la de programa = Educación (EUS) , veamos:
ele <- unique(opsu$Programa)[50]
plot(opsu$Promedio,(opsu$indice_academico- (50 * opsu$Promedio)/20),col = 'gray', pch = 4, main = ele,, xlab = 'Promedio',ylab = 'Indice Academico(sin promedio)')
points(opsu$Promedio[opsu$Programa == ele],(opsu$indice_academico[opsu$Programa == ele]- (50 * opsu$Promedio[opsu$Programa == ele])/20),col = 'red', pch = 4, main = ele)


# Codigo disponible en Github:
# 