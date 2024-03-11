

\appendix

# Estimación de parámetros para modelos en encuestas complejas

En la introducción de su excelente libro, *Statistical Design for Researches*, Leslie  Kish afirma que el enunciado de la mayoría de libros de inferencia estadística abren con el siguiente enunciado: *Dadas $n$ variables aleatorias, seleccionadas de una población, independientes e idénticamente distribuidas* y que cada palabra en el anterior enunciado es  engañosa. ¿Quién le da a uno las muestras? ¿Existe algún sitio en dónde las repartan? Las muestras no son dadas, las muestras deben ser seleccionadas, asignadas o capturadas. El tamaño de la muestra no siempre es un número $n$ fijo, en la mayoría de casos prácticos es una variable aleatoria. Los datos no siguen el supuesto de independencia ni de idéntica distribución; es más,  en muchas ocasiones no existe una sola población, sino que la muestra seleccionada es el resultado de una selección de sub-poblaciones para las cuales se deben producir, no solo una  sino muchas estimaciones.

En la teoría de muestreo, se considera que las características de interés son parámetros y no constituyen realizaciones de variables aleatorias. Para reforzar esta idea haga lo siguiente: examine una moneda y obsérvela. Suponga que usted está observando la cara (o sello, da igual) de la moneda. Esa cara (o sello) no constituye una realización de una variable aleatoria. Para que se pueda hablar de una variable aleatoria, es necesario realizar un experimento, el cual induce el conjunto de todos los posibles resultados, el cual a su vez induce una sigma-álgebra que define a la variable aleatoria. Sería muy diferente si se crease un experimento con esa moneda. El más sencillo de todos sería lanzarla al aire y observar si la moneda cayó en cara o sello. De forma similar, es muy válido afirmar que, por ejemplo, el estado de la naturaleza de un individuo que está desempleado no constituye una realización de una variable aleatoria.

Un ejemplo práctico se presenta a la hora de estimar la tasa de desempleo, se considera que, si un individuo está desempleado, pues está desempleado y punto. En otras palabras, el estado de la naturaleza del individuo al momento de la medición es "desempleado" y esta caracterización no corresponde a ninguna realización de algún evento aleatorio. Es por esto que, una vertiente de la inferencia en poblaciones finitas considera que el parámetro de interés será el número total de personas desempleadas dividido por el número total de personas en la fuerza laboral. Si se tuviese la oportunidad de medir a todos los integrantes de la fuerza laboral, mediante la realización de un censo, pues esa división correspondería al parámetro poblacional con el cual se tomarían decisiones y/o se cambiarían o reforzarían las políticas públicas de un país.

El propósito de este capítulo es llevar a los lectores al correcto análisis de sus datos, preguntándose acerca del proceso de selección de la muestra. Más aún, en términos de muestreo, solo hay un único caso para el cual la teoría de la inferencia estadística es aplicable y se trata del muestreo aleatorio simple con reemplazo en donde si se tienen las propiedades de independencia y de idéntica distribución.  Note que, en términos de selección de muestras, solo hay dos posibles escenarios generales. La selección con reemplazo y la selección sin reemplazo. 

## Acerca de las muestras aleatorias y su análisis

Hablemos primero de la selección sin reemplazo, en donde una muestra seleccionada está conformada por algunos elementos de la población que no se repiten. Para seleccionar una muestra sin reemplazo de tamaño $n=3$, de una población de tamaño $N=5$, el proceso de selección puede ser de la siguiente manera. Se escoge una unidad de las cinco posibles, luego se selecciona una unidad de las cuatro restantes, y por último, una unidad de las tres restantes. Esto hace que el proceso de selección de la muestra no se lleve a cabo de forma independiente. Por ejemplo, si el muestreo es aleatorio simple, la probabilidad de selección de la primera unidad es 1/5, la probabilidad de selección de la segunda unidad es 1/4 y así sucesivamente. Por otro lado, cuando el muestreo es con reemplazo, la selección se realiza de forma independiente puesto que se trata de realizar el mismo ensayo (seleccionar una unidad de cinco posibles) tres veces, sin importar que las unidades tengan diferentes probabilidades de selección.

Por otra parte, es bien sabido que la teoría de muestreo establece que el valor de la característica de interés, $y_k$, es eso, un valor; por tanto, no es aleatorio. Luego, es incorrecto decir que $y_k$ es una variable aleatoria asociada con alguna distribución de probabilidad. Recuerde que en el muestreo lo único aleatorio en la inferencia es la muestra. Ahora, no significa que no podamos construir variables aleatorias en muestreo. Por ejemplo, construyamos la siguiente variable aleatoria $X_i$ ($i=1,2,3$) definida como el valor de la característica de interés en el individuo $k$-ésimo, seleccionado en la $i$-ésima extracción. En este caso, existen tres variables aleatorias, puesto que la muestra es de tamaño tres. 

Si consideramos un muestreo aleatorio sin reemplazo, la primera variable aleatoria $X_1$, podrá tomar cualquiera de los siguiente cinco valores: $y_1, y_2, y_3, y_4, y_5$. La segunda variable aleatoria $X_2$, solo podrá tomar cuatro valores, puesto que $X_1$ ya fue realizada, y la tercera variable aleatoria $X_3$ solo podrá tomar tres valores, puesto que $X_1$ y $X_2$ ya fueron realizadas. Esto hace que $X_1$, $X_2$ y $X_3$ no constituya una sucesión de variables aleatorias independientes (puesto que la selección sin reemplazo no es un proceso independiente) ni idénticamente distribuidas (puesto que ni siquiera su espacio muestral es el mismo: $X_1$ puede tomar cinco valores, $X_2$ solo cuatro y $X_3$ solo tres). Lo cual quiere decir que a partir de un muestreo sin reemplazo (ni siquiera el tan mencionado muestreo aleatorio simple) no es posible construir una muestra aleatoria, como las que aparecen en los libros de teoría estadística. 

Sin embargo, algo muy distinto sucede con el muestreo con reemplazo. Cuando construimos las variables aleatorias $X_1$, $X_2$ y $X_3$, resulta ser que ellas sí conforman una sucesión de variables aleatorias independientes (puesto que el muestreo con reemplazo sí define un proceso de extracciones independientes) e idénticamente distribuidas (puesto que conservan el mismo espacio muestral y mantienen la probabilidad de selección). Es decir, $X_1$ puede tomar los valores $y_1, \ldots, y_5$. La probabilidad de que $X_1=y_1$ es $p_1$, la probabilidad de selección del primer elemento; la probabilidad de que $X_1=y_2$ es $p_2$, la probabilidad de selección del segundo elemento y así sucesivamente hasta obtener que la probabilidad de que $X_1=y_5$ es $p_5$, la probabilidad de selección del primer elemento primer elemento. La misma distribución la tienen $X_2$ y $X_3$. Por lo tanto, $X_1$, $X_2$ y $X_3$ conforman una muestra aleatoria, como las que aparecen en los libros clásicos de inferencia estadística.

Entonces, hemos llegado a un punto sin retorno, en donde la conclusión es que, si la muestra fue seleccionada con reemplazo, entonces podemos inducir una muestra aleatoria. Sin embargo, existen muchas variantes en el muestreo con reemplazo. A continuación, vamos a dilucidar cuál de ellas es la indicada para analizar la muestra de acuerdo con la teoría de los libros de inferencia. 

En primera instancia, veamos que para que la esperanza (bajo el diseño de muestreo $p$) de cualquier variable aleatoria $X_i$ sea igual a la media poblacional, es necesario que, para todos los individuos en la población, la probabilidad de selección sea idéntica e igual a $1/N$, como se muestra a continuación:

$$
E_p(X_i)=\sum_{k \in U} y_k Pr(X_i = Y_k) = \sum_{k \in U} y_k p_k 
= \frac{t_y}{N} = \bar{y}_U=\mu_N
$$

De la misma manera, para que la varianza de cualquier variable aleatoria $X_i$ sea igual a la varianza poblacional, se requiere la misma condición, puesto que:

$$
Var_p(X_i)
= \sum_{k \in U} (y_k - \bar{y}_U)^2 p_k 
= \frac{1}{N}\sum_{k \in U} (y_k - \bar{y}_U)^2  = S^2_{y_U} = \sigma^2_N
$$


Por lo tanto, la esperanza y la varianza de un estimador clásico como $\bar{X}$ solo coincidierón con los bien conocidos resultados de la inferencia clásica cuando el muestreo haya sido aleatorio simple con reemplazo. De otra forma, no se tienen las, bien conocidas, propiedades de esta estadística que implican que su esperanza es $E(\bar{X}) = \mu_N$ y su varianza es $Var(\bar{X}) = \frac{\sigma^2_N}{n}$. 

Este razonamiento de aplicarse de la misma forma para pruebas de hipótesis, construcción de intervalos de confianza, modelos de regresión, y hasta diseño de experimentos. Ahora, para una encuesta cuyos datos no fueron extraídos de manera aleatoria simple con reemplazo, la manera correcta de analizarla confiadamente es incluir los pesos de muestreo en todas las técnicas y metodologías estadísticas, ya sean regresiones simples y logísticas o simples varianzas del promedio.

## Modelos de superpoblación

Suponga que la estimación de máxima verosimilitud es apropiada para muestras aleatorias simples. Por ejemplo, modelos de regresión simple, múltiple, regresión logística, entre otros. Bajo este esquema, se asume que la función de densidad poblacional es $f(y | \theta)$ donde $\theta$ es el parámetro de interés.  Con una réplica del ejemplo que David Binder utiliza en un artículo del año 2011 (una excelente lectura para quienes ha seguido el trabajo de Ken Brewer), se introducen algunos conceptos que son de utilidad. Finalmente, todos los resultados se van a plasmar en simulaciones de Monte Carlo, algunas veces anidadas.

Suponga que se generaron $N=100$ realizaciones de variables aleatorias independientes distribuidas Bernoulli con parámetro de interés $\theta=0.3$. Los datos que se obtienen se muestran a continuación:



1 1 0 1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 1 0 
0 1 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 1 0 1 0 0 0 1 1 0 0 0 1 1 0 
0 1 0 0 0 1 0 0 1 1 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0


En esta población finita, que fue generada a partir de un modelo probabilístico (llamado modelo de superpoblación), hay 28 éxitos. 

### Primer proceso inferencial: el modelo

En este apartado, es notable que la medida de probabilidad que rige la inferencia hasta el momento sea la inducida por la distribución binomial con parámetro 0.3. De esta manera, el estimador insesgado de mínima varianza (todas estas propiedades obtenidas con base en la distribución binomial) está dado por el promedio poblacional. Nótese que la inferencia utiliza todos los datos de la población. Ahora, para reproducirlo computacionalmente, basta con simular muchas poblaciones de 100 variables aleatorias independientes distribuidas Bernoulli con parámetro desconocido $\theta$=0.3. 

Como es bien sabido, bajo la perspectiva de los modelos poblacionales y la inferencia estadística clásica, el estimador $\bar{y}_U = \frac{\sum_U y_k}{N}$ es insesgado. Para corroborarlo, es posible introducir la siguiente simulación de Monte Carlo.


```r
N = 100
theta = 0.3
nsim1 = 1000
Est0=rep(NA,nsim1)

for(i in 1:nsim1){
y=rbinom(N, 1, theta)
Est0[i]=mean(y)
}

Esp0 = mean(Est0)

cbind(theta, Esp0)  
```



<table>
 <thead>
  <tr>
   <th style="text-align:right;"> theta </th>
   <th style="text-align:right;"> Esp0 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.29742 </td>
  </tr>
</tbody>
</table>

### Segundo proceso inferencial: el muestreo

En el primer proceso inferencial, se asume que las variables de estudio son realizaciones de variables aleatorias gobernadas por un modelo probabilístico. Sin embargo, un razonamiento muy válido es que en cualquier población finita en particular, los valores de la medición son fijos aunque desconocidos y no siguen ningún modelo probabilístico;
es decir, no corresponden a realizaciones de variables aleatorias. Por ejemplo, suponga que para esa misma población del ejemplo anterior el dato uno corresponde a un individuo desempleado y el dato cero corresponde a un individuo empleado.

Por otra parte, asuma que la población está subdividida en conglomerados, que pueden ser llamados hogares. De esta forma, nuestra población finita toma la siguiente caracterización, mediante una partición de $N_{I}=27$ hogares:

(1 1 0) (1 0) (0 0 0 0 0 0 1) (1 0) (0 0 0 0 0 0 1) (0 0 
1) (0 0 0 0 0 0 0 1) (0 0 1) (0 0 0 1) (0 0 0 0 1) (0 0 
0 0 0 0 0 1) (1 0) (1 0) (0 0 1) (1 0) (0 0 1) (1 0) (0 1) 
(0 0 0 1) (0 0 1) (1 1 0) (0 0 0 0 1) (0 1) (0 1) (0 0 0 0 
0 0 0 0 0 1) (0 1) (0)

El proceso de aglomeración en hogares es obviamente artificioso en este ejemplo, pero ilustra que en la vida real las poblaciones finitas siempre están aglomeradas. Suponga por otra parte que tomamos una muestra $S_{I}$ de $n_{I}$ hogares y en cada hogar seleccionado realizamos un censo; además la selección de los hogares se hará aleatoriamente, sin reemplazo y con probabilidades de inclusión $\pi_{Ii}$ proporcionales
al tamaño del hogar $N_{i}$. Siendo la característica de interés $y_{k}$, el estado del individuo en la fuerza laboral (1, si está desempleado y 0, en otro caso); entonces es bien sabido que bajo este esquema de muestreo un estimador insesgado para la proporción de desempleados $\bar{y}_{U}$ es el siguiente:

$$
\bar{y}_{\pi S}=\sum_{i\in S_{I}}\frac{t_{y_{i}}}{\pi_{Ii}}=\frac{\sum_{i\in S_{I}}\bar{y}_{i}}{n_{I}}
$$
En donde $\bar{y}_{i}=\frac{t_{y_{i}}}{N_{i}}$ es la proporción de desempleados en el hogar $i$-ésimo, $t_{y_{i}}$ es el total de desempleados en el hogar $i$-ésimo, $N_{i}$ es el número de individuos en el hogar y $n_{I}$ es el número de hogares seleccionados. Por otro lado, un estimador ingenuo, correspondiente a la proporción de desempleados
en la muestra, que asume que el agrupamiento de los valores no interfiere
en el proceso de inferencia e ignora el diseño de muestreo es el siguiente:

$$
\bar{y}_{S}=\frac{\sum_{i\in S_{I}}t_{y_{i}}}{\sum_{i\in S_{I}}N_{i}}
$$

En términos generales el siguiente esquema trata de reproducir gráficamente este proceso de inferencia, en donde un gran número de muestras podrían haber sido extraídas siguiendo el diseño de muestreo. Con la siguiente simulación de Monte Carlo se comprueba fácilmente que es insesgado, mientras que es sesgado: 


```r
library(TeachingSampling)
N=100
theta=0.3
y=rbinom(N, 1, theta)
theta_N=mean(y)
nsim2=1000
Est1=Est2=rep(NA,nsim2)

#-----Creación de los clusters---------

clus=c(0,which((y[-N]-y[-1])!=0)+1)
NI=(length(clus)-1)
Ind=matrix(0, nrow=N, ncol=NI)
Tamaños=clus[-1]-clus[-(length(clus))]

for(l in 1:(length(clus)-1)){
a=(clus[l]+1):clus[l+1]
Ind[a,l]=a
}

#Tamaños

nsim2=1000
nI=floor(NI*0.3)

for(j in 1:nsim2){
res <- S.piPS(nI,Tamaños)
sam <- res[,1] 
Ind.sam=Ind[,sam]
Tamaños.sam=Tamaños[sam]
#-------Espacio para las medias
medias=matrix(NA)
for(k in 1:ncol(Ind.sam)){
medias[k]=mean(y[Ind.sam[,k]])
}
#-------
Est1[j]=mean(medias)
Est2[j]=sum(Tamaños.sam*medias)/sum(Tamaños)
}

Esp1=mean(Est1)
Esp2=mean(Est2)

cbind(theta_N, Esp1, Esp2)
```



<table>
 <thead>
  <tr>
   <th style="text-align:right;"> theta_N </th>
   <th style="text-align:right;"> Esp1 </th>
   <th style="text-align:right;"> Esp2 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.2928711 </td>
   <td style="text-align:right;"> 0.11186 </td>
  </tr>
</tbody>
</table>

Nótese que el primer estimador es insesgado (su esperanza equivale al parámetro de la población finita) porque es función del inverso de la probabilidad de inclusión de los elementos que son inducidas por la medida de probabilidad definida por el plan de muestreo. El segundo estimador es sesgado porque no tiene en cuenta el diseño de muestreo.

### Inferencia doble: los modelos y el muestreo

En último lugar, suponga que los valores de las variables de interés sí constituyen realizaciones de variables aleatorias que siguen un modelo probabilístico. Como una población finita está constituida por la realización particular de las variables aleatorias, condicionado a la realización de una población finita, se extrae una muestra aleatoria de elementos, mediante un diseño de muestreo complejo. Nótese que, en este tercer proceso inferencial, tanto el modelo como el diseño de muestreo como la medida de probabilidad que da origen a las superpoblaciones, constituyen dos medidas de probabilidad distintas que deben regir la inferencia del parámetro de interés.

Al respecto, nótese que, dado que el diseño de muestreo es complejo, no es viable utilizar técnicas clásicas, como el método de máxima verosimilitud, puesto que los datos finales no constituyen una muestra aleatoria de variables independientes ni idénticamente distribuidas. Por lo anterior, la forma final de la función de verosimilitud, definida como la densidad conjunta de las variables en la muestra, será muy compleja, intratable e insoluble. Una solución a este problema de estimación es la técnica de máxima pseudo-verosimilitud, la cual induce estimadores que tienen en cuenta las ponderaciones del diseño de muestreo complejo. Para el ejemplo de las proporciones, el estimador $\bar{y}_{\pi S}$ cumple la siguiente relación:

$$
E_{\xi p}(\bar{y}_{\pi S})=E_{\xi}E_{p}(\bar{y}_{\pi S}|Y)=E_{\xi}(\bar{y}_{U})=\theta=0.3
$$
Con la siguiente simulación de Monte Carlo se comprueba fácilmente que $\bar{y}_{\pi S}$ es insesgado, mientras que es $\bar{y}_{S}$ sesgado:



```r
library(TeachingSampling)

N=100
theta=0.3
nsim1=100
Esp1=Esp2=rep(NA,nsim1)

for(i in 1:nsim1){
y=rbinom(N, 1, theta)
#-----Creación de los clusters---------
clus=c(0,which((y[-N]-y[-1])!=0)+1)
NI=(length(clus)-1)
Ind=matrix(0, nrow=N, ncol=NI)
Tamaños=clus[-1]-clus[-(length(clus))]

for(l in 1:(length(clus)-1)){
a=(clus[l]+1):clus[l+1]
Ind[a,l]=a
}

Ind
Tamaños

nsim2=100
nI=floor(NI*0.3)
Est1=Est2=rep(NA,nsim2)

for(j in 1:nsim2){
res <- S.piPS(nI,Tamaños)
sam <- res[,1] 
sam
Ind.sam=Ind[,sam]
Tamaños.sam=Tamaños[sam]
#-------Espacio para las medias
medias=matrix(0)
for(k in 1:ncol(Ind.sam)){
medias[k]=mean(y[Ind.sam[,k]])
}

Est1[j]=mean(medias)
Est2[j]=sum(Tamaños.sam*medias)/sum(Tamaños)
}

Esp1[i]=mean(Est1)
Esp2[i]=mean(Est2)

}

cbind(theta, mean(Esp1), mean(Esp2))
```



<table>
 <thead>
  <tr>
   <th style="text-align:right;"> theta </th>
   <th style="text-align:right;">  </th>
   <th style="text-align:right;">  </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.3120952 </td>
   <td style="text-align:right;"> 0.1156181 </td>
  </tr>
</tbody>
</table>

Por supuesto que, dado que el proceso de inferencia es doble, entonces este ejercicio de Monte Carlo debe ser anidado. Es decir, muchas simulaciones dentro de una simulación. Nótese que en primer lugar se debe generar todas las poblaciones finitas y para cada una de ellas se debe generar las posibles muestras.

Los métodos que se explicarán en este capítulo serán la estimación por Máxima Verosimilitud (MV) y Máxima Pseudo Verosimilitud (MPV) para modelos de regresión. El primer método se basa en estimar un parámetro desconocido suponiendo que las variables de interés constituyen una muestra aleatoria de variables independiente e idénticamente distribuidas (IID) para poder hacer inferencia sobre la población de interés. Por otra parte el método de Máxima Pseudo Verosimilitud sigue un razonamiento parecido, pero con la gran diferencia de que la variable de interés se rige por un diseño muestral específico, lo cual induce una probabilidad de inclusión del individuo que debe ser tenida en cuenta al momento de realizar cualquier tipo de inferencia.


## Método de Máxima Verosimilitud

Uno de los métodos más utilizados en la estadística para estimar parámetros es el método de Máxima Verosimilitud, para utilizar este método debemos conocer la función de distribución de las variables de interés. Luego, si $y_{1},y_{2},\ldots,y_{N}$ una muestra aleatoria de las variable de interés que siguen una distribución $f(y;\theta)$. Por lo tanto, la función de verosimilitud está dada por:

$$
L(\theta)=\prod_{i=1}^{n}f(y_{i},\theta)
$$

Para un mejor manejo de esta función se sugiere aplicar propiedades de los logaritmos generando la siguiente función

$$
l(\theta)=\sum_{i=1}^{n}\ln[f(y_{i},\theta)]
$$
Calculando las derivadas con respecto a $\theta$ e igualando a cero tenemos el siguiente sistema de ecuaciones

$$
\sum_{i=1}^{N}\frac{\partial}{\partial\theta}\ln[f(y_{i},\theta)]=0
$$

Ahora, definiendo a $u_{i}=\frac{\partial}{\partial\theta}\ln[f(y_{i},\theta)]$, entonces el sistema de ecuaciones tendría la siguiente forma: 

$$
\sum_{i=1}^{N}u_{i}(\theta)=0
$$
$u_{i}$ es conocido el puntaje o \emph{score} de la unidad $i$-ésima. La solución de este sistema de ecuaciones, notada como $\hat{\theta}_{MV}$, es conocida como el Estimador de Máxima Verosimilitud. Una bondad de este método es que podemos obtener una varianza asintótica del modelo $\xi$, de la siguiente manera

$$
V_{\xi}(\hat{\theta}_{MV})\cong[J(\theta)]^{-1}
$$

donde,

$$
J(\theta)=\sum_{i=1}^{N}\partial u_{i}(\theta)/\partial\theta
$$

Como el anterior término depende del parámetro, un estimador consistente estaría dado por:

$$
\hat{V}_{\xi}(\hat{\theta}_{MV})=[J(\hat{\theta}_{MV})]^{-1}
$$

donde,

$$
J(\hat{\theta}_{MV})=J(\theta)\mid_{\theta=\hat{\theta}_{MV}}
$$

### MV para una distribución Bernoulli

En el ejemplo introductorio que sirvió como punto de partida para esta discusión, se habló de que los datos de naturaleza $\{0,1\}$ pueden ser modelados mediante una distribución Bernoulli, con parámetro de éxito $\theta$. De esta forma, la función de verosimilitud está dada por:

$$
L(\theta)  =\prod_{i=1}^{N}\theta^{y_{i}}(1-\theta)^{1-y_{i}}
$$

Luego, aplicando logaritmo, se tiene que:

$$
l(\theta)  =\sum_{i=1}^{N}\left[{y_{i}}\ln(\theta)+(1-y_{i})\ln(1-\theta)\right]
$$

Por lo tanto, las ecuaciones de verosimilitud, definidas en función de las variables de puntaje (\emph{scores}) son:

$$
\sum_{i=1}^{N}\frac{\partial}{\partial\theta}\left[{y_{i}}\ln(\theta)+(1-y_{i})\ln(1-\theta)\right]=\sum_{i=1}^{N}u_{i}(\theta)
$$

En donde, $u_{i}(\theta)=\frac{y_{i}-\theta}{\theta(1-\theta)}$. Por tanto, igualando a cero, se obtiene que

$$
\frac{\partial}{\partial\theta}\ln(\theta)\bar{y}_{U}+\frac{\partial}{\partial\theta}\ln(1-\theta)(n-\bar{y}_{U})=0
$$

De lo cual se obtiene el estimador de máxima verosimilitud dado por:

$$
\hat{\theta}_{MV}=\bar{y}_{U}=P_{d}
$$

Con varianza estimada dada por:

$$
\hat{V}_{\xi}(\hat{\theta}_{MV})=[J(\hat{\theta}_{MV})]^{-1}
$$

En donde,

$$
J(\hat{\theta}_{MV})=\sum_{i=1}^{N}\frac{\partial}{\partial\theta}u_{i}(\theta)=\frac{N}{\bar{y}_{U}(1-\bar{y}_{U})}=\frac{N}{P_{d}(1-P_{d})}
$$

Es decir que la estimación de la varianza para $\hat{\theta}_{MV}=P_{d}$
es $\hat{Var}_{\xi}(\hat{\theta}_{MV})=P_{d}Q_{d}/N$. En donde, $Q_{d}=1-P_{d}$.

### MV para una distribución normal

Ahora se ilustrará el método de Máxima Verosimilitud suponiendo la siguiente función de distribución de un variable aleatoria con distribución normal

$$
f(y;\theta)=\dfrac{1}{\sqrt{2\pi\sigma^{2}}}\exp\left[-\dfrac{1}{2}\left(\dfrac{y_{i}-\theta^{2}}{\sigma^{2}}\right)\right]
$$

Conociendo la función de distribución llegamos a la probabilidad conjunta

$$
L(\theta)=\prod_{i=1}^{N}\dfrac{1}{\sqrt{2\pi\sigma^{2}}}\exp\left[-\dfrac{1}{2}\left(\dfrac{y_{i}-\theta^{2}}{\sigma^{2}}\right)\right]
$$

Con un poco de álgebra llegamos a esta expresión

$$
L(\theta)=(2\pi\sigma^{2})^{-N/2}\exp[(-\dfrac{1}{2\sigma^{2}}\sum_{i=1}^{N}(y_{i}-\theta^{2})]
$$

Aplicando logaritmos

$$
l(\theta)=ln(2\pi\sigma^{2})^{-N/2}[-\dfrac{1}{2\sigma^{2}}\sum_{i=1}^{N}(y_{i}-\theta^{2})]
$$

Maximizando la anterior expresión llegamos a obtener el score $u_{i}$

$$
u_{i}=\partial l(\theta)/\partial\theta=\dfrac{1}{\sigma^{2}}\sum_{i=1}^{N}(y_{i}-\theta^{2})=0
$$

igualando a cero despejamos $\theta$ y tenemos

$$
\theta=\dfrac{\sum_{i=1}^{N}y_{i}}{N}=\bar{Y}
$$
Llegamos a que una estimación por el método de Máxima Verosimilitud, para la función $\theta$ que sigue una función de distribución normal, es el promedio poblacional $\bar{Y}$.

### MV para una regresión lineal múltiple

En un entorno matricial se puede tener en cuenta más de una variable predictora llevándonos a un modelo de regresión múltiple donde no solamente las variables $y_{i}$ son continuas, sino que también pueden ser categóricas. A continuación, se presenta la estimación de parámetros del modelo.

El modelo adopta la forma $X'\beta$, disponemos de $X$ como una matriz de dimensión $N\times i$, donde $n$ es el tamaño de muestra e $i$ es el número de variables predictoras, también se define un vector $Y$ de tamaño $n$ como la variable de interés y, por último, un vector $\beta$ de tamaño $i$. Suponiendo que $X$ sigue una distribución
normal tenemos la siguiente función:

$$
f(Y;X\beta)=\dfrac{1}{\sqrt{2\pi\sigma^{2}}}\exp\left[-\dfrac{1}{2\sigma^{2}}(Y-X\beta)'(Y-X\beta)\right]
$$

Conociendo la anterior función de distribución, llegaremos a la probabilidad conjunta de $f$

$$
L(Y;X\beta)=\prod_{i=1}^{n}\dfrac{1}{\sqrt{2\pi\sigma^{2}}}\exp\left[-\dfrac{1}{2\sigma^{2}}(Y-X\beta)'(Y-X\beta)\right]
$$

Con un poco de algebra matricial se llega a:

$$
L(Y;X\beta)=(2\pi\sigma^{2})^{-n/2}-\exp\left[\dfrac{1}{2\sigma^{2}}(Y'Y-Y'X\beta-(X\beta)'Y+(X\beta)'X\beta)\right]
$$
Aplicando propiedades de logaritmos nos queda la siguiente expresión:

$$
l(Y;X\beta)=ln(2\pi\sigma^{2})^{-n/2}-\dfrac{1}{2\sigma^{2}}(Y'Y-Y'X\beta-(X\beta)'Y+(X\beta)'X\beta)
$$
Maximizando el anterior resultado podemos llegar al score $u_{i}$:

$$
\dfrac{\partial l(Y;X\beta)}{\partial\beta}=-\dfrac{1}{2\sigma^{2}}(-2X'Y+2X'X\beta)
$$

Igualando a cero la derivada y despejando llegamos a la estimación de $\beta$:

$$
\beta=(X'X)^{-1}(X'Y)
$$

El anterior resultado es la estimación general de $\beta$ en una regresión múltiple, obtenida bajo el método de Máxima Verosimilitud.

## Método de Máxima Pseudo-Verosimilitud

El anterior método tiene la particularidad de que $y_{i}$ son IID, en la vida real muchas veces no es posible poder cumplir ese supuesto. En los procedimientos actuales se recurre a obtener una muestra compleja mediante la realización de conglomerados que tengan alguna relación particular de aglomeración para luego estratificarlos y llegar al individuo de interés que nos proporcione información sobre el estudio. Con esa muestra compleja cumplimos con que todos los individuos tienen una probabilidad de inclusión desigual sin la necesidad de utilizar un marco muestral.

A partir de eso *Pfeffermann(1993)* discutió la posibilidad de hacer inferencia en la población partiendo de la información de una muestra, para esto se propuso crear un pseudo-parámetro que tenga en cuenta el diseño muestral, es decir, que el score $u_{i}$ sea ponderado por el inverso de la probabilidad de inclusión que denominaremos $u_{i}$. Este método es conocido como Máxima Pseudo Verosimilitud(MPV).

$$
L(\theta)=\prod_{i=1}^{n}w_{i}f(y_{i},\theta)
$$
Para un mejor manejo de esta función se sugiere aplicar propiedades de los logaritmos generando la siguiente función:

$$
l(\theta)=\sum_{i=1}^{n}\ln[w_{i}f(y_{i},\theta)]
$$

Calculando las derivadas parciales de $L(\theta)$ con respecto a $\theta$ e igualando a cero tenemos un sistema de ecuaciones como sigue:

$$
\dfrac{\partial l(\theta)}{\partial\theta}=\sum_{i=1}^{n}w_{i}u_{i}(\theta)=0
$$

donde $ui=\partial\ln[f(y_{i},\theta)]/\partial\theta$ es el vector de "score" de elementos $i,i\in n$ ponderado por $w_{i}$, ahora definiremos $T$ como:

$$
T=\sum_{i=1}^{n}w_{i}u_{i}(\theta)=0
$$
Mediante la linealización de Taylor y considerando los resultados de *Binder(1983)*, podemos obtener una varianza asintóticamente insesgada de la siguiente forma:

$$
V_{p}(\hat{\theta}_{M}PV)\cong[J(\theta_{U})]^{-1}V_{p}(T)[J(\theta_{U})]^{-1}
$$

donde,

$$
J(\theta_{U})=\sum_{i\in U}\dfrac{\partial u_{i}(\theta)}{\partial(\theta)}\mid_{\theta=\theta_{U}}
$$

La estimación de la varianza anterior está definida por:

$$
\hat{V}_{p}(\hat{\theta}_{MPV})\cong[\hat{J}(\hat{\theta}_{MPV})]^{-1}\hat{V}_{p}(T)[\hat{J}(\hat{\theta}_{MPV})]^{-1}
$$
donde,

$$
\hat{J}(\theta_{MPV})=\sum_{i\in U}w_{i}\dfrac{\partial u_{i}(\theta)}{\partial(\theta)}\mid_{\theta=\theta_{MPV}}
$$

### MPV para una distribución Bernoulli

Las ecuaciones de verosimilitud dadas anteriormente, conllevan a aplicar la técnica de pseudo-verosimilitud, para la cual, en primer lugar, se definen:

$$
u_{k}(\theta)=\frac{y_{k}-\theta}{\theta(1-\theta)}
$$
Luego, las ecuaciones de pseudo-verosimilitud son:

$$
\sum_{k=1}^{n}w_{k}u_{k}(\theta)=\sum_{k=1}^{n}w_{k}\frac{y_{k}-\theta}{\theta(1-\theta)}
$$

Por lo tanto, al igualar a cero, se tiene que:

$$
\sum_{k=1}^{n}w_{k}y_{k}-\theta\sum_{k=1}^{n}w_{k}=0
$$

Por lo anterior, al despejar, se tiene que el estimador de máxima pseudo-verosimilitud, está dado por:

$$
\hat{\theta}_{MPV}=\frac{\sum_{k=1}^{n}w_{k}y_{k}}{\sum_{k=1}^{n}w_{k}}=\frac{\hat{t}_{y,\pi}}{\hat{N}}=\tilde{y}_{S}=\tilde{p}_{d}
$$

Luego, el estimador de la varianza de $\hat{\theta}_{MPV}$ es:

$$
\hat{V}_{p}(\hat{\theta}_{MPV})\cong[\hat{J}(\hat{\theta}_{MPV})]^{-1}\hat{V}_{p}(\hat{t}_{u\pi})[\hat{J}(\hat{\theta}_{MPV})]^{-1}
$$

donde

$$
\hat{J}(\theta_{MPV})=\sum_{i\in U}w_{i}\dfrac{\partial u_{i}(\theta)}{\partial(\theta)}\mid_{\theta=\hat{\theta_{MPV}}}=\frac{\hat{N}}{\tilde{y}_{S}(1-\tilde{y}_{S})}=\frac{\hat{N}}{\tilde{p}_{d}(1-\tilde{p}_{d})}
$$

Por ejemplo, bajo un muestreo aleatorio simple sin reemplazo, se tiene que el estimador de máxima pseudo-verosimilitud es $\hat{\theta}_{MPV}=\bar{y}_{S}$. Además, la estimación de su varianza es:

$$
\hat{V}_{MAS}(\hat{t}_{u\pi})=\frac{N^{2}}{n}\left(1-\frac{n}{N}\right)S_{\hat{u}_{S}}^{2}=\frac{N^{2}}{n}\left(1-\frac{n}{N}\right)\frac{1}{n-1}\sum_{k=1}^{n}(\hat{u}_{k}-\bar{\hat{u}})^{2}
$$

Luego, teniendo en cuenta que bajo este diseño de muestreo, se tiene que $\bar{\hat{u}}=0$ y que $\hat{N}=N$, entonces el estimador de la varianza de $\hat{\theta}_{MPV}$ es:

$$
\hat{V}_{MAS}(\hat{\theta}_{MPV})\cong\frac{1}{n}\left(1-\frac{n}{N}\right)S_{y_{S}}^{2}
$$
Nótese que la anterior expresión, coincide plenamente con la estimación de la varianza de la media muestral, es decir $\hat{V}_{MAS}(\hat{\theta}_{MPV})=\hat{V}_{MAS}(\bar{y}_{S})$.

### MPV para una distribución normal

Siguiendo el mismo orden de la sección de Máxima Verosimilitud, se ilustrará el método de Máxima Pseudo Verosimilitud, suponga que $f(y;\theta)$ sigue una función de distribución normal.

$$
f(y;\theta)=\dfrac{1}{\sqrt{2\pi\sigma^{2}}}\exp\left[-\dfrac{1}{2}\left(\dfrac{y_{i}-\theta^{2}}{\sigma^{2}}\right)w_{i}\right]
$$

Aplicaremos la productoria para llegar a la probabilidad conjunta:

$$
L(\theta)=\prod_{i=1}^{n}\dfrac{1}{\sqrt{2\pi\sigma^{2}}}\exp\left[-\dfrac{1}{2}\left(\dfrac{y_{i}-\theta^{2}}{\sigma^{2}}\right)w_{i}\right]
$$

Con algo de algebra llegamos a:

$$
L(\theta)=(2\pi\sigma^{2})^{-n/2}\exp[(-\dfrac{1}{2\sigma^{2}}\sum_{i=1}^{n}(y_{i}-\theta^{2})w_{i}]
$$

Utilizamos logaritmos tenemos:

$$
l(\theta)=ln(2\pi\sigma^{2})^{-n/2}[-\dfrac{1}{2\sigma^{2}}\sum_{i=1}^{n}(y_{i}-\theta^{2})w_{i}]
$$
Maximizamos la anterior expresión con derivadas parciales tenemos:

$$
\partial l(\theta)/\partial\theta=\dfrac{1}{\sigma^{2}}\sum_{i=1}^{n}(y_{i}-\theta^{2})w_{i}=0
$$

Despejando $\theta$, se llega a un resultado interesante:

$$
\theta=\dfrac{\sum_{i=1}^{n}y_{i}}{\sum_{i=1}^{n}w_{i}}=\dfrac{\hat{t}_{y\pi}}{\hat{N}}=\tilde{Y}
$$

Esto nos conlleva que, para la función $\theta$ una estimación es el promedio muestral ponderado.

### MPV para una regresión múltiple

Con el modelo de la forma $X'\beta$ se tiene una matriz $X$ de dimensión $n\times i$, donde $n$ es el tamaño de la muestra e $i$ es el número de variables predictoras, también una matriz $W$ diagonal, con los $w_{i}$, de tamaño $n\times n$, y, por último, se define dos vectores, uno $Y$ de tamaño $n$ como la variable de interés y otro $\beta$ de tamaño $i$. Con estas condiciones se puede definir una función de verosimilitud de la siguiente manera. Conociendo la función de distribución normal de $X$

$$
f(Y;X\beta)=\dfrac{1}{\sqrt{2\pi\sigma^{2}}}\exp\left[-\dfrac{1}{2\sigma^{2}}(Y-X\beta)'W(Y-X\beta)\right]
$$

Se halla la probabilidad conjunta matricialmente:

$$
L(Y;X\beta)=\prod_{i=1}^{n}\dfrac{1}{\sqrt{2\pi\sigma^{2}}}\exp\left[-\dfrac{1}{2\sigma^{2}}(Y-X\beta)'W(Y-X\beta)\right]
$$
Simplificando la anterior expresión se llega a:

$$
L(Y;X\beta)=(2\pi\sigma^{2})^{-N/2}-\exp\left[\dfrac{1}{2\sigma^{2}}(Y'WY-Y'WX\beta-(X\beta)'WY+(X\beta)'WX\beta)\right]
$$

Para poder derivar mejor, se aplica propiedades de los logaritmos:

$$
l(Y;X\beta)=ln(2\pi\sigma^{2})^{-N/2}-\dfrac{1}{2\sigma^{2}}(Y'WY-Y'WX\beta-(X\beta)'WY+(X\beta)'WX\beta)
$$
Maximizando el anterior resultado conoceremos el score $T$:

$$
T=\dfrac{\partial l(Y;X\beta)}{\partial\beta}=-\dfrac{1}{2\sigma^{2}}(-2X'WY+2X'WX\beta)
$$

Despejando $\beta$ tenemos el siguiente resultado:

$$
\beta=(X'WX)^{-1}(X'Y)
$$
Con este $\beta$ podemos estimar un modelo partiendo de una muestra probabilística compleja.
