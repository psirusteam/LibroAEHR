

# Análisis de variables categóricas 

Una variable categórica es aquella que representa categorías, clases o grupos distintos. Estas categorías pueden o no tener un orden inherente entre ellas y se utilizan para organizar la información en grupos discretos. En general, las variables categóricas pueden ser nominales (no tienen un orden específico, como la raza, el estado civil, o la región geográfica) o pueden ser ordinales (en donde las categorías sí tienen un orden). Estas variables son fundamentales en el análisis de los datos, ya que permiten clasificar y organizar la información en subgrupos que pueden facilitar la creación de políticas públicas. 

En ocasiones, desde las mismas variables continuas es posible considerar grupos o particiones que dan como resultado la creación de variables categóricas al dividir el rango de valores de la variable en intervalos. Un ejemplo de esto es la variable edad, que en una encuesta de hogares se pregunta como variable cuantitativa, pero que se puede dividir, por ejemplo, en las siguientes categorías: primera infancia (de 0 a 5 años), niñez (de 6 a 11 años), adolescencia (de 12 a 18 años), juventud (de 19 a 26 años), adultez (de 27 a 59 años), adulto Mayor (60 años o más).  


## Definición del diseño de muestreo

Se inicia este capítulo haciendo la definición del diseño de muestreo (como se mostró en capítulos anteriores) usando como ejemplo la misma base de datos del capítulo anterior. 


```r
library(tidyverse)
library(survey)
library(srvyr)
options(survey.lonely.psu = "adjust")

encuesta <- readRDS("Data/encuesta.rds")

diseno <- encuesta %>%
  as_survey_design(
    strata = Stratum,
    ids = PSU,
    weights = wk,
    nest = TRUE
  )
```

A continuación, para efectos del ejemplo, se generan tres nuevas variables dicotómicas que indican si la persona encuestada está en estado de pobreza, o no; si está desempleada, o no; y si es mayor de 18 años, o no. Estas nuevas variables categórica nacen de variables propias de la encuesta, como lo son el ingreso percápita, el estado de ocupación y la edad en años 


```r
diseno <- diseno %>%
  mutate(
    pobreza = ifelse(Poverty != "NotPoor", 1, 0),
    desempleo = ifelse(Employment == "Unemployed", 1, 0),
    edad_18 = case_when(Age < 18 ~ "< 18 anios", 
                        TRUE ~ ">= 18 anios")
  )
```

Como se pudo observar en el código anterior, se ha introducido la función `case_when` la cual es una extensión de la función `ifelse` que permite crear múltiples categorías a partir de una o varias condiciones. Asimismo, como se ha mostrado anteriormente, en ocasiones se desea realizar estimaciones por subpoblación; en este caso se extraen cuatro subgrupos de la encuesta y se definen a continuación:


```r
sub_Urbano <- diseno %>%  filter(Zone == "Urban")
sub_Rural  <- diseno %>%  filter(Zone == "Rural")
sub_Mujer  <- diseno %>%  filter(Sex == "Female")
sub_Hombre <- diseno %>%  filter(Sex == "Male")
```

## Estimación puntual

La estimación precisa de tamaños absolutos y proporciones en encuestas de hogares es fundamental para obtener datos representativos que reflejen la realidad demográfica y socioeconómica de una población. Estas cifras sirven como base para la toma de decisiones de política pública, para la asignación de recursos y para el diseño de programas sociales. 

La capacidad de entender la distribución de categorías específicas, como situación de pobreza, estado de ocupación, escolaridad, entre otras, aporta información valiosa para abordar desigualdades y promover el desarrollo equitativo. 

### Estimaciones de tamaños

En esta sección se realizarán los procesos de estimación de variables categóricas. En primera instancia, uno de los parámetros más importantes es el tamaño de una población, que representa la cardinalidad de ese conjunto; es decir, el número total de integrantes que lo componen. En términos de notación, el tamaño de la población se estima de la siguiente manera:

\begin{eqnarray}
\hat{N}_{\omega} = \sum_{h=1}^{H}\sum_{\alpha=1}^{a_{h}}\sum_{i=1}^{n_{h\alpha}}\omega_{h\alpha i}
\end{eqnarray}


De la misma manera, la estimación del tamaño en una subpoblación está definida por una variable dicotómica $I(y_i = d)$, que toma el valor uno si el individuo $i$ pertenece a la categoría $d$ en la variable discreta, está dada por la siguiente expresión:

\begin{eqnarray}
\hat{N}^d_{\omega} = \sum_{h=1}^{H}\sum_{\alpha=1}^{a_{h}}\sum_{i=1}^{n_{h\alpha}}\omega_{h\alpha i}I(y_i = d)
\end{eqnarray}

A continuación, se presenta la forma apropiada para estimar los tamaños de la población finita y sus subpoblaciones.


```r
diseno %>%
  group_by(Zone) %>%
  cascade(n = unweighted(n()),
          Nd = survey_total(vartype = c("se", "ci")),
          .fill = "Poblacional") %>%
  arrange(desc(Zone))
```

```
## # A tibble: 3 × 6
##   Zone            n      Nd Nd_se  Nd_low  Nd_upp
##   <chr>       <int>   <dbl> <dbl>   <dbl>   <dbl>
## 1 Urban        1308  78164. 2847.  72526.  83802.
## 2 Rural        1297  72102. 3062.  66039.  78165.
## 3 Poblacional  2605 150266. 4181. 141986. 158546.
```

En la tabla anterior, `n` denota el número de observaciones en la muestra por Zona y `Nd` denota la estimación del tamaño (número de personas) en cada subpoblación. Adicionalmente, en el código anterior se introdujo la función `unweighted`, que calcula resúmenes no ponderados a partir de un conjunto de datos de encuestas. Para el ejemplo, el tamaño de muestra en la zona rural fue de 1297 personas y para la urbana fue de 1308. Con esta información se logró estimar una población de 72102 con un error estándar de 3062 en la zona rural; además, se estimó una población de 78164 en la zona urbana con un error estándar de 2847. Así mismo, con una confianza del 95% se construyeron los intervalos de confianza para el tamaño de las poblaciones que, en la zona rural está entre 66038 y 78165, mientras que para la urbana están entre 72526 y 83801.

Ahora bien, empleando una sintaxis similar a la anterior, es posible estimar el número de personas en condición de pobreza extrema, pobreza relativa y personas no pobres como sigue:


```r
diseno %>%
  group_by(Poverty) %>%
  summarise(Nd = survey_total(vartype = c("se", "ci")))
```

```
## # A tibble: 3 × 5
##   Poverty      Nd Nd_se Nd_low  Nd_upp
##   <fct>     <dbl> <dbl>  <dbl>   <dbl>
## 1 NotPoor  91398. 4395. 82696. 100101.
## 2 Extreme  21519. 4949. 11719.  31319.
## 3 Relative 37349. 3695. 30032.  44666.
```

De la tabla anterior podemos concluir que, la cantidad estimada de personas que no se encuentran en pobreza es de 91398: mientras que 37348 personas se encuentran en pobreza y 21518 en pobreza extrema. Los demás parámetros estimados se interpretan de la misma manera que para la estimación desagregada por zona. En forma similar, es posible estimar el número total de personas que están por debajo de la línea de pobreza. 


```r
diseno %>%
  group_by(pobreza) %>%
  summarise(Nd = survey_total(vartype = c("se", "ci")))
```

```
## # A tibble: 2 × 5
##   pobreza     Nd Nd_se Nd_low  Nd_upp
##     <dbl>  <dbl> <dbl>  <dbl>   <dbl>
## 1       0 91398. 4395. 82696. 100101.
## 2       1 58868. 5731. 47519.  70216.
```

Concluyendo que, 58867 personas están por debajo de la línea de pobreza con un error estándar de 5731 y un intervalo de confianza que va desde 47518 hasta 70216. 

Otra variable de interés en encuestas de hogares es el estado de ocupación de las personas. A continuación, se muestra el código computacional que estima el tamaño de cada una de sus categorías:


```r
diseno %>%
  group_by(Employment) %>%
  summarise(Nd = survey_total(vartype = c("se", "ci")))
```

```
## # A tibble: 4 × 5
##   Employment     Nd Nd_se Nd_low Nd_upp
##   <fct>       <dbl> <dbl>  <dbl>  <dbl>
## 1 Unemployed  4635.  761.  3129.  6141.
## 2 Inactive   41465. 2163. 37183. 45748.
## 3 Employed   61877. 2540. 56847. 66907.
## 4 <NA>       42289. 2780. 36784. 47794.
```

De los resultados de la función, se puede estimar que 4634 personas están desempleadas con un intervalo de confianza entre 3128 y 6140. Además, se estima que 41465 personas están inactivas, con un intervalo de confianza entre 37182 y 45747. Por último, se estima que 61877 personas están ocupadas con un intervalo de confianza entre 36784 y 47793.

### Estimación de proporciones

La estimación de una proporción para una variable de respuesta binaria requiere una extensión directa del estimador de razón mostrado en el capítulo anterior. Como lo mencionan @Heeringa_West_Berglund_2017, al recodificar las categorías de respuesta originales en una sola variable indicadora $y_{i}$ con valores posibles de 1 y 0 (por ejemplo, sí = 1, no = 0), se define el estimador de una proporción de la siguiente manera:

\begin{eqnarray}
\hat{p}_{\omega}^d = \frac{\hat{N}^d_{\omega}}{\hat{N}_{\omega}}
= \frac{\sum_{h=1}^{H}\sum_{\alpha=1}^{a_{h}}\sum_{i=1}^{n_{h\alpha}}\omega_{h\alpha i}\ I(y_i = d)}{\sum_{h=1}^{H}\sum_{\alpha=1}^{a_{h}}\sum_{i=1}^{n_{h\alpha}}\omega_{h\alpha i}}
\end{eqnarray}

Aplicando Linealización de Taylor al anterior estimador, se tiene que su varianza está dada por la siguiente expresión:

$$
var\left(\hat{p}_{\omega}^d\right) \dot{=} \frac{var\left(\hat{N}^{d}_{\omega}\right)+(\hat{p}_{\omega}^d)^{2}var\left(\hat{N}_{\omega}\right)-2\,\hat{p}_{\omega}^d\,cov\left(\hat{N}^{d}_{\omega},\hat{N}_{\omega}\right)}{(\hat{N}_{\omega})^{2}}
$$


Es normal observar que muchos paquetes estadísticos opten por generar estimaciones de proporciones y errores estándar en la escala de porcentaje. `R` genera las estimaciones de proporciones dentro del intervalo [0,1]. A continuación, se presenta el código computacional para estimar la proporción de personas por zona:


```r
diseno %>%
  group_by(Zone) %>%
  summarise(prop = survey_mean(vartype = c("se", "ci"),
                               proportion = TRUE))
```

```
## # A tibble: 2 × 5
##   Zone   prop prop_se prop_low prop_upp
##   <chr> <dbl>   <dbl>    <dbl>    <dbl>
## 1 Rural 0.480  0.0140    0.452    0.508
## 2 Urban 0.520  0.0140    0.492    0.548
```

Como se pudo observar, se usó la función `survey_mean` para la estimación. Sin embargo, con el parámetro `proportion = TRUE`, se le indica a `R` que lo que se desea estimar es una proporción. Para este ejemplo se puede estimar que el 47.9% de las personas viven en zona rural obteniendo un intervalo de confianza comprendido entre (45.2%,	50.7%); además el 52% de las personas viven en la zona urbana con un intervalo de confianza de (49.2%, 54.7%).

La librería `survey` tiene implementado una función específica para estimar proporciones la cual es `survey_prop` que genera los mismos resultados mostrados anteriormente. Le queda al lector la decisión de usar la función con la que más cómodo se sienta. A continuación, se muestra un ejemplo del uso de la función `survey_prop`.



```r
diseno %>%
  group_by(Zone) %>%
  summarise(prop = survey_prop(vartype = c("se", "ci")))
```

```
## # A tibble: 2 × 5
##   Zone   prop prop_se prop_low prop_upp
##   <chr> <dbl>   <dbl>    <dbl>    <dbl>
## 1 Rural 0.480  0.0140    0.452    0.508
## 2 Urban 0.520  0.0140    0.492    0.548
```

Como es bien sabido en la literatura especializada, cuando la proporción de interés estimada está cerca de cero o de uno, los límites del intervalo de confianza tradicional, basados en el diseño de muestreo, pueden salirse de los rangos permitidos para las proporciones. Lo anterior no tendría ninguna interpretación por la naturaleza del parámetro. Es por esto que, para solventar este problema, se pueden realizar estimaciones alternativas de los intervalos de confianza basados en el diseño de muestreo como lo proponen @Rust2007ConfidenceIF y @DeanPagano2015. De esta manera, el intervalo de confianza utilizando la transformación $Logit\left(p\right)$
está dado por:

$$
IC\left[logit\left(p^d\right)\right]  =  \left\{ ln\left(\frac{\hat{p}_{\omega}^d}{1-\hat{p}_{\omega}^d}\right)\pm\frac{t_{1-\alpha/2,\,gl} \times   se\left(\hat{p}_{\omega}^d\right)}{\hat{p}_{\omega}^d\left(1-\hat{p}_{\omega}^d\right)}\right\} 
$$

Por tanto, el intervalo de confianza para $p^d$ sería:

\begin{eqnarray}
IC\left(p^d\right)  =  \left\{ \frac{exp\left[ln\left(\frac{\hat{p}_{\omega}^d}{1-\hat{p}_{\omega}^d}\right)\pm\frac{t_{1-\alpha/2,\,gl}\times se\left(\hat{p}_{\omega}^d\right)}{\hat{p}_{\omega}^d\left(1-\hat{p}_{\omega}^d\right)}\right]}{1+exp\left[ln\left(\frac{\hat{p}_{\omega}^d}{1-\hat{p}_{\omega}^d}\right)\pm\frac{t_{1-\alpha/2,\,gl}\times se\left(\hat{p}_{\omega}^d\right)}{\hat{p}_{\omega}^d\left(1-\hat{p}_{\omega}^d\right)}\right]}\right\} 
\end{eqnarray}

A continuación, siguiendo con la base de ejemplo, se estima la proporción de hombres y mujeres en pobreza y no pobreza junto con su error estándar e intervalos de confianza.


```r
diseno %>%
  group_by(pobreza, Sex) %>%
  summarise(prop = survey_prop(vartype = c("se", "ci"))) 
```

```
## # A tibble: 4 × 6
## # Groups:   pobreza [2]
##   pobreza Sex     prop prop_se prop_low prop_upp
##     <dbl> <chr>  <dbl>   <dbl>    <dbl>    <dbl>
## 1       0 Female 0.529  0.0124    0.505    0.554
## 2       0 Male   0.471  0.0124    0.446    0.495
## 3       1 Female 0.524  0.0159    0.492    0.555
## 4       1 Male   0.476  0.0159    0.445    0.508
```

Como se puede observar, se ha estimado que entre las personas en condición de pobreza, el 52.3% son mujeres y el 47.6% son hombres; generando intervalos de confianza al 95% de (49.2%,	55.5%) para las mujeres y (44.5%,	50.7%) para los hombres.

En la librería survey existe una alternativa para estimar tablas de proporciones utilizando la función `svyby`. Los argumentos que requiere la función se definen a partir de la la variable que se desea estimar (`formula`), las categorías por la cual se desea estimar (`by`), el diseño muestral (`desing`) y el parámetro que se desea estimar (`FUN`). A continuación, se ejemplifica el uso de la función:


```r
tab_Sex_Pobr <- svyby(
  formula = ~ Sex,
  by =  ~ pobreza,
  design = diseno,
  FUN = svymean
)

tab_Sex_Pobr
```

```
##   pobreza SexFemale   SexMale se.SexFemale se.SexMale
## 0       0 0.5291800 0.4708200   0.01242026 0.01242026
## 1       1 0.5236123 0.4763877   0.01586237 0.01586237
```



Para la estimación de los intervalos de confianza (que coinciden con los generados anteriormente usando la funicón `group_by`.) se utiliza la función `confint` como sigue:


```r
confint(tab_Sex_Pobr) 
```

```
##                 2.5 %    97.5 %
## 0:SexFemale 0.5048367 0.5535232
## 1:SexFemale 0.4925226 0.5547019
## 0:SexMale   0.4464768 0.4951633
## 1:SexMale   0.4452981 0.5074774
```

Otro análisis de interés relacionado con tablas de doble entrada en encuestas de hogares es estimar el porcentaje de desempleados por sexo.


```r
tab_Sex_Ocupa <- svyby(
  formula = ~ Sex,
  by = ~ Employment,
  design = diseno,
  FUN = svymean
)
tab_Sex_Ocupa
```

```
##            Employment SexFemale   SexMale se.SexFemale se.SexMale
## Unemployed Unemployed 0.2726730 0.7273270   0.05351318 0.05351318
## Inactive     Inactive 0.7703406 0.2296594   0.02340005 0.02340005
## Employed     Employed 0.4051575 0.5948425   0.01851986 0.01851986
```

De la anterior salida se puede estimar que, dentro de los desempleado, el 27.2% son mujeres y el 72.7% son. Por la naturaleza simétrica de las proporciones con dos únicos grupos, los errores estándares para estas estimaciones coinciden y se estiman en 5.3%. Los intervalos de confianza se muestran a continuación:


```r
confint(tab_Sex_Ocupa) 
```

```
##                          2.5 %    97.5 %
## Unemployed:SexFemale 0.1677891 0.3775570
## Inactive:SexFemale   0.7244773 0.8162038
## Employed:SexFemale   0.3688592 0.4414557
## Unemployed:SexMale   0.6224430 0.8322109
## Inactive:SexMale     0.1837962 0.2755227
## Employed:SexMale     0.5585443 0.6311408
```

Si ahora el objetivo es estimar la pobreza, pero por las distintas regiones que se tienen en la base de datos, lo primero que se debe realizar es la conversión de la variable `pobreza`, la cual de de tipo numérica, en tipo factor; luego se realiza la estimación con la función `svyby`.



```r
svyby(
  formula = ~ as.factor(pobreza),
  by = ~ Region,
  design =  diseno,
  FUN = svymean
)
```

```
##              Region as.factor(pobreza)0 as.factor(pobreza)1
## Norte         Norte           0.6410318           0.3589682
## Sur             Sur           0.6561536           0.3438464
## Centro       Centro           0.6346152           0.3653848
## Occidente Occidente           0.5991839           0.4008161
## Oriente     Oriente           0.5482079           0.4517921
##           se.as.factor(pobreza)0 se.as.factor(pobreza)1
## Norte                 0.05547660             0.05547660
## Sur                   0.04348901             0.04348901
## Centro                0.07858599             0.07858599
## Occidente             0.04670473             0.04670473
## Oriente               0.08849644             0.08849644
```

De lo anterior se puede concluir que, en la región Norte, el 35% de las personas están en estado de pobreza mientras que en el sur es el 34%. La pobreza más alta se tiene en la región oriente con una estimación de 45%. 

Si el interés ahora se centra en estimar proporciones en subpoblaciones desagregadas, por zona, el código computacional apropiado es el siguiente:


```r
sub_Urbano %>%
  group_by(Sex) %>%
  summarise(prop = survey_prop(vartype = c("se", "ci")))
```

```
## # A tibble: 2 × 5
##   Sex     prop prop_se prop_low prop_upp
##   <chr>  <dbl>   <dbl>    <dbl>    <dbl>
## 1 Female 0.537  0.0130    0.511    0.563
## 2 Male   0.463  0.0130    0.437    0.489
```

Arrojando como resultado una estimación enn donde el 53.6% de las mujeres y 46.4%  de los hombres viven en la zona urbana con intervalos de confianza entre (51%, 56.2%) y (43.7%, 48.9%), respectivamente. Realizando el mismo ejercicio anterior, pero ahora en la zona rural se tiene:


```r
sub_Rural %>%
  group_by(Sex) %>%
  summarise(n = unweighted(n()),
            prop = survey_prop(vartype = c("se", "ci")))
```

```
## # A tibble: 2 × 6
##   Sex        n  prop prop_se prop_low prop_upp
##   <chr>  <int> <dbl>   <dbl>    <dbl>    <dbl>
## 1 Female   679 0.516 0.00824    0.500    0.533
## 2 Male     618 0.484 0.00824    0.467    0.500
```

De donde se estima que el 51.6% de las mujeres y el 48.4% de los hombres viven en la zona rural con intervalos de confianza de (49.9%, 53.2%) y (46.7%, 50.0%), respectivamente. Ahora bien, si nos centramos solo en la población de hombres en la base de datos y se desea estimar la proporción de hombres por zona, el código computacional es el siguiente:


```r
sub_Hombre %>%
  group_by(Zone) %>%
  summarise(prop = survey_prop(vartype = c("se", "ci")))
```

```
## # A tibble: 2 × 5
##   Zone   prop prop_se prop_low prop_upp
##   <chr> <dbl>   <dbl>    <dbl>    <dbl>
## 1 Rural 0.491  0.0178    0.455    0.526
## 2 Urban 0.509  0.0178    0.474    0.545
```

En la anterior tabla se puede observar que el 49% de los hombres están en la zona rural y el 51% en la zona urbana. Si se realiza ahora el mismo ejercicio para la mujeres, el código computacional es el siguiente:


```r
sub_Mujer %>%
  group_by(Zone) %>%
  summarise(prop = survey_prop(vartype = c("se", "ci")))
```

```
## # A tibble: 2 × 5
##   Zone   prop prop_se prop_low prop_upp
##   <chr> <dbl>   <dbl>    <dbl>    <dbl>
## 1 Rural 0.470  0.0140    0.443    0.498
## 2 Urban 0.530  0.0140    0.502    0.557
```

De la tabla anterior se puede inferir que, el 47% de las mujeres están en la zona rural y el 52% en la zona urbana. Observando también  intervalos de confianza al 95% de (44%, 49%) y (50%, 55%) para las zonas rural y urbana, respectivamente. 

Si, dentro de la base de datos filtrada por hombres, ahora se desea estimar por varios niveles de desagregación, se debería recurrir al uso de la función `group_by`, la cual hace posible combinar dos o más variables dentro de un filtro. Por ejemplo, si se desea estimar la proporción de hombres por zona y en estado de pobreza, se realiza de la siguiente manera:


```r
sub_Hombre %>%
  group_by(Zone, Poverty) %>%
  summarise(prop = survey_prop(vartype = c("se", "ci")))
```

```
## # A tibble: 6 × 6
## # Groups:   Zone [2]
##   Zone  Poverty   prop prop_se prop_low prop_upp
##   <chr> <fct>    <dbl>   <dbl>    <dbl>    <dbl>
## 1 Rural NotPoor  0.549  0.0626   0.424     0.668
## 2 Rural Extreme  0.198  0.0675   0.0958    0.364
## 3 Rural Relative 0.254  0.0372   0.187     0.334
## 4 Urban NotPoor  0.660  0.0366   0.584     0.728
## 5 Urban Extreme  0.113  0.0245   0.0726    0.171
## 6 Urban Relative 0.227  0.0260   0.180     0.283
```

De la salida anterior se puede estimar que, en la ruralidad, el 19% de los hombres están en pobreza extrema, mientras que en la zona urbana el 11% lo está. Por otro lado, se estima que el 54% de los hombres que viven en la zona rural no están en pobreza mientras que, en la zona urbana el 65% no está en esta condición.

Otro parámetro de interés es estimar en encuestas de hogares es la proporción de personas en condición de pobreza asociada a la edad; por ejemplo, personas menores y mayores de 18 años. A continuación, ejemplificamos la estimación de estos subgrupos cruzado por pobreza:


```r
diseno %>%
  group_by(edad_18, pobreza) %>%
  summarise(Prop = survey_prop(vartype =  c("se", "ci")))
```

```
## # A tibble: 4 × 6
## # Groups:   edad_18 [2]
##   edad_18     pobreza  Prop Prop_se Prop_low Prop_upp
##   <chr>         <dbl> <dbl>   <dbl>    <dbl>    <dbl>
## 1 < 18 anios        0 0.498  0.0373    0.425    0.572
## 2 < 18 anios        1 0.502  0.0373    0.428    0.575
## 3 >= 18 anios       0 0.665  0.0298    0.603    0.721
## 4 >= 18 anios       1 0.335  0.0298    0.279    0.397
```

De la anterior salida se puede observar que, el 50% de los menores de edad y el 33% de los mayores de edad están en estado de pobreza. Al observar los intervalos de confianza para los menores de edad en estado de pobreza se puede observar que, dicha estimación puede llegar, con una confianza del 95% a 57% mientras que a los mayores de edad puede llegar a 39%.

Como se mencionó al inicio del capítulo, es posible categorizar una variable de tipo cuantitativo como por ejemplo la edad y cruzarla con la variable que categoriza la empleabilidad. A continuación, se estiman las proporciones de mujeres por edad y condición de ocupación:


```r
sub_Mujer %>%
  mutate(edad_rango =
           case_when(Age >= 18 & Age <= 35  ~ "18 - 35",
                     TRUE ~ "Otro")) %>%
  group_by(edad_rango, Employment) %>%
  summarise(Prop = survey_prop(vartype =  c("se", "ci"))) 
```

```
## # A tibble: 7 × 6
## # Groups:   edad_rango [2]
##   edad_rango Employment   Prop Prop_se Prop_low Prop_upp
##   <chr>      <fct>       <dbl>   <dbl>    <dbl>    <dbl>
## 1 18 - 35    Unemployed 0.0289 0.00914  0.0154    0.0537
## 2 18 - 35    Inactive   0.517  0.0379   0.442     0.591 
## 3 18 - 35    Employed   0.455  0.0357   0.385     0.526 
## 4 Otro       Unemployed 0.0102 0.00403  0.00462   0.0222
## 5 Otro       Inactive   0.353  0.0207   0.313     0.395 
## 6 Otro       Employed   0.255  0.0217   0.214     0.300 
## 7 Otro       <NA>       0.382  0.0223   0.339     0.427
```

De la anterior tabla se puede observar, entre otros que, de las mujeres con edades entre 18 y 35 años el 2.8% están desempleadas, mientras que el 45% están empleadas. Análisis similares se pueden hacer para los demás rangos de edades. 

## Relación entre varias variables

Las tablas de contingencia y las pruebas de independencia son herramientas esenciales en el análisis de las encuestas de hogares, puesto que permiten analizar relaciones entre variables categóricas. Estas tablas organizan las estimaciones poblacionales en función de dos o más características, revelando patrones y asociaciones. Las pruebas de independencia evalúan si las variables están relacionadas o son independientes. Este análisis es crucial en investigaciones y toma de decisiones, ya que proporciona información sobre la dependencia entre factores, influyendo en la formulación de estrategias basadas en estimaciones precisas y exactas.

### Tablas 

En la literatura especializada las tablas también se denominan como tablas de contingencia o tablas cruzadas. En general, una tabla se asume como un arreglo bidimensional de $r=1,\ldots,R$ filas y $c=1,\ldots,C$ columnas. Estas son herramientas muy utilizadas en el análisis de encuestas de hogares puesto que, al estar conformada por al menos dos filas y dos columnas, representan información de variables categóricas en términos de conteos de frecuencia al mismo tiempo.  Estas tablas tienen el objetivo de representar de manera resumida la relación entre diferentes variables categóricas. En la muestra no expandida, estas tablas se definen con frecuencias no ponderadas como se muestra a continuación:

| Variable 2        | Variable 1                  | |
|-------------------|-------------|---------------|--------------|
|                   | 0           | 1             | Marginal fila             |
| 0                 | $n^{00}$    |   $n^{01}$    | $n^{0+}$     |
| 1                 |  $n^{10}$   |  $n^{11}$     | $n^{1+}$     |
| Marginal columna  |  $n^{+0}$   |    $n^{+1}$   |  $n^{++}$    |

Mientras que, en un análisis ponderado sobre la muestra expandida, la tabla de doble entrada se presenta con la estimación poblacional de las frecuencias, justo como sigue:

| Variable 2        | Variable 1                  | |
|-------------------|-------------|---------------|--------------|
|                   | 0           | 1             | Marginal fila             |
| 0                 | $\hat{N}^{00}_{\omega}$|   $\hat{N}^{01}_{\omega}$| $\hat{N}^{0+}_{\omega}$|
| 1                 | $\hat{N}^{10}_{\omega}$|   $\hat{N}^{11}_{\omega}$| $\hat{N}^{1+}_{\omega}$|
| Marginal columna  | $\hat{N}^{+0}_{\omega}$|   $\hat{N}^{+1}_{\omega}$| $\hat{N}_{\omega}$|


De esta manera, teniendo en cuenta que el subíndice $i\in\left(r,c\right)$ representa a los individuos que están clasificados en la celda ($r, c$), entonces el estimador de la frecuencia en esta celda está dado por la siguiente expresión. 

\begin{eqnarray}
\hat{N}^{rc}_{\omega}={ \sum_{h=1}^{H}\sum_{\alpha=1}^{\alpha_{h}}\sum_{i\in\left(r,c\right)}^{n_{h\alpha}}}\omega_{h\alpha i}
\end{eqnarray}

Los estimadores de las demás frecuencias en la tabla se definen de forma similar, inclusive las marginales por fila y por columna. Las proporciones estimadas a partir de estas frecuencias muestrales ponderadas, se obtienen de la siguiente manera 

\begin{eqnarray}
\hat{p}_{\omega}^{rc}=\frac{\hat{N}^{rc}_{\omega}}{\hat{N}_{\omega}}
\end{eqnarray}

Utilizando la función `group_by` es posible obtener resultados por más de un nivel de agregación. A continuación, se muestra la estimación ocupación desagregada por niveles de pobreza:


```r
diseno %>%
  group_by(Employment, Poverty) %>%
  cascade(Nd = survey_total(vartype = c("se", "ci")),
          .fill = "Total") 
```

```
## # A tibble: 17 × 6
## # Groups:   Employment [5]
##    Employment Poverty       Nd Nd_se  Nd_low  Nd_upp
##    <fct>      <fct>      <dbl> <dbl>   <dbl>   <dbl>
##  1 Unemployed NotPoor    1768.  405.    966.   2571.
##  2 Unemployed Extreme    1169.  348.    480.   1859.
##  3 Unemployed Relative   1697.  458.    791.   2604.
##  4 Unemployed Total      4635.  761.   3129.   6141.
##  5 Inactive   NotPoor   24346. 1736.  20908.  27784.
##  6 Inactive   Extreme    6422. 1321.   3807.   9037.
##  7 Inactive   Relative  10697. 1460.   7806.  13589.
##  8 Inactive   Total     41465. 2163.  37183.  45748.
##  9 Employed   NotPoor   44600. 2596.  39460.  49741.
## 10 Employed   Extreme    5128. 1122.   2907.   7349.
## 11 Employed   Relative  12149. 1347.   9483.  14816.
## 12 Employed   Total     61877. 2540.  56847.  66907.
## 13 Total      Total    150266. 4181. 141986. 158546.
## 14 <NA>       NotPoor   20684. 1257.  18195.  23172.
## 15 <NA>       Extreme    8800. 2980.   2900.  14701.
## 16 <NA>       Relative  12805. 1551.   9734.  15876.
## 17 <NA>       Total     42289. 2780.  36784.  47794.
```


De las anteriores salidas se puede estimar, entre otros, que 44600 personas que trabajan no son pobres con un intervalo de confianza entre 39459 y 49741. Asimismo, se estima que 6421 personas se encuentran inactivas y al mismo tiempo están en situación de pobreza extrema con un intervalo de confianza entre 3806 y 9037. Para obtener un arreglo rectangular con las estimaciones anteriores, es posible utilizar la función `svytable` del paquete `survey` de la siguiente manera:


```r
svytable( ~ Poverty + Employment, diseno)
```

```
##           Employment
## Poverty    Unemployed  Inactive  Employed
##   NotPoor    1768.375 24346.008 44600.347
##   Extreme    1169.201  6421.825  5127.531
##   Relative   1697.231 10697.414 12149.142
```


Por otro lado, también es posible tener tablas que reporten las estimaciones de frecuencias relativas, o porcentajes, en la población. Este análisis se hace, por supuesto, de manera ponderada sobre la muestra expandida. La tabla de doble entrada con la estimación poblacional de las proporciones se presenta a continuación:


| Variable 2        | Variable 1                  | |
|-------------------|-------------|---------------|--------------|
|                   | 0           | 1             | Marginal fila             |
| 0                 | $\hat{p}^{00}_{\omega}$|   $\hat{p}^{01}_{\omega}$| $\hat{p}^{0+}_{\omega}$|
| 1                 | $\hat{p}^{10}_{\omega}$|   $\hat{p}^{11}_{\omega}$| $\hat{p}^{1+}_{\omega}$|
| Marginal columna  | $\hat{p}^{+0}_{\omega}$|   $\hat{p}^{+1}_{\omega}$| $\hat{p}_{\omega}$|


De la misma manera que para las frecuencias absolutas, teniendo en cuenta que el subíndice $i\in\left(r,c\right)$ representa a los individuos que están clasificados en la celda ($r, c$), entonces el estimador de la proporción asociada a esta celda está dado por la siguiente expresión. 

$$
\hat{p}^{rc}_{\omega}=\frac{\hat{N}^{rc}_{\omega}}{\hat{N}_{\omega}}=
\frac{\sum_{h=1}^{H}\sum_{\alpha=1}^{\alpha_{h}}\sum_{i\in\left(r,c\right)}^{n_{h\alpha}}\omega_{h\alpha i}}{\sum_{h=1}^{H}\sum_{\alpha=1}^{\alpha_{h}}\sum_{i=1}^{n_{h\alpha}}\omega_{h\alpha i}}
$$ 

Por ejemplo, si se desea estimar la proporción de personas por zona y en estado de pobreza, se realiza de la siguiente manera:


```r
svytable( ~ Poverty + Zone, diseno, Ntotal = 1)
```

```
##           Zone
## Poverty         Rural      Urban
##   NotPoor  0.26460893 0.34363467
##   Extreme  0.08547174 0.05773275
##   Relative 0.12974844 0.11880348
```

### Pruebas de independencia

Sobre las tablas estimadas, es posible realizar pruebas de independecia para corroborar si existe o no asociación entre dos variables de tipo categórico. Que dos variables sean independientes significa que el comportamiento estructural de una variable no depende de la otra, ni viceversa. @Heeringa_West_Berglund_2017 afirman que, bajo muestreo aleaotrio simple, dos variables categóricas son independientes si la proporción esperada en la fila $r$ y la columna $c$, denotada como $\pi^{rc}$, guarda la siguiente relación:

$$
\pi^{rc} = \frac{n^{r+}\times n^{+c}}{(n^{++})^2}
$$

De esta forma, una manera de corroborar si existe independencia entras las variables de interés es comparar directamente las proporciones estimadas $\hat{p}^{rc}_{\omega}$ con las proporciones esperadas $\pi^{rc}$. Si hay una diferencia muy grande entre ellas, entonces la hipótesis de independencia no sería corroborada por los datos recolectados. Por ende, se define la siguiente estadística $\chi^{2}_{RS}$ [@rao1984adjusted], que sigue una distribución Ji cuadrado con $(R-1) \times (C-1)$ grados de libertad.

\begin{eqnarray}
\chi^{2}_{RS}  =  \frac{\chi^{2}_{Pearson}}{GDEFF}
\end{eqnarray}

En donde

$$
\chi^{2}_{Pearson} = n^{++}\ \left(\sum_r\sum_c (\hat{p}^{rc}_{\omega} -\pi^{rc} )^2/\pi^{rc}\right)
$$

Además, $GDEFF$ es una estimación del efecto de diseño generalizado de Rao–Scott, dado por 


$$
GDEFF=\frac{\sum_{r}\sum_{c}\left(1-p_{rc}\right)d^{2}\left(p_{rc}\right)-\sum_{r}\left(1-p_{r+}\right)d^{2}\left(p_{r+}\right)-\sum_{c}\left(1-p_{+c}\right)d^{2}\left(p_{+c}\right)}{\left(R-1\right)\left(C-1\right)}
$$





Como lo mencionan @Heeringa_West_Berglund_2017, fueron @fay1979adjusting, junto con @fellegi1980approximate quienes empezaron a proponer la corrección del estadístico chi-cuadrado de Pearson basada en un efecto de diseño generalizado.  @rao1984chi_squared y más tarde @thomas1987small_sample ampliaron la teoría de las correcciones del efecto de diseño generalizado para estas pruebas estadísticas. El método de Rao-Scott requiere el cálculo de efectos de diseño generalizados que son analíticamente más complicados que el enfoque de Fellegi. Las correcciones de Rao-Scott son ahora el estándar en los procedimientos para el análisis de datos de encuestas categóricas en sistemas de software como Stata y SAS. 

Adicional a lo anterior, la prueba de independencia F de Fisher permite analizar si dos variables dicotómicas están asociadas cuando la muestra que se observó es demasiado pequeña y no se cumplen las condiciones para aplicar la prueba $\chi^{2}_{Pearson}$. Para utilizar esta técnica, tengamos en cuenta las expresiones para la probabilidad estimada y la misma estadística $\chi{2}$ de Pearson. A partir de estas, la estadística de razón de verosimilitud se define como:

$$
G^{2}=2\times n_{++}\times\sum_{r}\sum_{c}p_{cr}\times\ln\left(\frac{p_{rc}}{\hat{\pi}_{rc}}\right)
$$

donde, $r$ es el número de filas y $c$ representa el número de columnas, la prueba tiene $(R-1)\times (C-1)$ grados de libertad. Realizando una corrección por el efecto de diseño generalizado, la estadística basada en la razón de verosimilitud se calcula como:


$$
G^2_{(R-S)}  =  G^2\big/GDEFF
$$


Por tanto, la estadística F para independencia basada en la prueba chi-cuadrado de Pearson se calcula como sigue:

$$
F_{R-S,Pearson}=\chi_{R-S}^{2}\big/\left[\left(R-1\right)\left(C-1\right)\right]\sim F_{\left(R-1\right)\left(C-1\right),\left(R-1\right)\left(C-1\right)df}
$$

y, la estadística F para independencia basada en la razón de verosimilitudes se calcula como sigue:

$$
F_{R-S,LRT}=G_{R-S}^{2}\big/\left(C-1\right)\sim F_{\left(C-1\right),df}
$$

donde $C$ es el número de columnas de la tabla cruzada. 


Para realizar la prueba de independencia $\chi^{2}_{RS}$ en `R`, se utilizará la función `svychisq` del paquete `survey`. Esta función requiere que se definan las variables de interés (`formula`) y el diseño muestral (`desing`). Ahora, para ejemplificar el uso de esta función tomaremos la base de datos de ejemplo y se probará si la pobreza es independiente del sexo. A continuación, se presentan los códigos computacionales: 


```r
svychisq(formula = ~ Sex + pobreza,
         design = diseno,
         statistic = "F")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  NextMethod()
## F = 0.056464, ndf = 1, ddf = 119, p-value = 0.8126
```

De la anterior salida, se puede concluir con una confianza del 95% y basado en las estimaciones sobre la muestra expandida que la pobreza no depende del sexo de las personas, puesto que que el valor p (0.8126) es mayor que el nivel de significación (0.05). En este mismo sentido, si se desea saber si el desempleo está relacionado con el sexo, se realiza la prueba de hipótesis como sigue:


```r
svychisq(
  formula = ~ Sex + Employment,
  design = diseno,
  statistic = "F"
)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  NextMethod()
## F = 62.251, ndf = 1.6865, ddf = 200.6978, p-value < 2.2e-16
```
De la anterior salida, se puede concluir con una confianza del 95% y basado en las estimaciones sobre la muestra expandida que la desocupación depende del sexo de las personas, puesto que que el valor p (2.2e-16) no es mayor que el nivel de significación (0.05). Es decir, estas dos variables no son independientes. Si en el análisis ahora se quisiera verificar si la pobreza de las personas es independiente de las regiones establecidas en la base de datos, se realiza de la siguiente manera:


```r
svychisq(
  formula = ~ Region + pobreza,
  design = diseno,
  statistic = "F"
)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  NextMethod()
## F = 0.48794, ndf = 3.0082, ddf = 357.9731, p-value = 0.6914
```
Concluyendo que sí hay independencia entre la pobreza y la región. Lo anterior implica que, no existe relación entre las personas en estado de pobreza por región.

### Diferencia de proporciones y contrastes

Como lo mencionan @Heeringa_West_Berglund_2017, las estimaciones de las proporciones de las filas en las tablas de doble entrada son estimaciones de subpoblaciones en las que la subpoblación se define por los niveles de la variable categórica. En algunas ocasiones, puede ser de interés estimar diferencias de las proporciones de las categorías entre dos niveles o en dos subpoblaciones. Como ya se vio en el capítulo anterior, esto puede ser logrado utilizando contrastes. 

A manera de ejemplo, considere que se requiere estimar el contraste de proporciones de mujeres en estado de pobreza contra los hombres en esta misma condición  ($\hat{\Delta}_{\omega} = \hat{p}^{F1}_{\omega}-\hat{p}^{M1}_{\omega}$). Para ello, primero estimamos la proporción de hombres y mujeres en estado de pobreza como se ha mostrado en capítulos anteriores:


```r
(
  tab_sex_pobreza <- svyby(
    formula = ~ pobreza,
    by = ~ Sex,
    design = diseno ,
    svymean,
    na.rm = T,
    covmat = TRUE,
    vartype = c("se", "ci")
  )
)
```

```
##           Sex   pobreza         se      ci_l      ci_u
## Female Female 0.3892389 0.03159581 0.3273123 0.4511656
## Male     Male 0.3945612 0.03662762 0.3227724 0.4663501
```

Ahora bien, para calcular la estimación de la diferencia de proporciones junto con sus errores estándares, se realizarán los siguientes pasos:

-   *Paso 1:* Calcular la diferencia de estimaciones 

```r
0.3892 - 0.3946 			 
```

```
## [1] -0.0054
```

-   *Paso 2:* Con la función `vcov` obtener la matriz de covarianzas:

```r
vcov(tab_sex_pobreza)
```

```
##              Female         Male
## Female 0.0009982953 0.0009182927
## Male   0.0009182927 0.0013415823
```

-   *Paso 3:* Calcular el error estándar es:   

```r
sqrt(0.0009983 + 0.0013416 - 2*0.0009183)
```

```
## [1] 0.02243435
```


Ahora bien, aplicando la función `svycontrast` se puede obtener la estimación de la diferencia de proporciones anterior de manera más expedita: 


```r
svycontrast(tab_sex_pobreza,
            list(diff_Sex = c(1,-1))) 
```

```
##            contrast     SE
## diff_Sex -0.0053223 0.0224
```

De lo que se concluye que, la diferencia entre las proporciones estimadas de mujeres y hombres en condición de pobreza es -0.005 (-0.5%) con una error estándar estimado de 0.022.

Otro ejercicio de interés en un análisis de encuestas de hogares es verificar la si existen brechas en la condición de ocupación (por ejemplo en el desempleo) por sexo. Al igual que el ejemplo anterior, se inicia con la estimación del porcentaje de desempleados por sexo, omitiendo las personas menores de edad:


```r
tab_sex_desempleo <- svyby(
  formula = ~ desempleo,
  by = ~ Sex,
  design  = diseno %>% filter(!is.na(desempleo)) ,
  FUN     = svymean,
  na.rm = T,
  covmat = TRUE,
  vartype = c("se", "ci")
)
tab_sex_desempleo
```

```
##           Sex  desempleo          se       ci_l       ci_u
## Female Female 0.02168620 0.005580042 0.01074952 0.03262288
## Male     Male 0.06782601 0.012161141 0.04399062 0.09166141
```

Para calcular la estimación de la diferencia de proporciones junto con sus errores estándares, se realizarán los siguientes pasos:

-   *Paso 1*: Diferencia de las estimaciones 

```r
0.02169 - 0.06783 	
```

```
## [1] -0.04614
```

-   *Paso 2:* Con la función `vcov` obtener la matriz de covarianzas:


```r
vcov(tab_sex_desempleo)
```

```
##              Female         Male
## Female 3.113687e-05 2.081301e-05
## Male   2.081301e-05 1.478933e-04
```

-   *Paso 3*: Estimación del error estándar. 

```r
sqrt(0.00003114	 + 0.00014789 - 2*0.00002081)
```

```
## [1] 0.0117222
```

Siguiendo el ejemplo anterior, utilizando la función `svycontrast` se tiene que:


```r
svycontrast(tab_sex_desempleo,
            list(diff_Sex = c(-1, 1))) 
```

```
##          contrast     SE
## diff_Sex  0.04614 0.0117
```

De los resultados anteriores, se concluye que la estimación del contraste es 0.04 (4%) con un error estándar estimado de 0.011.

Adentrándose un poco más en la complejidad de los contrates, otro ejercicio que se puede realizar en una encuesta de hogares es estimar la proporción de desempleados por región. Para la realización de este ejercicio, se seguirán los pasos de los dos ejemplos anteriores:


```r
tab_region_desempleo <- svyby(
  formula =  ~ desempleo,
  by = ~ Region,
  design  = diseno %>% filter(!is.na(desempleo)) ,
  FUN     = svymean,
  na.rm = T,
  covmat = TRUE,
  vartype = c("se", "ci")
)

tab_region_desempleo
```

```
##              Region  desempleo         se        ci_l       ci_u
## Norte         Norte 0.04877722 0.02002293 0.009532997 0.08802144
## Sur             Sur 0.06563877 0.02375124 0.019087202 0.11219034
## Centro       Centro 0.03873259 0.01240317 0.014422832 0.06304235
## Occidente Occidente 0.03996523 0.01229650 0.015864529 0.06406592
## Oriente     Oriente 0.02950231 0.01256905 0.004867428 0.05413719
```

Ahora, asuma que el interés es realizar contrastes para la proporción de desempleados entre las regiones Norte y Sur, entre Sur y Centro y, finalmente, entre Occidente y Oriente. A continuación se tendrían las estimaciones puntuales: 

- $\hat{p}_{Norte} - \hat{p}_{Centro} = 0.04877722 - 0.03873259 = -0.01004463$, 
- $\hat{p}_{Sur} - \hat{p}_{Centro} = 0.06563877 - 0.03873259 = 0.02690618$ 	
- $\hat{p}_{Occidente} - \hat{p}_{Oriente} = 0.03996523 - 0.02950231 = 0.01046292$	

Asimismo, escrita de forma matricial, la matriz de contraste sería: 

$$
A =
\left[\begin{array}{ccccc}
1 & 0 & -1 & 0 & 0\\
0 & 1 & -1 & 0 & 0\\
0 & 0 & 0 & 1 & -1
\end{array}\right]
$$

La matriz de varianzas y covarianzas de las proporciones estimadas es:


```r
vcov(tab_region_desempleo)
```

Por tanto, la varianza estimada para cada diferencia de proporciones está dada por:


```r
sqrt(0.0004009178 + 0.0001538386 - 2 * 0)
```

```
## [1] 0.02355327
```

```r
sqrt(0.0005641213 + 0.0001538386 - 2 * 0)
```

```
## [1] 0.02679477
```

```r
sqrt(0.0001512039 + 0.000157981 - 2 * 0)
```

```
## [1] 0.01758365
```


Usando la función `svycontrast`, la estimación de los contrastes sería:


```r
svycontrast(tab_region_desempleo,
            list(
              Norte_sur = c(1, 0, -1, 0, 0),
              Sur_centro = c(0, 1, -1, 0, 0),
              Occidente_Oriente = c(0, 0, 0, 1, -1)
            )) 
```

```
##                   contrast     SE
## Norte_sur         0.010045 0.0236
## Sur_centro        0.026906 0.0268
## Occidente_Oriente 0.010463 0.0176
```

### Razones de odds

Como lo menciona @monroy2018analisis, la traducción más aproximada del término odds es “ventaja” que denota la posibilidad de que un evento ocurra con relación a que no ocurra; es decir, es un número que expresa cuánto más probable es que se produzca un evento frente a que no se produzca. También se puede utilizar para cuantificar la asociación entre los niveles de una variable y un factor categórico. Por ejemplo, considere la siguiente salida que relaciona el sexo con la pobreza:


```r
tab_Sex_Pobr <-
  svymean(
    x = ~ interaction (Sex, pobreza),
    design = diseno,
    se = T,
    na.rm = T,
    ci = T,
    keep.vars = T
  )

tab_Sex_Pobr 
```

```
##                                      mean     SE
## interaction(Sex, pobreza)Female.0 0.32187 0.0178
## interaction(Sex, pobreza)Male.0   0.28637 0.0177
## interaction(Sex, pobreza)Female.1 0.20513 0.0166
## interaction(Sex, pobreza)Male.1   0.18663 0.0178
```
Las ODDS de ser mujer dado que es pobre son $ODDS(Sexo = Mujer|Pobre) = \hat{p}^{1|Female}_{\omega} / (1 - \hat{p}^{1|Female}_{\omega}) = \hat{p}^{1|Female}_{\omega} / \hat{p}^{0|Female}_{\omega} = 0.20/0.32=0.63$. Por otro lado, las ODDS de ser hombre dado que es pobre son $ODDS(Sexo = Hombre|Pobre) = \hat{p}^{1|Male}_{\omega} /  \hat{p}^{0|Male}_{\omega}=0.18/0.28=0.65$. De esta forma, la razón de odds estaría dada por la siguiente expresión. 

$$
\widehat{OR}_{\omega}^{Sexo-Pobreza} =  \frac{ODDS(Sexo = Mujer|Pobre)}{ODDS(Sex = Hombre|Pobre)} = 
\frac{\frac{P(Sex = Female \mid pobreza = 1 )}{P(Sex = Female \mid pobreza = 0 )}}{
 \frac{P(Sex = Male \mid pobreza = 1 )}{P(Sex = Male \mid pobreza = 0 )}
 } 
= \frac{0.63}{0.65}=0.97
$$

El procedimiento para realizarlo en `R`, luego de haber estimado las respectivas proporciones de la tabla cruzada entre las variables sexo y pobreza, se centra en realizar el contraste dividiendo cada uno de los elementos de la expresión mostrada anteriormente:



```r
svycontrast(stat = tab_Sex_Pobr,
            contrasts =
              quote((
                `interaction(Sex, pobreza)Female.1` /
                  `interaction(Sex, pobreza)Female.0`
              ) /
                (
                  `interaction(Sex, pobreza)Male.1` /
                    `interaction(Sex, pobreza)Male.0`
                )
              ))
```

```
##            nlcon     SE
## contrast 0.97791 0.0919
```

Del anterior resultado se estima que el odds de las mujeres que están en condición de pobreza es 0.97 (muy cercano a uno) comparándolo con el odds de los hombres que están en condición de pobreza. En otras palabras, se estima que las probabilidades de que las mujeres no estén en estado de pobreza sin tener en cuenta ninguna otra variable de la encuesta es cerca de 3% mayor que las probabilidades de los hombres.

