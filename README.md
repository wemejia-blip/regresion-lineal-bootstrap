# Análisis de Bootstrap y Mínimos Cuadrados Ordinarios en Regresión Lineal Simple

Scripts en R que compara y análiza los resultados MCO vs Bootstrap por pares, y exploran empiricamente la estabilidad asintótica del Bootstrap (Bickel & Freedman, 1981).

## Contenido

[📄 1_simulaciones.R](simulaciones.R)
- 1.1 Simulación con supuestos clásicos cumplidos (n=50, B=1000)
- 1.2 Simulación con heterocedasticidad (n=50, B=1000)

[📄 2_prueba_supuestos.R](Prueba_de_supuestos_clasicos.R)
- 2.1 Exploración Auto MPG (mpg vs weight)
- 2.2 Caso 1: log(mpg) ~ weight — Shapiro-Wilk y Breusch-Pagan
- 2.3 Caso 2: mpg ~ log(weight) — Shapiro-Wilk y Breusch-Pagan

 [📄 3_bootstrap_auto_mpg.R](Aplicacion_boostrap.R)
- 3.1 Bootstrap Caso 1: log(mpg) ~ weight
- 3.2 Bootstrap Caso 2: mpg ~ log(weight)
- 3.3 Evaluación predictiva (partición 70/30: RMSE, MAE, R²)

[📄 4_estabilidad_asintotica.R.R](Estabilidad_asintótica.R)
- 4.1 Distancia KS — población simulada homocedástica
- 4.2 Distancia KS — población simulada heterocedástica
- 4.3 Distancia KS — datos reales Auto MPG (Caso 1 y Caso 2)


  ## Datos

[auto-mpg.csv](auto-mpg.csv) — dataset Auto MPG (~398 vehículos). Variables usadas:

- `mpg`: millas recorridas por galón de combustible.
- `weight`: peso vehicular en libras.

Fuente original: [UCI Machine Learning Repository — Auto MPG](https://archive.ics.uci.edu/dataset/9/auto+mpg)(Quinlan, 1993).

## Ejecución

```r
install.packages("lmtest")
```

Colocar `[auto-mpg.csv](auto-mpg.csv) en el directorio de trabajo y correr los scripts en el orden listado arriba.

## Requisitos

- R (≥ 4.0)
- Paquete `lmtest`
- Funciones base: `lm()`, `sample()`, `quantile()`, `shapiro.test()`


## Informe completo

El análisis detallado, gráficos y discusión de resultados se encuentran en `proyecto_final_seminario.pdf`.







