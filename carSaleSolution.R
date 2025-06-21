#****************
#* Proyecto de Predicción de Ventas de Vehículos con lenguaje R y python
#* se cargara la data por medio de python las descripcion de la informacion
#* R se uso para la parte visual
#* Se seleccionó un conjunto de datos "kaggle" el cual contiene información de la venta de vehículos, contando con columnas como:
#*  Data - fecha de la venta
#*  Price - precio del vehículo
#*  Model, Company - Modelo y Marca de Vehículo
#* Y como datos del comprador los valores en las columnas:
#*  Annual_Income - Ingresos Anuales
#*  Gender - Género
#*
#* El conjunto de Datos tiene una tendencia muy marcada en el mercado de las cuales se repiten en ambos géneros
#* las preferencias por las categorías SUV y Hatchback, causa a su vez que se tenga un promedio en precio de vehículos comprados
#* bastante similar.
#* Podremos observar las regiones como se distribuyen los montos segun genero
#* observamos la tendencia de gusto de marcas por genero 
#* Precios promedios por marca 
#* Los meses donde historicamente cada genero a gastado mas y poder leer las tendencia
#* los graficos nos aportaran de forma visular la distribucion de la informacion

# Inicio de carga par auso de python en R
# Descomentar linea siguiente si no se cuenta con reticulate: 
#install.packages("reticulate", dependencies = TRUE)
#/*********
# Llamado para el uso de la libreria reticulate
library(reticulate)
#primera vez que usas reticulate para crear entornos
#py_install("virtualenv", pip = TRUE)
#/**********

tryCatch({
  origen_C<-"C:/Users/Fredy/RandP" #Definir la ruta base donde se usara la solucion
  new_env_path <- paste0(origen_C,"/CarSaleSolution") #CarSaleSolution ser la carpeta de origen para el entorno Python
  
  #Creacion de entorno virtual
  virtualenv_create(envname = new_env_path)
  #Verificacion de entorno, comentar si es necesario :
  message(paste("¡Nuevo entorno virtual creado con éxito en:", new_env_path, "!"))
  
  #/**************
  #******
  #* Por error en el entorno no detectado probar con la siguiente linea o reiniciar RStudio
  #* El error es: failed to initialize requested version of Python
  #python_path <- file.path(new_env_path, "Scripts", "python.exe")
  #/*******************
  
  #Se indica el entorno virtual a usar
  use_virtualenv(virtualenv = new_env_path, required = TRUE)
  message(paste("Reticulate está usando ahora el entorno:", py_config()$python))
  
  
  # Verificacion de Python
  py_config()
}, error = function(e) {
  message("Error al inicializar el entorno Python:")
  message(e$message)
  message("Por favor, asegúrate de que la ruta al entorno virtual sea correcta y que esté funcional.")
  stop("El script se detiene debido a un error de configuración de Python.")
})

#******
#*Inicio de solucion para carSale
#*Pasos ateoriores solo se ejecutan una vez
#******

# Intalamos paquetes necesraios:
py_install("pandas")

ruta_csv<-"C:/Users/Fredy/RandP/resources/CarSales-car_data.csv" #asignar ruta segun corresponda
#podemos validar si el archivo existe
file.exists(ruta_csv)


# Define el código Python directamente en R
#cargamos csv con python para gestionar mejor el origen y la data
#de la carpeta origen crear la carpeta resources y dentro el archivo .cs o brindar la ruta correspondiente
py_run_string(sprintf("
import pandas as pd
df = pd.read_csv('%s')
print('*'*75)
print(df.head())
print('*'*75)
print(f'Filas y Columnas {df.shape}') #obtenemos numero de filas y columnas
print('*'*75)
print(df.columns)#todas las columnas de nuestro df
print('*'*75)
print(df.dtypes)# los tipos de datos
print('*'*75)
#procedemos con la verificacion del df
print(df.isnull().sum())
print('*'*75)
#Verificacion de Datos duplicados
#para mantener integridad del df original generamos uno nuevo para eliminar la informacion duplicada
df1=df.copy()
duplicados=df1.duplicated().sum()
print('*'*75)
print(f'Cantidad de duplicados: {duplicados}')
print('*'*75)
if duplicados>0 :
    df1 = df1.drop_duplicates()
",ruta_csv))#pasamos la ruta como parametro

#para mantener el orden y no perder los pasos de python
#procedemos a iniciar un nuevo py_run para el tratamiento de los datos
py_run_string("
#cambio de tipos de datos
df1['Phone'] = df1['Phone'].astype('category')
df1['Customer_Name'] = df1['Customer_Name'].astype('category')
df1['Gender'] = df1['Gender'].astype('category')
df1['Engine'] = df1['Engine'].astype('category')
df1['Dealer_Name'] = df1['Dealer_Name'].astype('category')
df1['Company'] = df1['Company'].astype('category')
df1['Model'] = df1['Model'].astype('category')
df1['Transmission'] = df1['Transmission'].astype('category')
df1['Color'] = df1['Color'].astype('category')
df1['Body_Style'] = df1['Body_Style'].astype('category')
df1['Dealer_Region'] = df1['Dealer_Region'].astype('category')

df1['Car_id'] = df1['Car_id'].astype('category')
df1['Date'] = pd.to_datetime(df1['Date'])
df1['Dealer_No'] = df1['Dealer_No'].astype('category')
print(df1.dtypes)
#agregagos una columna mas que contiene solo mes y Año
df1['Mes_year'] = df1['Date'].dt.to_period('M')
df1['year'] = df1['Date'].dt.to_period('Y')
#creamos agrupaciones que utilizaremos para la parte visual
conteo_Body_Style = df1['Body_Style'].value_counts()
conteo_Gender = df1['Gender'].value_counts()
conteo_Company = df1['Company'].value_counts()
#suma_ventas = df1.groupby('Company')['Model'].count()
ingresos_por_genero = df1.groupby('Gender')['Price'].sum()
promedio_ingresos_por_genero = df1.groupby('Gender')['Price'].mean()

#Agrupacion de ventas por Año de cada sucursal

agrupado_Y_Dealer_Name_monto = df1.groupby(['year', 'Dealer_Name'])['Price'].sum().unstack()
#Promedio de preferencias por genero
promedio_fem = df1[df1['Gender']=='Female']['Body_Style'].value_counts(normalize=True)
promedio_mas = df1[df1['Gender']=='Male']['Body_Style'].value_counts(normalize=True)
")

#Parte visual con R
#importamos librerias necesarias
library(caret)
library(dplyr)
library(lubridate)
library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)
options(scipen = 999) 

#pruba de data no es necesirio ejecutar el head
df1_r<-py$df1

# Grafico de distribucion de conteo de ventas por categorias

body_style_count <- as.data.frame(py$conteo_Body_Style)
body_style_count$Body_Style <- rownames(body_style_count)
colnames(body_style_count) <- c("Count", "Body_Style")
body_style_count <- body_style_count[, c("Body_Style", "Count")]

#Grafico de barras
ggplot(body_style_count, aes(x = reorder(Body_Style, -Count), y = Count)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(x = "Estilo de carrocería", y = "Cantidad", title = "Distribución por Body Style")

#grafico de pastel
pie_chart <- ggplot(body_style_count, aes(x = '', y = Count, fill = Body_Style)) + 
  geom_bar(width = 1, stat = 'identity') + 
  coord_polar('y') + 
  labs(title = 'Conteo por Body Style Vendidos') + 
  theme_void()
# Mostrar el gráfico
print(pie_chart)

#/***** Grafica de conteo de ventas por marca ****/
sales_by_brand <- as.data.frame(py$conteo_Company)
sales_by_brand$Company <- rownames(sales_by_brand)
colnames(sales_by_brand) <- c("Count", "Company")
sales_by_brand <- sales_by_brand[, c("Company", "Count")] 
#ya estan creados y ordenados los datos a usar

plot <- ggplot(sales_by_brand, aes(x = Company, y = Count)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  theme_minimal() + 
  labs(title = 'Cantidad de Ventas por Marca', x = 'Marca', y = 'Cantidad de Ventas') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot)

# Crear gráfico que expresa la cantidad de ventas segun el genero del comprador
conteo_gender <- as.data.frame(py$conteo_Gender)
conteo_gender$Gender <- rownames(conteo_gender)
colnames(conteo_gender) <- c("Count", "Gender")
conteo_gender <- conteo_gender[, c("Gender", "Count")]

ggplot(conteo_gender, aes(x = Gender, y = Count, fill = Gender)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Balance de Ventas por Genero", x = "Género", y = "Cantidad")

#Ingresos totales por genero y Promedio por genero
ingresos_genero <- as.data.frame(py$ingresos_por_genero)
ingresos_genero$Gender <- rownames(ingresos_genero)
colnames(ingresos_genero) <- c("Total", "Gender")
ingresos_genero <- ingresos_genero[, c("Gender", "Total")]

ggplot(ingresos_genero, aes(x = Gender, y = Total, fill = Gender)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Ingreso total por Género", x = "Género", y = "Ingreso Anual")

promedio_ingresos_genero <- as.data.frame(py$promedio_ingresos_por_genero)
promedio_ingresos_genero$Gender <- rownames(promedio_ingresos_genero)
colnames(promedio_ingresos_genero) <- c("Total", "Gender")
promedio_ingresos_genero <- promedio_ingresos_genero[, c("Gender", "Total")]

ggplot(promedio_ingresos_genero, aes(x = Gender, y = Total, fill = Gender)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Ingreso total por Género", x = "Género", y = "Ingreso Anual")
#******
#*
#*
#*/

df1_r$Date <- as.Date(df1_r$Date,format = '%m/%d/%Y')
df1_r <- df1_r %>%
  mutate(Mes_year = floor_date(as.Date(Date), "month"))
# Obtener géneros únicos
generos <- unique(df1_r$Gender)

# Loop por género
for (genero in generos) {
  
  df_genero <- df1_r %>%
    filter(Gender == genero) %>%
    group_by(Gender, Mes_year) %>%
    summarise(Precio_Promedio = mean(Price, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(df_genero, aes(x = Mes_year, y = Precio_Promedio)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 2) +
    labs(title = "Precio Promedio de Vehículos por Género y Mes",
         subtitle = paste("Género:", genero),
         x = "Mes y Año",
         y = "Precio Promedio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)  # <- Esto asegura que se muestre el gráfico dentro del loop
}

#/****** boxplot *****/

ggplot(df1_r, aes(x = factor(1), y = Annual_Income)) +
  geom_boxplot(fill = "salmon", color = "grey") +
  labs(title = "Boxplot Ingreso Anual", x = "", y = "Ingreso anual") +
  coord_flip() +
  theme_minimal()

ggplot(df1_r, aes(x = factor(1), y = Price)) +
  geom_boxplot(fill = "salmon", color = "grey") +
  labs(title = "Boxplot Precio", x = "", y = "Precio") +
  coord_flip() +
  theme_minimal()

#/**** Aplicacion de Logaritmo Natural ******
df1_r$Annual_Income_ln <- log(df1_r$Annual_Income)
df1_r$Price_ln <- log(df1_r$Price)

ggplot(df1_r, aes(x = factor(1), y = Annual_Income_ln)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot Ingreso Anual", x = "", y = "Ingreso anual") +
  coord_flip() +
  theme_minimal()

ggplot(df1_r, aes(x = factor(1), y = Price_ln)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot Precio", x = "", y = "Precio") +
  coord_flip() +
  theme_minimal()

#frecuencia de typo de vehiculo y genero
ggplot(df1_r, aes(x = Gender, fill = Body_Style)) +
  geom_bar(position = "dodge") +
  theme_minimal()



# Crear el gráfico de líneas usamos df generados y agrupados en python
fem_body <- as.data.frame(py$promedio_fem)
fem_body$Body_Style <- rownames(fem_body)
colnames(fem_body) <- c("Proporcion", "Body_Style")
fem_body <- fem_body[, c("Body_Style", "Proporcion")]
mas_body <- as.data.frame(py$promedio_mas)
mas_body$Body_Style <- rownames(mas_body)
colnames(mas_body) <- c("Proporcion", "Body_Style")
mas_body <- mas_body[, c("Body_Style", "Proporcion")]

ggplot() +
  geom_line(data = fem_body, aes(x = Body_Style, y = Proporcion, group = 1), 
            color = "pink", linewidth = 1.2) +
  geom_point(data = fem_body, aes(x = Body_Style, y = Proporcion), color = "pink", size = 3) +
  geom_line(data = mas_body, aes(x = Body_Style, y = Proporcion, group = 1), 
            color = "blue", linewidth = 1.2) +
  geom_point(data = mas_body, aes(x = Body_Style, y = Proporcion), color = "blue", size = 3) +
  labs(title = "Promedio de Preferencias por Género",
       x = "Body Style",
       y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje X
  scale_x_discrete(limits = sort(unique(df1_r$Body_Style))) +  # Ordenar categorías en el eje X
  geom_text(data = fem_body, aes(x = Body_Style, y = Proporcion, label = scales::percent(Proporcion)), vjust = -0.5, color = "pink") +
  geom_text(data = mas_body, aes(x = Body_Style, y = Proporcion, label = scales::percent(Proporcion)), vjust = -0.5, color = "blue")

#grafica de precio promedio por marca
df1_r %>%
  group_by(Company) %>%
  summarise(Precio_Med = mean(Price, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Company, -Precio_Med), y = Precio_Med)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #se roto el texto del eje x para poder ser visualizado
  labs(title = "Precio promedio por marca",
       x = "Marca",
       y = "Precio Promedio")

#grafico de precio promedio de cada marcar agrupandose por genero
df_burbujas <- df1_r %>%
  group_by(Company, Gender) %>%
  summarise(
    Precio_Medio = mean(Price, na.rm = TRUE),
    Ingreso_Medio = mean(Annual_Income, na.rm = TRUE),
    .groups = "drop"
  )

df_burbujas2 <- df1_r %>%
  group_by(Dealer_Region, Gender) %>%
  summarise(
    Precio_Medio = mean(Price, na.rm = TRUE),
    Ingreso_Medio = mean(Annual_Income, na.rm = TRUE),
    .groups = "drop"
  )

#Por fabricante de vehiculo
ggplot(df_burbujas, aes(x = Company, y = Ingreso_Medio, size = Precio_Medio, fill = Gender)) +
  geom_point(alpha = 0.7, shape = 21, color = "black") +
  scale_size_continuous(name = "Precio Promedio") +
  theme_minimal() +
  labs(title = "Gráfico de Burbujas: Ingreso Medio vs. Marca",
       y = "Ingreso Anual Promedio", x = "Marca") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Por dealer_region para ver las regiones que pueden gastar mas
ggplot(df_burbujas2, aes(x = Dealer_Region, y = Ingreso_Medio, size = Precio_Medio, fill = Gender)) +
  geom_point(alpha = 0.7, shape = 21, color = "black") +
  scale_size_continuous(name = "Precio Promedio") +
  theme_minimal() +
  labs(title = "Gráfico de Burbujas: Ingreso Medio vs. Region",
       y = "Ingreso Anual Promedio", x = "Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Pair Plot
ggpairs(df1_r, 
        columns = c("Annual_Income", "Price"),  # Solo columnas numéricas
        aes(color = Gender)) 

