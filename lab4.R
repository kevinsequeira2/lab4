library(RODBC)
library(ggplot2)
library(dplyr)

# Conexion sqlserver

servidor<-Sys.getenv("SERVIDOR_DSN")
usuario<-Sys.getenv("USUARIO_DB")
clave<-Sys.getenv("CONTRASENA_DB")

con<-odbcConnect(servidor,uid = usuario,pwd = clave)

# Use las funciones odbcConnect y sqlQuery e importe las tablas (cliente, pedido y 
  detalle_pedido) para realizar un análisis exploratorio.

cliente<-sqlQuery(con,"select *from customers")
pedido<-sqlQuery(con,"select *from orders")
detalle_pedido<-sqlQuery(con,"select *from [Order Details]")


View(cliente)
View(pedido)
View(detalle_pedido)
# unir detalle_pedido y pedido
pedidos<-merge(x = pedido, y = detalle_pedido, by = c("OrderID"))
View(pedidos)
# unir pedidos con clientes
cliente_pedido<-merge(x = cliente, y = pedidos, by = c("CustomerID"))
View(cliente_pedido)

# Exploracion
names(detalle_pedido)
glimpse(cliente)
str(pedido)
head(detalle_pedido)
summary(cliente_pedido)

# conversiones
as.factor(cliente_pedido<-c(0))
meses<-mutate(cliente_pedido,ShippedDate=as.numeric(format(OrderDate,'%m')))
cantidad<-as.integer(cliente_pedido[,"Quantity"])
precio<-as.double(cliente_pedido[,"UnitPrice"])


datos_cadena <- table(cliente_pedido<-c(""))

# Cree un conjunto de datos nuevo, con las columnas (nombre_cliente,ciudad, región,país,fecha_pedido,estado, cantidad, precio_unidad,código_producto)


fill_clientes<-sqlQuery(con,"select c.ContactName as nombre_cliente,c.City as ciudad,c.Region as region,
c.Country as Pais,od.Discount as estado,od.Quantity as cantidad,od.UnitPrice as precio_unidad
, p.ProductID as codigo_producto from Customers c
INNER JOIN Orders o on o.CustomerID = c.CustomerID
INNER JOIN [Order Details] od on od.OrderID = o.OrderID
INNER JOIN Products p on p.ProductID = od.ProductID")

View(fill_clientes)
#  En el nuevo conjunto de datos genere una nueva columna de nombre total_ventas la cual será el producto de multiplicar la cantidad * precio_unidad

fill_clientes$total_ventas <- fill_clientes$cantidad * 
  fill_clientes$precio_unidad

# Cree dos nuevas columnas y extraiga el mes y el año a partir de la fecha_pedido

fecha<-sqlQuery(con,"select o.RequiredDate as fecha from Customers c
INNER JOIN Orders o on o.CustomerID = c.CustomerID
INNER JOIN [Order Details] od on od.OrderID = o.OrderID
INNER JOIN Products p on p.ProductID = od.ProductID")

fill_clientes$mes <- as.numeric(format(fecha$fecha,'%m'))
fill_clientes$annio <- as.numeric(format(fecha$fecha,'%Y'))

# Determine en cuales columnas hay valores nulos o ausentes

apply(is.na(fill_clientes), 2, sum)

#   Utilice la función boxplot para determinar si existen valores atípicos en total_ventas.

boxplot(fill_clientes$total_ventas, horizontal = TRUE)

#  Utilice la función hist para determinar la distribución de los datos de total_ventas

hist(fill_clientes$total_ventas, main = "Histograma de distribución",
     ylab = "Distribución")
# Cree un nuevo juego de datos con nombre ventas_ciudad y agrupe las ventas_totales por ciudad
utilice la utilidad %>%
  
ventas_totales<-fill_clientes[,c(2,9)]
View(ventas_totales)
venta_ciudad <- ventas_totales %>% group_by(ciudad)
venta_ciudad 

# Cree un nuevo juego de datos con nombre ventas_anuales y agrupe por año y excluya los datos del 2006, utilice la utilidad %>% y la función filter

anuales <- fill_clientes[,c(9,11)]
anuales
ventas_anuales <- anuales %>%  filter( annio < 1998) %>% 
  filter(annio != 2006)
ventas_anuales

# Cree un nuevo juego de datos con nombre ventas_resumen y agrupe por pais, utilice la utilidad 
%>% y agregue la columnas ventas_totales y total_clientes, con la función summarize


ventas_resumen <- fill_clientes  %>% group_by(Pais) %>% 
                              summarise(ventas_totales=sum(fill_clientes$total_ventas)) 
                              
ventas_resumen
ventas_resumen <- fill_clientes  %>% group_by(Pais,codigo_producto) %>%
  summarise(ventas_totales=sum(fill_clientes$total_ventas)) 
ventas_resumen


ventas_ciudad_graf<-venta_ciudad[,c("ciudad","total_ventas")]
ggplot(ventas_ciudad = ventas_ciudad_graf)+geom_bar()+
  geom_histogram(bins = 15,color="Blue",fill="lightblue")


venta_ciudad
