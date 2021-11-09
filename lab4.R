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


  