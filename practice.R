install.packages("RODBC")

library(RODBC)
library(ggplot2)
library(dplyr)

# Conexion sqlserver

servidor<-Sys.getenv("SERVIDOR_DSN")
usuario<-Sys.getenv("USUARIO_DB")
clave<-Sys.getenv("CONTRASENA_DB")

con<-odbcConnect(servidor,uid = usuario,pwd = clave)

invoices<-sqlQuery(con,"select *from invoices")

View(invoices)

glimpse(invoices)

class(invoices)
# Analizar los nulos
summary(invoices)
head(invoices,2)
is.na(invoices)
sum(is.na(invoices))
which(is.na(invoices))
invoices$total<-invoices$Quantity

apply(is.na(invoices),MARGIN=2, FUN = sum)

newdata<-na.omit(invoices)
apply(is.na(newdata),MARGIN=2, FUN = sum)

rm(newdata)

invoices[,c(1,3)]

glimpse(invoices)

invoices$OrderDate<-as.Date(invoices$OrderDate)
invoices$RequiredDate<-as.Date(invoices$RequiredDate)

invoices$total<-invoices$Quantity*invoices$UnitPrice
names(invoices)
View(invoices)

invoices<-mutate(invoices,month=as.numeric(format(OrderDate,'%m')))

invoices<-mutate(invoices,year=as.numeric(format(OrderDate,'%m')))
plot(invoices$total)

total_year<- invoices %>% group_by(year) %>% 
  summarise(totals=sum(total))

View(total_year)
head(total_year)

colnames(total_year)<-c("anio","total")

rownames(total_year)<-total_year$anio

invoices1997<-invoices %>% filter(year==1997)
invoices1998<-invoices %>% filter(year %in%  c(1997,1998)) #&&


invoices_country<-invoices %>%  group_by(Country) %>% 
  summarise(totals=sum(total))
invoices_country

hist(invoices$total,col = 'lightblue',main = "Histograma de las ventas")
hist(invoices_country$totals,col = 'lightblue',main = "Histograma de las ventas")

datos<-iris
hist(datos$Sepal.Width,col = 'lightblue',main = "Histograma de las ventas")

invoices_graf<-invoices[,c("year","total")]
ggplot(data = invoices_graf)+
  aes(x=total,y=density..)+
  geom_histogram(bins = 15,color="Blue",fill="lightblue")+
  geom_density()
