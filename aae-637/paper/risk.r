### RISK




stacked.firm.df$yield <- stacked.firm.df$x19.produccion.obtenidad.kg /
  stacked.firm.df$x19.superficie.cultivada.hectareas

targ.risk.crop <- "Fava Beans"
#targ.risk.crop <- "Maize"
# [1] "Potatoes"   "Maize"      "Barley"     "Wheat"      "Fava Beans"


var(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg>0])
var(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg==0])

IQR(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg>0])
IQR(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg==0])


summary(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg>0])
summary(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg==0])



IQR(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg>0 & stacked.firm.df$x19.uso.riego=="Si"])
IQR(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg>0 & stacked.firm.df$x19.uso.riego=="No"])

IQR(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg==0 & stacked.firm.df$x19.uso.riego=="Si"])
IQR(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg==0 & stacked.firm.df$x19.uso.riego=="No"])


summary(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg>0])
summary(stacked.firm.df$yield[stacked.firm.df$which.crop==targ.risk.crop & stacked.firm.df$x19.fertilizante.cantidad.kg==0])



& stacked.firm.df$x19.uso.riego


stacked.firm.df[stacked.firm.df$which.crop==targ.risk.crop, ]

x19.produccion.obtenidad.kg 
x19.superficie.cultivada.hectareas
x19.fertilizante.cantidad.kg
$x19.uso.riego


