
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$Energia <- renderPlot({

        Demanda.Meta.Datos <- approxfun( x = c(0.0,5,10,15,20,25,30,35,40), #times
                                         y = c(312*1.0,312*1.2,312*1.25,312*1.3,312*1.4,312*1.5,312*1.7,312*1.9,312*2.0), # [TWH], basado en analisis de Mariana y Sabrina
                                         method = "linear",
                                         rule = 2)
        
      Precio_Hidrocarburos_Datos <- approxfun( x = c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0), #times
                                               y = c(1.0,1.0,1.00,1.00,1.00,1.00,1.00,1.00,1.00), # [1], basado en analisis de Mariana y Sabrina
                                               method = "linear",
                                               rule = 2)
      incremento.publico<- (input$incremento.publico)
      incremento.privado<- (input$incremento.privado)
      impuesto.publico<- (input$impuesto.publico)
      impuesto.privado<- (input$impuesto.privado)
      incremento.fosil.publico <- (input$incremento.fosil.publico)
      parameters<-c(
        #NoFosil_privada
        Factor_de_plata_base_NoFosil_privada = 0.33*1.10 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector pÃºblico
        Costo_Desarrollo_Capacidad_NoFosil_privada = 1228, # [Millones MXN/GW instalado], este costo marginal es estimado en funcion del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida econÃ³mica y mismo tiempo de construcciÃ³n
        Inversion_base_NoFosil_privada = 10000*incremento.privado, # 500*20 #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html
        Vida_util_Capacidad_NoFosil_privada = 23, #[anos]
        #NoFosil_publica
        Factor_de_plata_base_NoFosil_publica = 0.33*0.9 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
        Costo_Desarrollo_Capacidad_NoFosil_publica =  2857, # [Millones MXN/GW instalado], este costo marginal es estimado en funcion del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida econÃ³mica y mismo tiempo de construcciÃ³n
        Inversion_base_NoFosil_publica = 2000*incremento.publico, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
        Vida_util_Capacidad_NoFosil_publica = 23, #[anos]
        #Fosil_privada
        Factor_de_plata_base_Fosil_privada = 0.47*1.19 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
        Costo_Desarrollo_Capacidad_Fosil_privada = 1739*impuesto.privado,  # [Millones MXN/GW instalado], este costo marginal es estimado en funcion del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida econÃ³mica y mismo tiempo de construcciÃ³n
        Inversion_base_Fosil_privada = 10000, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,
        Vida_util_Capacidad_Fosil_privada = 28 , #[anos]
        #Fosil_publica
        Factor_de_plata_base_Fosil_publica = 0.47*0.90 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
        Costo_Desarrollo_Capacidad_Fosil_publica = 5822*impuesto.publico , # [Millones MXN/GW instalado], este costo marginal es estimado en funcion del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida econÃ³mica y mismo tiempo de construcciÃ³n
        Inversion_base_Fosil_publica = 2000*incremento.fosil.publico, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
        Vida_util_Capacidad_Fosil_publica = 28 #[ano]
      )
      
      
      InitialConditions <- c(
        Capacidad_NoFosil_privada = 30*0.3 , # [GW] , asumimos que el sector privado tiene el 30% de la capacidad instalada
        Capacidad_Fosil_privada = 56 *0.3, # [GW] , asumimos que el sector pÃºblico tiene 70% de la capacidad instalada
        Capacidad_NoFosil_publica = 30*0.7 , # [GW]
        Capacidad_Fosil_publica = 56*0.7  #[GW]
      )
      
      
      #years
      times <- seq(0, #initial time
                   40, #end of simulation #[year]
                   1)#time step #[year] para verlo por mes.
      
      intg.method<-c("rk4")
      
      EnerMX <- function(t, state, parameters) {
        with(as.list(c(state,parameters)), {
          #Auxiliary variables
          Precio_Hidrocarburos<-Precio_Hidrocarburos_Datos(t)
          Efecto_Precio_Hidrocarburos_en_CNG_Fosil<-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                            c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=Precio_Hidrocarburos)$y
          
          #NoFosil_privada
          Generacion_NoFosil_privada <- Capacidad_NoFosil_privada*Factor_de_plata_base_NoFosil_privada*24*365 # [GWH]
          CNG_NoFosil_privada <- Costo_Desarrollo_Capacidad_NoFosil_privada*Capacidad_NoFosil_privada/Generacion_NoFosil_privada
          
          #NoFosil_publica
          Generacion_NoFosil_publica <- Capacidad_NoFosil_publica*Factor_de_plata_base_NoFosil_publica*24*365 # [GWH]
          CNG_NoFosil_publica <- Costo_Desarrollo_Capacidad_NoFosil_publica*Capacidad_NoFosil_publica/Generacion_NoFosil_publica
          
          #Fosil_privada
          Generacion_Fosil_privada <- Capacidad_Fosil_privada*Factor_de_plata_base_Fosil_privada*24*365 # [GWH]
          CNG_Fosil_privada <- Costo_Desarrollo_Capacidad_Fosil_privada*Capacidad_Fosil_privada*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_privada
          
          #Fosil_publica
          Generacion_Fosil_publica <- Capacidad_Fosil_publica*Factor_de_plata_base_Fosil_publica*24*365 # [GWH]
          CNG_Fosil_publica <- Costo_Desarrollo_Capacidad_Fosil_publica*Capacidad_Fosil_publica*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_publica
          
          # Generacion fosil y no fosil total
          Generacion_NoFosil_Total <- (Generacion_NoFosil_privada+Generacion_NoFosil_publica)/1000 #[TWH]
          
          Generacion_Fosil_Total <- (Generacion_Fosil_privada+Generacion_Fosil_publica)/ 1000 #[TWH]
          
          Generacion_privada_Total <- (Generacion_Fosil_privada+Generacion_NoFosil_privada)/1000 #[TWH]
          
          #Costo Promedio Sistema
          CNG_sistema <- mean(c(CNG_NoFosil_privada,CNG_NoFosil_publica,CNG_Fosil_privada,CNG_Fosil_publica))
          
          
          #Precios relativos
          #dentro de sectores
          CNG_relativo_NoFosil_privada_crd_Fosil_privada <- CNG_NoFosil_privada/CNG_Fosil_privada
          CNG_relativo_NoFosil_publica_crd_Fosil_publica <- CNG_Fosil_privada/CNG_Fosil_publica
          
          #entre sectores
          CNG_relativo_NoFosil_privada_crd_NoFosil_publica <- CNG_NoFosil_privada/CNG_NoFosil_publica
          CNG_relativo_Fosil_privada_crd_Fosil_publica <- CNG_NoFosil_publica/CNG_Fosil_publica
          
          
          #Efecto de precios relativos
          #dentro sectores
          Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                        c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
          Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                      c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
          Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                        c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
          Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                      c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
          #Entre sectores
          Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                         c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
          
          
          Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                         c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
          
          
          Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                    c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
          
          
          Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                   c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
          
          
          
          
          #Niveles de inversion
          #Nofosil_privada
          Inversion_NoFosil_privada <- Inversion_base_NoFosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada
          
          #Nofosil_publica
          Inversion_NoFosil_publica <- Inversion_base_NoFosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica
          
          #Fosil_privada
          Inversion_Fosil_privada <- Inversion_base_Fosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada
          
          #Fosil_publica
          Inversion_Fosil_publica <- Inversion_base_Fosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica
          
          
          
          #Flow Variables
          
          #Capacidad_NoFosil_privada
          Perdida_Anual_Capacidad_NoFosil_privada <-  Capacidad_NoFosil_privada/Vida_util_Capacidad_NoFosil_privada
          Incremento_Anual_Capacidad_NoFosil_privada <- Inversion_NoFosil_privada/Costo_Desarrollo_Capacidad_NoFosil_privada
          
          #Capacidad_Fosil_privada
          Perdida_Anual_Capacidad_Fosil_privada <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_privada
          Incremento_Anual_Capacidad_Fosil_privada <- Inversion_Fosil_privada/Costo_Desarrollo_Capacidad_Fosil_privada
          
          #Capacidad_NoFosil_publica
          Perdida_Anual_Capacidad_NoFosil_publica <-  Capacidad_NoFosil_publica/Vida_util_Capacidad_NoFosil_publica
          Incremento_Anual_Capacidad_NoFosil_publica <- Inversion_NoFosil_publica/Costo_Desarrollo_Capacidad_NoFosil_publica
          
          #Capacidad_Fosil_publica
          Perdida_Anual_Capacidad_Fosil_publica <-  Capacidad_Fosil_publica/Vida_util_Capacidad_Fosil_publica
          Incremento_Anual_Capacidad_Fosil_publica <- Inversion_Fosil_publica/Costo_Desarrollo_Capacidad_Fosil_publica
          
          
          #Variables de salida
          EmisionesCO2e <- (Generacion_Fosil_privada + Generacion_Fosil_publica)*0.001 # , 0.001 [MTCO2e/GWH] fuente: https://www.eia.gov/tools/faqs/faq.php?id=74&t=11
          Generacion_Total <- (Generacion_NoFosil_privada + Generacion_Fosil_privada+
                                 Generacion_NoFosil_publica + Generacion_Fosil_publica)/1000 # [TWH]
          Deficit_Generacion<- Demanda.Meta.Datos(t)-Generacion_Total
          Participacion_NoFosil <- Generacion_NoFosil_Total/Generacion_Total
          Participacion_Privada <- Generacion_privada_Total/Generacion_Total
          
          #State Variable
          dCapacidad_NoFosil_privada <- Incremento_Anual_Capacidad_NoFosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
          dCapacidad_Fosil_privada <- Incremento_Anual_Capacidad_Fosil_privada - Perdida_Anual_Capacidad_Fosil_privada
          dCapacidad_NoFosil_publica <- Incremento_Anual_Capacidad_NoFosil_publica - Perdida_Anual_Capacidad_NoFosil_publica
          dCapacidad_Fosil_publica <- Incremento_Anual_Capacidad_Fosil_publica - Perdida_Anual_Capacidad_Fosil_publica
          
          
          #Output model results
          list(c( #State Variables
            dCapacidad_NoFosil_privada,
            dCapacidad_Fosil_privada,
            dCapacidad_NoFosil_publica,
            dCapacidad_Fosil_publica
          ),Participacion_NoFosil=Participacion_NoFosil,
          EmisionesCO2e=EmisionesCO2e,
          Generacion_Fosil_Total=Generacion_Fosil_Total,
          Generacion_NoFosil_Total=Generacion_NoFosil_Total,
          Deficit_Generacion=Deficit_Generacion,
          Inversion_NoFosil_privada=Inversion_NoFosil_privada,
          Inversion_NoFosil_publica=Inversion_NoFosil_publica,
          Inversion_Fosil_privada=Inversion_Fosil_privada,
          Inversion_Fosil_publica=Inversion_Fosil_publica,
          Participacion_Privada=Participacion_Privada
          )
        })
      }
      
      
      #Simulate model
      
      out <- data.frame(ode(y = InitialConditions,
                 times = times,
                 func = EnerMX,
                 parms = parameters,
                 method =intg.method))
      
      p1<-ggplot(out,aes(x=time,y=Participacion_NoFosil))+geom_line()+xlab("Tiempo")+scale_y_continuous(name="Participación de energías limpias", limits=c(0.2, 0.55))+theme_bw()
      p3<-ggplot(out,aes(x=time,y=EmisionesCO2e))+geom_line()+xlab("Tiempo")+scale_y_continuous(name="Emisiones de CO2 equivalentes", limits=c(0, 1010))+theme_bw()
      p4<-ggplot(out,aes(x=time,y=Deficit_Generacion))+geom_line()+xlab("Tiempo")+scale_y_continuous(name="Déficit de generación energetica", limits=c(-1000, 5))+theme_bw()
      p2<-ggplot(out,aes(x=time,y=Participacion_Privada))+geom_line()+xlab("Tiempo")+scale_y_continuous(name="Participación del sectro privado", limits=c(0.25, 0.91))+theme_bw()
      grid.arrange(p1,p2,p3,p4,nrow=2)
      #plot(out)
      out.1 <- as.data.frame(out)
      output$Tabla <- renderDataTable(as.data.frame(out.1))
    })
output$Plot2 <- renderPlot({
      
      Demanda.Meta.Datos <- approxfun( x = c(0.0,5,10,15,20,25,30,35,40), #times
                                       y = c(312*1.0,312*1.2,312*1.25,312*1.3,312*1.4,312*1.5,312*1.7,312*1.9,312*2.0), # [TWH], basado en analisis de Mariana y Sabrina
                                       method = "linear",
                                       rule = 2)
      
      Precio_Hidrocarburos_Datos <- approxfun( x = c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0), #times
                                               y = c(1.0,1.0,1.00,1.00,1.00,1.00,1.00,1.00,1.00), # [1], basado en analisis de Mariana y Sabrina
                                               method = "linear",
                                               rule = 2)
      incremento.publico<- (1+input$incremento.publico)
      incremento.privado<- (1+input$incremento.privado)
      impuesto.publico<- (1+input$impuesto.publico)
      impuesto.privado<- (1+input$impuesto.privado)
      
      parameters<-c(
        #NoFosil_privada
        Factor_de_plata_base_NoFosil_privada = 0.33*1.10 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector pÃºblico
        Costo_Desarrollo_Capacidad_NoFosil_privada = 1228, # [Millones MXN/GW instalado], este costo marginal es estimado en funcion del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida econÃ³mica y mismo tiempo de construcciÃ³n
        Inversion_base_NoFosil_privada = 10000*incremento.privado, # 500*20 #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html
        Vida_util_Capacidad_NoFosil_privada = 23, #[anos]
        #NoFosil_publica
        Factor_de_plata_base_NoFosil_publica = 0.33*0.9 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
        Costo_Desarrollo_Capacidad_NoFosil_publica =  2857, # [Millones MXN/GW instalado], este costo marginal es estimado en funcion del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida econÃ³mica y mismo tiempo de construcciÃ³n
        Inversion_base_NoFosil_publica = 100*20*incremento.publico, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
        Vida_util_Capacidad_NoFosil_publica = 23, #[anos]
        #Fosil_privada
        Factor_de_plata_base_Fosil_privada = 0.47*1.19 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
        Costo_Desarrollo_Capacidad_Fosil_privada = 1739*impuesto.privado,  # [Millones MXN/GW instalado], este costo marginal es estimado en funcion del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida econÃ³mica y mismo tiempo de construcciÃ³n
        Inversion_base_Fosil_privada = 500*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,
        Vida_util_Capacidad_Fosil_privada = 28 , #[anos]
        #Fosil_publica
        Factor_de_plata_base_Fosil_publica = 0.47*0.90 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
        Costo_Desarrollo_Capacidad_Fosil_publica = 5822*impuesto.publico , # [Millones MXN/GW instalado], este costo marginal es estimado en funcion del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida econÃ³mica y mismo tiempo de construcciÃ³n
        Inversion_base_Fosil_publica = 100*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
        Vida_util_Capacidad_Fosil_publica = 28 #[ano]
      )
      
      
      InitialConditions <- c(
        Capacidad_NoFosil_privada = 30*0.3 , # [GW] , asumimos que el sector privado tiene el 30% de la capacidad instalada
        Capacidad_Fosil_privada = 56 *0.3, # [GW] , asumimos que el sector pÃºblico tiene 70% de la capacidad instalada
        Capacidad_NoFosil_publica = 30*0.7 , # [GW]
        Capacidad_Fosil_publica = 56*0.7  #[GW]
      )
      
      
      #years
      times <- seq(0, #initial time
                   40, #end of simulation #[year]
                   1)#time step #[year] para verlo por mes.
      
      intg.method<-c("rk4")
      
      EnerMX <- function(t, state, parameters) {
        with(as.list(c(state,parameters)), {
          #Auxiliary variables
          Precio_Hidrocarburos<-Precio_Hidrocarburos_Datos(t)
          Efecto_Precio_Hidrocarburos_en_CNG_Fosil<-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                            c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=Precio_Hidrocarburos)$y
          
          #NoFosil_privada
          Generacion_NoFosil_privada <- Capacidad_NoFosil_privada*Factor_de_plata_base_NoFosil_privada*24*365 # [GWH]
          CNG_NoFosil_privada <- Costo_Desarrollo_Capacidad_NoFosil_privada*Capacidad_NoFosil_privada/Generacion_NoFosil_privada
          
          #NoFosil_publica
          Generacion_NoFosil_publica <- Capacidad_NoFosil_publica*Factor_de_plata_base_NoFosil_publica*24*365 # [GWH]
          CNG_NoFosil_publica <- Costo_Desarrollo_Capacidad_NoFosil_publica*Capacidad_NoFosil_publica/Generacion_NoFosil_publica
          
          #Fosil_privada
          Generacion_Fosil_privada <- Capacidad_Fosil_privada*Factor_de_plata_base_Fosil_privada*24*365 # [GWH]
          CNG_Fosil_privada <- Costo_Desarrollo_Capacidad_Fosil_privada*Capacidad_Fosil_privada*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_privada
          
          #Fosil_publica
          Generacion_Fosil_publica <- Capacidad_Fosil_publica*Factor_de_plata_base_Fosil_publica*24*365 # [GWH]
          CNG_Fosil_publica <- Costo_Desarrollo_Capacidad_Fosil_publica*Capacidad_Fosil_publica*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_publica
          
          # Generacion fosil y no fosil total
          Generacion_NoFosil_Total <- (Generacion_NoFosil_privada+Generacion_NoFosil_publica)/1000 #[TWH]
          
          Generacion_Fosil_Total <- Generacion_Fosil_privada+Generacion_Fosil_publica/ 1000 #[TWH]
          
          
          #Costo Promedio Sistema
          CNG_sistema <- mean(c(CNG_NoFosil_privada,CNG_NoFosil_publica,CNG_Fosil_privada,CNG_Fosil_publica))
          
          
          #Precios relativos
          #dentro de sectores
          CNG_relativo_NoFosil_privada_crd_Fosil_privada <- CNG_NoFosil_privada/CNG_Fosil_privada
          CNG_relativo_NoFosil_publica_crd_Fosil_publica <- CNG_Fosil_privada/CNG_Fosil_publica
          
          #entre sectores
          CNG_relativo_NoFosil_privada_crd_NoFosil_publica <- CNG_NoFosil_privada/CNG_NoFosil_publica
          CNG_relativo_Fosil_privada_crd_Fosil_publica <- CNG_NoFosil_publica/CNG_Fosil_publica
          
          
          #Efecto de precios relativos
          #dentro sectores
          Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                        c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
          Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                      c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
          Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                        c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
          Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                      c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
          #Entre sectores
          Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                         c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
          
          
          Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                         c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
          
          
          Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                    c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
          
          
          Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                   c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
          
          
          
          
          #Niveles de inversion
          #Nofosil_privada
          Inversion_NoFosil_privada <- Inversion_base_NoFosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada
          
          #Nofosil_publica
          Inversion_NoFosil_publica <- Inversion_base_NoFosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica
          
          #Fosil_privada
          Inversion_Fosil_privada <- Inversion_base_Fosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada
          
          #Fosil_publica
          Inversion_Fosil_publica <- Inversion_base_Fosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica
          
          
          
          #Flow Variables
          
          #Capacidad_NoFosil_privada
          Perdida_Anual_Capacidad_NoFosil_privada <-  Capacidad_NoFosil_privada/Vida_util_Capacidad_NoFosil_privada
          Incremento_Anual_Capacidad_NoFosil_privada <- Inversion_NoFosil_privada/Costo_Desarrollo_Capacidad_NoFosil_privada
          
          #Capacidad_Fosil_privada
          Perdida_Anual_Capacidad_Fosil_privada <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_privada
          Incremento_Anual_Capacidad_Fosil_privada <- Inversion_Fosil_privada/Costo_Desarrollo_Capacidad_Fosil_privada
          
          #Capacidad_NoFosil_publica
          Perdida_Anual_Capacidad_NoFosil_publica <-  Capacidad_NoFosil_publica/Vida_util_Capacidad_NoFosil_publica
          Incremento_Anual_Capacidad_NoFosil_publica <- Inversion_NoFosil_publica/Costo_Desarrollo_Capacidad_NoFosil_publica
          
          #Capacidad_Fosil_publica
          Perdida_Anual_Capacidad_Fosil_publica <-  Capacidad_Fosil_publica/Vida_util_Capacidad_Fosil_publica
          Incremento_Anual_Capacidad_Fosil_publica <- Inversion_Fosil_publica/Costo_Desarrollo_Capacidad_Fosil_publica
          
          
          #Variables de salida
          EmisionesCO2e <- (Generacion_Fosil_privada + Generacion_Fosil_publica)*0.001 # , 0.001 [MTCO2e/GWH] fuente: https://www.eia.gov/tools/faqs/faq.php?id=74&t=11
          Generacion_Total <- (Generacion_NoFosil_privada + Generacion_Fosil_privada+
                                 Generacion_NoFosil_publica + Generacion_Fosil_publica)/1000 # [TWH]
          Deficit_Generacion<- Demanda.Meta.Datos(t)-Generacion_Total
          Participacion_NoFosil <- Generacion_NoFosil_Total/Generacion_Total
          
          #State Variable
          dCapacidad_NoFosil_privada <- Incremento_Anual_Capacidad_NoFosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
          dCapacidad_Fosil_privada <- Incremento_Anual_Capacidad_Fosil_privada - Perdida_Anual_Capacidad_Fosil_privada
          dCapacidad_NoFosil_publica <- Incremento_Anual_Capacidad_NoFosil_publica - Perdida_Anual_Capacidad_NoFosil_publica
          dCapacidad_Fosil_publica <- Incremento_Anual_Capacidad_Fosil_publica - Perdida_Anual_Capacidad_Fosil_publica
          
          
          #Output model results
          list(c( #State Variables
            dCapacidad_NoFosil_privada,
            dCapacidad_Fosil_privada,
            dCapacidad_NoFosil_publica,
            dCapacidad_Fosil_publica
          ),Participacion_NoFosil=Participacion_NoFosil,
          EmisionesCO2e=EmisionesCO2e,
          Generacion_Fosil_Total=Generacion_Fosil_Total,
          Generacion_NoFosil_Total=Generacion_NoFosil_Total,
          Deficit_Generacion=Deficit_Generacion,
          Inversion_NoFosil_privada=Inversion_NoFosil_privada,
          Inversion_NoFosil_publica=Inversion_NoFosil_publica,
          Inversion_Fosil_privada=Inversion_Fosil_privada,
          Inversion_Fosil_publica=Inversion_Fosil_publica
          )
        })
      }
      
      
      #Simulate model
      
      out <- data.frame(ode(y = InitialConditions,
                            times = times,
                            func = EnerMX,
                            parms = parameters,
                            method =intg.method))
      
      p1<-ggplot(out,aes(x=time,y=Inversion_NoFosil_privada))+geom_line()+xlab("Tiempo")+scale_y_continuous(name="Inversión no fosil privada", limits=c(0, 10000))+theme_bw()
      p2<-ggplot(out,aes(x=time,y=Inversion_NoFosil_publica))+geom_line()+xlab("Tiempo")+scale_y_continuous(name="Inversion no fosil pública", limits=c(0, 10000))+theme_bw()
      p3<-ggplot(out,aes(x=time,y=Inversion_Fosil_privada))+geom_line()+xlab("Tiempo")+scale_y_continuous(name="Inversión fósil privada", limits=c(0, 500))+theme_bw()
      p4<-ggplot(out,aes(x=time,y=Inversion_Fosil_publica))+geom_line()+xlab("Tiempo")+scale_y_continuous(name="Inversión fósil pública", limits=c(0, 10000))+theme_bw()
      grid.arrange(p1,p2,p3,p4,nrow=2)
      
    })
})
