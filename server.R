# SERVER



source('global.R', local=TRUE)

shinyServer(
function(input, output) {
        

        
        observeEvent(
                    input$goButton,
                    
                    {datasetInput <- reactive({getInfoPage(input$url)})
        
                    
        # Calculo de variables, consultas y dataframes    
                    result_data0 <- datasetInput()
                    
                    if (input$fecha == "60dias"){
                      result_data <- result_data0    
                      
                      }else{
                        if(input$fecha == "30dias"){
                          result_data <- result30(result_data0)
                          
                        }else{
                          result_data <- result15(result_data0)
                        }
                      }
                    
              observeEvent(
                      result_data, {  
                                      output$name <- renderText({getName(result_data)})   # Page name
                                      
                                      output$update <-  reactive({updateDate(input$url)})  # Update date
                                      
                                      output$del <- renderText({" "})   # Delete output
                                      
                                      totlikes <- sum(result_data$likes_count)
                                      totcomments <- sum(result_data$comments_count)
                                      totshares <- sum(result_data$shares_count)
                                      totlove <- sum(result_data$love_count)
                                      tothaha <- sum(result_data$haha_count)
                                      totwow <- sum(result_data$wow_count)
                                      totsad <- sum(result_data$sad_count)
                                      totangry <- sum(result_data$angry_count)
                  
                             # Con library(sqldf) y como resultado un dataframe:
                  
                                      totals <- sqlTotals(result_data)
                  
                                      totbytype <- sqlTypes(result_data)
                                      
                                      byday <- sqlbyday(result_data)
                  
                                      byweekday <- sqlweekday(result_data)
                                      
                                      totreactionsdf <- dfReactions(totals)
                                      
                                      cfra<-sqldf(("select type,timeslot,count(timeslot) as sum from result_data group by timeslot,type"))
                                      
        ##### GRAFICAS #####
                    
        # Render Barplot  "Likes-Comments-Shares by Time"
                    output$TimePlot <- renderPlot({
                      plot(as.Date(byday$YMDay),byday$likes, type = "l", xlab="Fecha",ylab="likes-comments-shares/dia", main = "Tipo de publicación", col = "red", lwd = 2.5)
                      lines(as.Date(byday$YMDay),byday$comments,col = "green",lwd=2.5)
                      lines(as.Date(byday$YMDay),byday$shares,col = "blue",lwd=2.5)
                      # axis.Date(1, at= as.Date(byday$day), format="%d-%m", labels = TRUE, las = 3)
                      legend("topright", # places a legend at the appropriate place
                             c("likes","comments","shares"), # puts text in the legend
                             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
                             lwd=c(2.5,2.5,2.5),col=c("red", "green","blue")) # gives the legend lines the correct color and width                
                    }) #end TimePlot
                      
          # Render Barplot  "Tipo"
                    output$PlotTipo <- renderPlot({
                      barplot(totbytype[,input$inter],
                              main=paste0(input$inter," por Tipo de Publicacion"),
                              ylab=paste0(input$inter),
                              xlab="Tipo",
                              names.arg = totbytype$type,
                              col = c("slategray1","steelblue3","steelblue4")
                              )
                    }  # ,height = 200, width = 400) #end PlotTipo
                    ) #end PlotTipo
                    
          # Grafica de pastel de Likes por Tipo de Publicacion
                    output$PieTipo<- renderPlot({
                      # type_labels <- paste( totbytype$type, totbytype$likes, sep="\n")
                      pie(totbytype[,input$inter], 
                          labels = totbytype$type, 
                          # ylab=paste0(input$inter),
                          # xlab="Tipo",
                          main= paste0(input$inter," por Tipo de Publicacion"), 
                          col = c("slategray1","steelblue3","steelblue4"))
                      
                      }) # end PieTipo
                    
          # Render Barplot  "Reactions Totales"
                            output$TotReactPlot <- renderPlot({
                              barplot(totreactionsdf[input$variable,],main="Reacciones",
                                      names.arg = input$variable,
                                      col = c("slategray1","steelblue1","steelblue2","steelblue3","steelblue4"))
                              })   # end TotReactPlot
          
          # Render Barplot Likes / weekday
                            output$PlotWeekDay<- renderPlot({
                            barplot(byweekday[,input$inter], names.arg = byweekday$weekday, 
                                    main= paste0(input$inter," por Dia de la Semana"), 
                                    col = terrain.colors(length(byweekday$weekday)))
                            
                            })  # end PlotWeekDay
                            

          # Render Barplots Type / timeslot
                            output$TypeSlot <- renderPlot({              
                            
                            # Aparece el total de type por tipo y distingue por colores que cantidad corresponde a cada franja
                            typeslot <- table(result_data$timeslot,result_data$type)
                            
                            #Otra forma de representacion (3 barras) (separando las barras de las franjas horarias)
                            barplot(typeslot, main = "Tipo de publicación por franja horaria", beside = TRUE, 
                                    col = c("slategray1","steelblue3","steelblue4"))
                            legend("topleft", 
                                   legend = rownames(typeslot), fill = c("slategray1","steelblue3","steelblue4"))
                            }) # end Type-Slot Barplots
                      
                            
                      output$TypeSlotGG2 <- renderPlot({  
                            ggplot(cfra,aes(x=type,y=sum,fill=factor(timeslot)))+
                              labs(title = "Tipo de Publicación por Franja Horaria")+
                              theme(plot.title = element_text(size = rel(2))) +
                              geom_bar(stat="identity",position="dodge")+
                              xlab("Type")+ylab("Total publicaciones")
                            }) # end Type-Slot Barplots GGplot
                            
                            
                            output$TypeSlotGG <- renderPlot({            
                            
                              ggplot(cfra,aes(x=timeslot,y=sum,fill=factor(type)))+
                                labs(title = "Tipo de Publicación por Franja Horaria")+
                                theme(plot.title = element_text(size = rel(2))) +
                                geom_bar(stat="identity",position="dodge")+
                                xlab("Franja horaria")+ylab("Total post")
                              
                            }) # end Type-Slot Barplots GGplot
                            
                            
                            
                            
                            
                            
                            
          # Likes, Comments y Shares por día de la semana
                            
          output$TypePubWeek <- renderPlot({                
                            totalLCS<-t(byweekday[,c("likes","comments","shares")])
                            totalLCSdf<-as.data.frame(totalLCS)
                            colnames(totalLCSdf)=byweekday$weekday
                            totalLCSdf$reactions<-rownames(totalLCSdf)
                            
                            totalLCSdf.long<-melt(totalLCSdf,id.vars="reactions")
                            
                            ggplot(totalLCSdf.long,aes(x=variable,y=value,fill=factor(reactions)))+
                              labs(title = "Likes, Comments y Shares por día de la semana")+
                              theme(plot.title = element_text(size = rel(2))) +
                              geom_bar(stat="identity",position="dodge")+
                              xlab(" ")+ylab("LIKES/COMMENTS/SHARES")
                    })

          
          # Reacciones por día de la semana
          output$TypeReactWeek <- renderPlot({  
                              totalreactions<-t(byweekday[,c("love","haha","wow","sad","angry")])
                              totalreactionsdf<-as.data.frame(totalreactions)
                              colnames(totalreactionsdf)=byweekday$weekday
                              totalreactionsdf$reactions<-rownames(totalreactionsdf)
                              
                              totalreactionsdf.long<-melt(totalreactionsdf,id.vars="reactions")
                              
                              ggplot(totalreactionsdf.long,aes(x=variable,y=value,fill=factor(reactions)))+
                                labs(title = "Reacciones por día de la semana")+
                                theme(plot.title = element_text(size = rel(2))) +
                                geom_bar(stat="identity",position="dodge")+
                                xlab("Dia")+ylab("Total Reactions")
            
          })
          

          
          # output$LCSporDIA <- renderPlot({    # ¿¿¿totalsbyday???
          #           library(reshape2)
          #           totalLCSdia<-t(totalsbyday[,c("likes","comments","shares")])
          #           totalLCSdiadf<-as.data.frame(totalLCSdia)
          #           colnames(totalLCSdiadf)=totalsbyday$day
          #           totalLCSdiadf$LCS<-rownames(totalLCSdiadf)
          # 
          #           totalLCSdiadf.long<-melt(totalLCSdiadf,id.vars="LCS")
          # 
          #           library(ggplot2)
          #           ggplot(totalLCSdiadf.long,aes(x=variable,y=value,fill=factor(LCS)))+
          #             geom_bar(stat="identity",position="dodge")+
          #             xlab("Dia")+ylab("TOTAL")
          # 
          # })
          
          
          # output$LCSporDIAlinea <- renderPlot({    # ¿¿¿totalsbyday???
          #             plot(as.Date(totalsbyday$day),totalsbyday$likes,type="l", xlab="Fecha",
          #                  ylab="LIKES,COMMENTS,SHARES POR DIA",labels=FALSE,col = "green",lwd=2.5)
          #             lines(as.Date(totalsbyday$day),totalsbyday$comments,col = "red",lwd=2.5) 
          #             lines(as.Date(totalsbyday$day),totalsbyday$shares,col = "blue",lwd=2.5) 
          #             axis.Date(1, at= as.Date(totalsbyday$day), format="%d-%m", labels = TRUE, las = 3)
          #             axis
          #             legend("topright",c("likes","comments","shares"),lty=c(1,1,1),
          #                    lwd=c(2.5,2.5,2.5),col=c("green", "red","blue"))
          # 
          # })
          # 
          
          # output$LCSporFRANJA <- renderPlot({   # ¿¿¿¿¿totalsbytimeslot?????
                        # totalsLCSfranja<-t(totalsbytimeslot[,c("likes","comments","shares")])
                        # totalsLCSfranjadf<-as.data.frame(totalsLCSfranja)
                        # colnames(totalsLCSfranjadf)<-totalsbytimeslot$timeslot
                        # totalsLCSfranjadf$LCS<-rownames(totalsLCSfranjadf)
                        # 
                        # totalsLCSfranjadf.long<-melt(totalsLCSfranjadf,id.vars="LCS")
                        # 
                        # ggplot(totalsLCSfranjadf.long,aes(x=variable,y=value,fill=factor(LCS)))+
                        #   geom_bar(stat="identity",position="dodge")+
                        #   xlab("Dia")+ylab("TOTAL")
          
          # })
          
          # output$ReactDAYs <- renderPlot({    # ¿¿¿totalsbyday???
          
                        # 
                        # totalREACTdia<-t(totalsbyday[,c("love","haha","wow","sad","angry")])
                        # totalREACTdiadf<-as.data.frame(totalREACTdia)
                        # colnames(totalREACTdiadf)=totalsbyday$day
                        # totalREACTdiadf$Reactions<-rownames(totalREACTdiadf)
                        # 
                        # totalREACTdiadf.long<-melt(totalREACTdiadf,id.vars="Reactions")
                        # 
                        # ggplot(totalREACTdiadf.long,aes(x=variable,y=value,fill=factor(Reactions)))+
                        #   geom_bar(stat="identity",position="dodge")+
                        #   xlab("Dia")+ylab("TOTAL")
          
          # })
          
           # output$ReactFranja <- renderPlot({   # ¿¿¿¿totalsbytimeslot???
          #
                      # totalsREACTfranja<-t(totalsbytimeslot[,c("love","haha","wow","sad","angry")])
                      # totalsREACTfranjadf<-as.data.frame(totalsREACTfranja)
                      # colnames(totalsREACTfranjadf)<-totalsbytimeslot$timeslot
                      # totalsREACTfranjadf$Reactions<-rownames(totalsREACTfranjadf)
                      # 
                      # totalsREACTfranjadf.long<-melt(totalsREACTfranjadf,id.vars="Reactions")
                      # 
                      # ggplot(totalsREACTfranjadf.long,aes(x=variable,y=value,fill=factor(Reactions)))+
                      #   geom_bar(stat="identity",position="dodge")+
                      #   xlab("Dia")+ylab("TOTAL")
          
           # })
          
          #  output$PieReactions <- renderPlot({    # ----> espacio vacio
          # 
          # # # Grafica de pastel 
          #                 totalreactions <- t(totals[,c("love","haha","wow","sad","angry")])
          #                 colnames(totalreactions) <- "Reactions"
          #                 activity_labels <- paste( rownames(totalreactions), totalreactions[,1], sep="\n")
          #                 pie(totalreactions,
          #                      labels = activity_labels,
          #                     main="TOTAL REACTIONS POR MES"
          #                      , col = terrain.colors(length(totalreactions))
          #                     )
          # 
          #  })

          
          
          
          # output$BarReactions <- renderPlot({   # ----> espacio vacio
          # # Grafica de barras
          # totalreactions <- t(totals[,c("love","haha","wow","sad","angry")])
          # totalreactionsdf <- as.data.frame(totalreactions)
          # colnames(totalreactionsdf) <- "Reactions"
          # barplot(totalreactionsdf$Reactions, names.arg = rownames(totalreactions),
          #         main="TOTAL REACTIONS POR MES", beside = TRUE,
          #         col = terrain.colors(length(totalreactionsdf$Reactions)),xlab = c("Reactions"),
          #         ylab = c("TOTAL"),axes = TRUE,axisnames = TRUE)
          # 
          #             })
          
          output$NumPosts <- renderPlot({ 
                    cdia<-sqldf(("select type,day,count(id) as sum_id from result_data group by day,type"))
                    
                    ggplot(cdia,aes(x=day,y=sum_id,fill=factor(type)))+
                      labs(title = "Número de publicaciones")+
                      theme(plot.title = element_text(size = rel(2))) +
                      geom_bar(stat="identity",position="dodge")+
                      xlab("Día")+ylab("Número posts")
                      })
          
          
                      }) # end second observe event
                            
                }  # end datasetInput argument from observeEvent
          
              ) #end observeEvent
              
  
  
  
  
            ##### DELETE button ##### 
              observeEvent(
                input$goButton2,
                { output$del <- reactive({deletemongo(input$url2)})
                }
              ) # end observeEvent Delete Button
  
  
  } # end function 
  )#end shiny server

