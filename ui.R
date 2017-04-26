library(datasets)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Analisis Paginas de Facebook"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      
      #Seleccion tabla
      selectInput("fecha", "Elige el tiempo:",
                         c("15dias","30dias","60dias"),
                         selected = c("60dias")),
      

      # Search PAGE url
      textInput("url", "URL", "https://www.facebook.com/"),  # Text input facebook page ID
      actionButton("goButton", "Buscar"),                    # action button
      
      # Delete mongo id
      textInput("url2", "Borrar registro de la base de datos", 
                "https://www.facebook.com/"),  
      actionButton("goButton2", "Borrar"),                   # action button
      
      
      # Select interacción por tipo de publicacion
      selectInput("inter", "Tipo de interaccion:",
                  choices= c("likes","comments","shares"),
                             #,"love","haha","wow","sad","angry"),
                  selected = "totlikes"),
      
      # Select Reacciones Grafico
      checkboxGroupInput("variable", "Reacciones:",
                         c("love","haha","wow","sad","angry"),
                         selected = c("love","wow")),

      
      
      hr(),
      helpText("Troll3")
    ),
    
    # Create a spot for the barplot
    mainPanel(
     
      h5("Nombre de la pagina: "),verbatimTextOutput("name"),        # facebook page name
      h5("Ultima actualizacion: "),verbatimTextOutput("update"), 
      verbatimTextOutput("del"),
      
      plotOutput("TimePlot",  width = "100%") ,               # shares, posts, likes por tiempo
      plotOutput(outputId = "PlotTipo",  width = "100%")  ,   # Barplot "Tipo" 
      plotOutput("PieTipo",  width = "100%"),                 # Pie "Tipo"
      # plotOutput("PieReactions ",  width = "100%"),
      # plotOutput("BarReactions ",  width = "100%"),
      plotOutput("TotReactPlot",  width = "100%"),            # Barplot "Reactions"
      plotOutput("PlotWeekDay",  width = "100%"),             # Plot por dia de la semana
      plotOutput("NumPosts",  width = "100%"),                # Numero de publicaciones
      plotOutput("TypeSlot",  width = "100%"),                # Tipo por franjas horarias
      plotOutput("TypeSlotGG2",  width = "100%"), 
      plotOutput("TypeSlotGG",  width = "100%"), 
      plotOutput("TypePubWeek",  width = "100%"),             # Tipo publicación dia semana
      plotOutput("TypeReactWeek",  width = "100%")            # Tipo Reacción dia semana
      # , plotOutput("LCSporDIA",  width = "100%")
      # , plotOutput("LCSporDIAlinea",  width = "100%")
      # , plotOutput("LCSporFRANJA",  width = "100%")
      # , plotOutput("ReactDAYs",  width = "100%")
       # , plotOutput("ReactFranja",  width = "100%")
      
      
      
    ) # end mainPanel
  ) # end sidebarLayout
)  # end fluidPage


