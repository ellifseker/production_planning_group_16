library(shiny)

# Mevcut modüller
source("R/R/moving_average.R")
source("R/R/ogrenme.R")

# Yeni eklediğimiz Holt modülünü kaynak olarak gösteriyoruz
# Dosyanızın adının tam olarak "holt_method" veya "holt_method.R" olmasına göre uzantıyı ayarlayın.
# Görselde uzantısız "holt_method" görünüyor, eğer uzantısı yoksa aşağıdaki gibi bırakabilirsiniz, 
# hata verirse sonuna .R ekleyin.
source("R/R/holt_method") 

ui <- navbarPage("Production Planning",
  tabPanel("Learning Curve", learning_ui("learning")),
  tabPanel("Moving Average", moving_average_ui("ma_1")),
  
  # Yeni sekmemizi ekliyoruz
  tabPanel("Holt Method", holt_ui("holt_1"))
)

server <- function(input, output, session) {
  learning_server("learning")
  moving_average_server("ma_1")
  
  # Yeni modülün server kısmını çağırıyoruz
  holt_server("holt_1")
}

shinyApp(ui, server)
