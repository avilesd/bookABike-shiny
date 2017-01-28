library(leaflet)
shinyUI(fluidPage(
  useShinyjs(),  # Include shinyjs
  
  # Application title
  titlePanel("Book a Bike!"),
  
  # Link to main.css stylesheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap-datetimepicker.min.css"),
    tags$head(tags$script(src="bootstrap-datetimepicker.min.js"))
  ),
  
  HTML("<script type='text/javascript'>
        $( document ).ready(function() {
         $('#datePicker').datetimepicker({format: 'yyyy-mm-dd hh:ii'});
        });
       </script>"),
  
  # Navigation Bar
  HTML("<nav class='navbar navbar-inverse navbar-fixed-top' role='navigation'>
      <div class=''>
        <div class='navbar-header'>
          <button type='button' class='navbar-toggle collapsed' data-toggle='collapse' data-target='#navbar' aria-expanded='false' aria-controls='navbar'>
       <span class='sr-only'>Toggle navigation</span>
         <span class='icon-bar'></span>
         <span class='icon-bar'></span>
         <span class='icon-bar'></span>
         </button>
         <a class='navbar-brand' href='#'>Book a Bike</a>
         </div>
         <div id='navbar' class='navbar-collapse collapse'>
         
         </div><!--/.navbar-collapse -->
         </div>
         </nav>"),
  
  HTML("<legend style='padding-top: 5px;'>Reiseauskunft</legend>"),
  
  # 
  # Here does the actual application begin
  #
  
  tags$div(id="removeDiv",
    fluidRow(
      column(6, 
        textInput("strasse", "Strasse & Strassennr.", value = "", placeholder = "Koenigsstrasse 1")
      ),
      column(6, 
             selectInput("stadt", "Stadt", c("Stuttgart" = "Stuttgart", "Hamburg" = "Hamburg"), selected = "Stuttgart")
             #textInput("stadt", "Stadt", value = "", placeholder = " Stuttgart")
      ),
      column(12,
        textInput("datePicker", "Datum & Uhrzeit", value = as.character(Sys.time())),
        
        actionButton("showApp", "Suchen", class="pull-right"),
        leafletOutput("mymap")
      )
    )
  ),
  
  tags$div(id="parameters",
    HTML("<div class='col-xs-12 parameters-top'>"), 
      HTML("<div class='col-xs-12 col-sm-3'>"), 
        checkboxInput("showCurrent", "Aktuell", value = FALSE),
      HTML("</div>"),
      HTML("<div class='col-xs-12 col-sm-3'>"), 
        textInput("radius", "Radius", value = 700),
      HTML("</div>"),
    HTML("</div>") 
  ),
  
  tags$footer(id="footer", tags$p("© FZI Seminar - Gruppe 3"))
  
))