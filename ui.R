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
         $('.form_datetime').datetimepicker({format: 'yyyy-mm-dd hh:ii'});
        });
       </script>"),
  
  # Navigation Bar
  HTML("<nav class='navbar navbar-inverse navbar-fixed-top' role='navigation'>
      <div class='container'>
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
  
  HTML("<legend style='padding-top: 15px;'>Reiseauskunft</legend>"),
  
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
        HTML("<label class='col-xs-12 control-label' for='date-picker' style='padding-top: 15px;'>Datum &amp;Uhrzeit</label>
        <input label='Datum & Uhrzeit' id='datePicker' size='16' type='text' value='2016-12-07 15:30' class='form-control timepoint form_datetime' />")
      )
    ),
    
    textOutput( "output1"),
    textOutput("output2"),
    
    actionButton("showApp", "Suchen", class="pull-right"),
    leafletOutput("mymap")
  ),
  
  checkboxInput("showCurrent", "Aktuell", value = FALSE),
  
  tags$footer(id="footer", tags$p("© FZI Seminar - Gruppe 3"))
  
))