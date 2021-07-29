library(shiny)#
library(dbConnectBPO)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(ECharts2Shiny)
library(echarts4r)
library(RColorBrewer)
library(highcharter)
library(shiny)
library(shinyWidgets) 
library(shinyjs)

source("helper.R")

APP_NAME <<- "Mapa Hit Rate"  # bardzo ważne aby ta nazwa pokrywała się z nazwą jaką nadaliśmy aplikacji w tabeli shiny.apps na anr1.infr.co
PROTECTED <<- T





ui <- dashboardPage(
  dashboardHeader( 
    title = "Raport eventów"
  ),
  skin = c("yellow"),
  dashboardSidebar( 
    sidebarMenu(id = '1',
                
                
                menuItem("Mapa Hit Rate",
                         tabName = "tab1"),
                
                menuItem("Legenda",
                         tabName = "tab2")
                #icon = icon("columns"))
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
    ),
    
    ### filtry ogolne
    
    
    fluidRow( tags$script(HTML("$('body').addClass('fixed');")), 
              
              
              pickerInput(
                inputId = "select_grupa", 
                label = "Grupa produktowa",  
                choices = sort(grupy),
                selected = "slim",
                options = pickerOptions(
                  actionsBox = TRUE, 
                  size = 10,
                  selectedTextFormat = "count > 3",
                  deselectAllText = 'odznacz wszystkie',
                  selectAllText = 'zaznacz wszytskie',
                  liveSearch = TRUE
                ), 
                multiple = TRUE
              ),
              
              
              pickerInput(
                inputId = "select_kanal_l", 
                label = "Kanał reklamy",  
                choices = sort(kanal_l[, action_channel]),
                selected = sort(kanal_l[, action_channel]),
                options = pickerOptions( 
                  actionsBox = TRUE, 
                  size = 10,     
                  selectedTextFormat = "count > 3",
                  deselectAllText = 'odznacz wszystkie',
                  selectAllText = 'zaznacz wszytskie',
                  liveSearch = TRUE
                ), 
                multiple = TRUE
              ),
              
              
              pickerInput(
                inputId = "select_kraj", 
                label = "Kraj",  
                choices = sort(kraj_l[, campaign_market]),
                selected = "PL",
                options = pickerOptions(
                  actionsBox = TRUE, 
                  size = 10,
                  selectedTextFormat = "count > 3",
                  deselectAllText = 'odznacz wszystkie',
                  selectAllText = 'zaznacz wszytskie',
                  liveSearch = TRUE
                ), 
                multiple = TRUE
              ),
              
              
              # awesomeCheckboxGroup(
              #   inputId = "select_typ_sciezki",
              #   label = "Typ ścieżki", 
              #   choices = c("reaktywacyjna" = "zam_wysylkowe", "rezygnacyjna" = "zam_rezygnacyjne"),
              #   selected = "zam_wysylkowe"
              # ),
              
              
              
              dateRangeInput('select_daty',
                             label = 'Data wysłania',
                             start = paste0(substring(Sys.Date()-180,1,7),"-01") , end = Sys.Date()
              ),
              awesomeRadio(
                inputId = "select_typ_leada",
                label = "Działanie", 
                choices = c("import", "kontakt"),
                selected = "kontakt"
              ),
              
              
              
              
              actionButton("zastosuj2" , "Zastosuj",
                           style="color: #191970; background-color: #e0a031;border-color: #191970;"),
              h4(textOutput("selected_var")),
              h4(textOutput("selected_var2"))
    )
    
    
    
    
  ),
  
  dashboardBody(
    
    tags$head( 
      includeScript("www/js/getLogin.js"),
      
      
      tags$script('$(document).ready(function () {
                  
                  var tab1_p = document.querySelector(".sidebar-menu li a[data-value=tab1]");
                  var tab2_p = document.querySelector(".sidebar-menu li a[data-value=tab2]");
                  
                  
                  
                  tab_1p.style.display = "none";
                  tab_2p.style.display = "none";
                  })
                  var client = new XMLHttpRequest();
                  client.open("GET", window.location.href, true);
                  client.send();
                  client.onreadystatechange = function() {
                  var response = client.getResponseHeader("X-Remote-User");
                  var link = window.location.href;
                  setTimeout(function() {Shiny.onInputChange("login", response)}, 10); 
                  }
                  
                  ;')
      
    ),
    
    
    
    includeCSS("~/ShinyApps/mapa_hit_rate/load.css"),
    
    conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                     div(style="width:100%;position: absolute;top:200px;",
                         div(style="height:0;z-index: 1000;position: fixed; margin: 0 auto;left: 0;right: 0;",
                             includeHTML("~/ShinyApps/mapa_hit_rate/loadh.html")))),
    
    
    
    
    
    
    
    useShinyjs(), 
    shinyjs::hidden(progressBar(id = "progres_bar", value = 0, striped = T, display_pct = T, title = NULL)),
    
    tabItems(
      # <tr>
      #   </tr>
      #   <tr>				
      #   <td style="font-size:15px; color:#46484; padding-left:20px "><br><b>Ścieżka konsumenta</b> jest otwierana przez zamówienie, które jest na jednym ze statusów : <u> Sent, Warranty, Resigned</u>.  
      # <br>Ścieżka jest przerywana jeśli powstanie kolejne zamówienie które jest na jednym z wcześniej wymienionych statusów. Po dacie rozpoczęcia ścieżki, sprawdzamy kiedy nastąpił kontakt przez kanały reklamy PMB.
      # </td>
      #   </tr>
      #   <tr>				
      #   <td style="font-size:15px; color:#46484; padding-left:20px "><br><br>Co oznaczają poszczególne atrybuty ścieżki:<ul>
      #   <li type="square"><b>Grupa produktowa ścieżki</b>:grupa produktowa kampanii z której powstało zamówienie.</li>
      #   <li type="square"><b>Kanał reklamy ścieżki</b>: kanał reklamy w którym powstało zamówienie.</li>
      #   <li type="square"><b>Kraj ścieżki</b>: kraj wysyłki zamówienia lub jeśli nie mamy tej informacji to jest to język konsumenta.</li>
      #   </ul>
      #   </td>
      #   </tr>
      
      
      
      
      # <tr>				
      #   <td style="font-size:15px; color:#46484; padding-left:20px "><br><u><b>UWAGA:</b></u> 
      #   <br>Jeśli konsument jednego dnia miał wysłane dwa różne zamówienia reaktywacyjne, to liczymy to jako jedną ścieżkę. Analogicznie, jeśli konsument miał jednego dnia dwa różne zamówienia rezygnacyjne, to liczymy to jako jedną ścieżkę.
      # <br>Od momentu rozpoczęcia ścieżki sprawdzamy daty kiedy następował kontakt poprzez kanały reklamy PMB - 
      #   </td>
      #   </tr>
      #   
      #   
      #   </tr>
      
      tabItem(tabName = "tab2",
              HTML('
                   <html>
                   <head>
                   </head>
                   <body>
                   <font face="Arial">
                   <table>
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px ">Dashboard przedstawia wartość wskaźnika <b>Hit Rate</b> w dwóch perspektywach.: <br>
                   -pierwsza pokazuje kontakt za pomocą kanałów reklamy PMB. <br>
                   -druga pokazuje załadowanie do narzędzia wykonującego akcje. <br>
                   Raport przedstawia również wskaźnik w perspektywie różnicy w dniach pomiędzy  wysłaniem ostatniego zamówienia konsumenta przed wykonanym działaniem (kontaktem lub importem), a datą tego działania (kontakt lub import).
                   <br>Raport uwzględnia zamówienia, które zostały wysłane w ciągu ostatnich 180 dni oraz które są na statusie Sent. Źródło danych dla raportu jest odświeżane w weekendy.
                   </td>
                   </tr>
                   
                   
                   
                   <tr>
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><u><b>UWAGA:</b></u>
                   <br>Pokazujemy działania PMB z
                   wyłączeniem akcji z typu resigned. W przypadku działań z kanału EMAIL typu reactivation bierzemy pod uwagę tylko pierwszy follow up.
                   </td>
                   </tr>
                   
                   
                   </tr>
                   
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><br>Co oznacza import w poszczególnych kanałach reklamy:
                   <ul>
                   <li type="square"><b>Import w kanale EMAIL</b>: do konsumenta został wysłany email.</li>
                   <li type="square"><b>Import w kanale POST</b>: do konsumenta został wysłany list.</li>
                   <li type="square"><b>Import w kanale SMS</b>: do konsumenta został wysłany sms.</li>
                   <li type="square"><b>Import w kanale TEL</b>: konsument został załadowany do serwisu w celu obsłużenia przez konsultanta.</li>
                   <li type="square"><b>Import w kanale INSERT</b>: została odnotowana informacja dla partnera logistycznego o dodaniu konkretnej ulotki do zamówienia</li>
                   <li type="square"><b>Import w kanale VMS</b>: konsument został załadowany do Cludo w celu obsłużenia przez vms</li>
                   </ul>
                   </td>
                   </tr>
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><br>Co oznacza kontakt w poszczególnych kanałach reklamy:
                   <ul>
                   <li type="square"><b>Kontakt w kanale EMAIL</b>: do konsumenta został wysłany i dostarczony do głównego folderu odbiorczego email.</li>
                   <li type="square"><b>Kontakt w kanale POST</b>: do konsumenta został wysłany list.</li>
                   <li type="square"><b>Kontakt w kanale SMS</b>: do konsumenta został wysłany i dostarczony sms.</li>
                   <li type="square"><b>Kontakt w kanale TEL</b>: konsultant po otrzymaniu danego konsumenta do wydzwonienia dodzwonił się lub włączyła się poczta.</li>
                   <li type="square"><b>Kontakt w kanale INSERT</b>: do konsumenta została wysłana i dostarczona przez partnera logistycznego ulotka</li>
                   <li type="square"><b>Kontakt w kanale VMS</b>: do konsumenta podjął próbę dodzwonienia się vms</li>
                   </ul>
                   </td>
                   </tr>
                   
                   
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><b>Liczba zamówień z działaniem</b> to liczba zamówień po których wysłaniu nastąpił kontakt lub import za pośrednictwem któregoś z kanałów reklamy PMB.</u>.  
                   </td>                  
                   </tr>
                   
                   <tr>
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><b>Liczba zamówień dedykowanych dla kanału reklamy</b> to zamówienia utworzone przez konsumenta który spełnia kryteria do wzięcia udziału w danym kanale reklamy PMB</u>. 
                   
                   </td>
                   </tr>
                   
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><br>Kryteria do wzięcia udziału w danym kanale reklamy PMB:
                   <ul>
                   <li type="square"><b>EMAIL</b>: konsument musiał uzupełnić pole przeznaczone dla adresu email oraz adres ten powinien być poprawny (mamy odpowiedź od Emarsysa że email jest poprawny, jeśli klient nie był w Emarsysie czyli nie mamy odpowiedzi na pytanie czy email jest poprawny to uznaje go za poprawny) oraz nie ma RODO.</li>
                   <li type="square"><b>POST</b>: konsument musi mieć uzupełniony adres oraz nie ma RODO</li>
                   <li type="square"><b>SMS</b>: konsument musi mieć telefon komórkowy, nie miał negatywnej odpowedzi na smsa oraz nie ma RODO</li>
                   <li type="square"><b>TEL</b>: konsument musi mieć telefon, nie miał negatywnej odpowiedzi na vmsa oraz nie ma RODO</li>
                   <li type="square"><b>INSERT</b>: dla INSERTU nie mamy żadnych warunków nakładach na konsumenta</li>
                   <li type="square"><b>VMS</b>: konsument musi mieć telefon oraz nie ma RODO</li>
                   </ul>
                   </td>
                   </tr>
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><br>Hit Rate liczony w podziale na kanały reklamy PMB, możemy liczyć na dwa sposoby:
                   <ul>
                   <li type="square"><b>Hit Rate ogólny</b>: Liczba zamówień z działaniem w danym kanale reklamy / Liczba zamówień</li>
                   <li type="square"><b>Hit Rate dedykowany dla kanału reklamy</b>: Liczba zamówień z działaniem w danym kanale reklamy / Liczba zamówień spełniająca warunki danego kanału reklamy PMB (czyli Liczba zamówień dedykowanych dla kanału)</li>
                   </ul>
                   </td>
                   </tr>
                   
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><br> Z czego wynikają różnice w wartości wskaźnika Hit Rate w innych raportach dotyczących kanału reklamy INSERT:
                   <ul>
                   <li type="square"><b> Statusy </b>: W raporcie insert pokazujemy wszystkie zamówienia które osiągnęły status  „To sent”. 
                   W raporcie eventów pokazujemy ścieżki rozpoczęte przez zamówienia na statusach „Sent”.
                   .</li>
                   <li type="square"><b> Data raportowa </b>: W raporcie insert raportujemy po dacie przejścia zamówienia na status „To sent”.
                   W raporcie eventów raportujemy po dacie wysyłki zamówienia.
                   </li>
                   <li type="square"><b> Warunki liczenia Hit Rate </b>: W raporcie insert możemy liczyć Hit Rate jako liczbę zamówień z insertem dołożonym przez PCMgr lub przez partnera logistycznego.
                   W raporcie eventów liczmy tylko według  tego co zaraportował nam partner logistyczny.
                   </li>
                   </ul>
                   </td>
                   </tr>
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><br> Z czego wynikają różnice w wartości wskaźnika Hit Rate w innych raportach dotyczących kanału reklamy POST:
                   <ul>
                   <li type="square"><b> Data raportowa </b>: W raporcie hit rate clients raportujemy po dacie wpadnięcie konsumenta do bazy lub po dacie jego ostatniego zamówienia.
                   W raporcie eventów raportujemy po dacie wysyłki zamówienia.
                   W raporcie eventów pokazujemy ścieżki konsumenta a więc jeden konsument może występować kilkukrotnie a w hit rate clients nie.
                   
                   </li>
                   <li type="square"><b> Warunki wykluczające </b>: W raporcie hit rate clients jest możliwość wykluczenia w filtrze RODO, is return, was waranty, sms negative response.
                   W raporcie eventów  konsument musi mieć uzupełniony adres oraz nie ma RODO.
                   </li>
                   </ul>
                   </td>
                   </tr>
                   
                   
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><br> Z czego wynikają różnice w wartości wskaźnika Hit Rate w innych raportach dotyczących kanału reklamy VMS:
                   <ul>
                   <li type="square"><b> Data raportowa </b>: W raporcie hit rate orders raportujemy po dacie powstania zamówienia.
                   W raporcie eventów raportujemy po dacie wysyłki zamówienia.
                   
                   
                   </li>
                   <li type="square"><b> Statusy </b>: W raporcie hit rate orders mamy możliwość wyboru statusów.
                   W raporcie eventów pokazujemy ścieżki rozpoczęte przez zamówienia na statusach „Sent”.
                   
                   </li>
                   <li type="square"><b> Warunki wykluczające </b>: W raporcie hit rate orders wykluczamy: was_warranty, RODO, sms_negative_response, error przy wysyłce danych do Cludo.
                   W raporcie eventów konsument musi mieć telefon według cludo, oraz nie ma RODO.
                   </li>
                   
                   </ul>
                   </td>
                   </tr>
                   
                   
                   
                   
                   
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><br> Z czego wynikają różnice w wartości wskaźnika Hit Rate w innych raportach dotyczących kanału reklamy SMS:
                   <ul>
                   <li type="square"><b> Data raportowa </b>: W raporcie hit rate orders raportujemy po dacie powstania zamówienia.
                   W raporcie eventów raportujemy po dacie wysyłki zamówienia.
                   </li>
                   <li type="square"><b> Statusy </b>: W raporcie hit rate orders mamy możliwość wyboru statusów.
                   W raporcie eventów pokazujemy ścieżki rozpoczęte przez zamówienia na statusach „Sent”.
                   </li>
                   <li type="square"><b> Warunki wykluczające </b>: W raporcie hit rate orders warunki wykluczające zależą od tego, czy akcja była typu rezygnacja czy reaktywacja.
                   Akcja rezygnacja: wartość zamówienia jest większa od 1, zamówienie nie jest snickersem, jest uzupełniona grupa produktowa w zamówieniu, status zamówienia to „Resigned” oraz zamówienie nie pochodzi z akcji POSTowej: HRSR15.
                   Akcja reaktywacja: status zamówienia to „Sent”, wartość zamówienia nie jest równa 0.
                   W raporcie eventów konsument musi mieć telefon komórkowy, nie miał negatywnej odpowiedzi na smsa oraz nie ma RODO.
                   
                   </li>
                   
                   </ul>
                   </td>
                   </tr>
                   
                   
                   
                   
                   <tr>				
                   <td style="font-size:15px; color:#46484; padding-left:20px "><br><br> Z czego wynikają różnice w wartości wskaźnika Hit Rate w innych raportach dotyczących kanału reklamy TEL:
                   <ul>
                   <li type="square"><b> Data raportowa </b>: W raporcie hit rate orders raportujemy po dacie powstania zamówienia.
                   W raporcie eventów raportujemy po dacie wysyłki zamówienia.
                   </li>
                   <li type="square"><b> Statusy </b>: W raporcie hit rate orders mamy możliwość wyboru statusów.
                   W raporcie eventów pokazujemy ścieżki rozpoczęte przez zamówienia na statusach „Sent”.
                   
                   </li>
                   <li type="square"><b> Warunki liczenia Hit Rate  </b>: W raporcie eventów mamy dodatkowy warunek dla kontaktu, na to aby był faktyczny kontakt czyli aby call time był większy od 0. W hit rate kontakt jest podjęciem leada przez konsultanta.
                   </li>
                   
                   </ul>
                   </td>
                   </tr>
                   
                   
                   
                   
                   
                   
                   
                   </table>
                   </font>
                   </body>
                   </html>
                   '
                   
              )
      ),
      tabItem(tabName = "tab1",
              #fluidRow(
              fluidPage(
                
                
                
                # actionButton('lolek', "Filtry"),
                # 
                # shinyBS::bsModal("modal", trigger = "lolek",
                #                  size = 'large',
                #                  title = 'Filtry :',
                
                
                
                
                
                
                column(3,
                       pickerInput(
                         inputId = "select_kanal", 
                         label = "Kanał reklamy PMB",  
                         choices = sort(unique(kanal_dt[, action_channel])),
                         selected = sort(unique(kanal_dt[, action_channel])),
                         options = pickerOptions(
                           actionsBox = TRUE, 
                           size = 10,
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'odznacz wszystkie',
                           selectAllText = 'zaznacz wszytskie',
                           liveSearch = TRUE
                         ), 
                         multiple = TRUE
                       )),
                
                column(3,
                       pickerInput(
                         inputId = "select_action_type", 
                         label = "Typ akcji PMB",  
                         choices = sort(unique(dane_baza[, action_type])),
                         selected = 
                           
                           c( "active sale", 
                              
                              
                              "cross sell", 
                              "cross young", 
                              "followup" ,              
                              "followup sms do insertu", 
                              "followup sms do postu",
                              "followup vms do post",
                              
                              "reactivation" ,
                              "reactivation #2",
                              
                              "winback"),
                         
                         options = pickerOptions(
                           actionsBox = TRUE, 
                           size = 10,
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'odznacz wszystkie',
                           selectAllText = 'zaznacz wszytskie',
                           liveSearch = TRUE
                         ), 
                         multiple = TRUE
                       )),
                
                
                
                
                column(3,
                       actionButton("zastosuj" , "Zastosuj", 
                                    style="color: #191970; background-color: #e0a031;border-color: #191970;")),
                
                
                
                
                
                
                box(title = "Wskaźnik HitRate w zależności od miesiąca", 
                    
                    width = 12, status = "primary",height = "720" ,solidHeader = T, plotly::plotlyOutput("plot_01", height= "650px")),
                
                column(3,awesomeRadio(
                  inputId = "select_typ_hit",
                  label = "Rodzaj Hit Rate na wykresie", 
                  choices = c("dedykowany dla kanału rekalmy", "ogólny"),
                  selected = "dedykowany dla kanału rekalmy",
                  inline = T
                )),
                
                box(title = "Wskaźnik HitRate w zależności od miesiąca w podziale na kanały reklamy PMB", 
                    width = 12, status = "primary",height = "720" ,solidHeader = T, plotly::plotlyOutput("plot_02", height= "650px")),
                
                
                # box(title = "Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt", 
                #     width = 12,
                #     status = "primary",height = "720" ,solidHeader = T, plotly::plotlyOutput("plot_1", height= "650px")),
                # 
                # 
                # box(title = "Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt w podziale na kanały reklamy PMB", 
                #     width = 12, status = "primary",height = "720" ,solidHeader = T, plotly::plotlyOutput("plot_2", height= "650px")),
                
                
                column(3,
                       
                       awesomeRadio(
                         inputId = "select_typ_hit_rate",
                         label = "Rodzaj Hit Rate na wykresie", 
                         choices = c("dedykowany dla kanału rekalmy", "ogólny"),
                         selected = "dedykowany dla kanału rekalmy",
                         inline = T
                       )),
                
                column(3,
                       
                       awesomeRadio(
                         inputId = "select_typ_kanalu_na_wykresie",
                         label = "Czy dodać rozróżnienie typów akcji?", 
                         choices = c("tak", "nie"),
                         selected = "nie",
                         inline = T
                       )),
                
                column(3,
                       
                       awesomeRadio(
                         inputId = "select_kalka",
                         label =  "Czy nałożyć kalkę działań?", 
                         choices = c("tak", "nie"),
                         selected = "nie",
                         inline = T
                       )),
                
                column(3,
                       pickerInput(
                         inputId = "select_action_type_kanaly", 
                         label = "Typ akcji PMB",  
                         choices = sort(unique(dane_baza[, action_type])),
                         selected = sort(unique(dane_baza[, action_type])),
                         
                         options = pickerOptions(
                           actionsBox = TRUE, 
                           size = 10,
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'odznacz wszystkie',
                           selectAllText = 'zaznacz wszytskie',
                           liveSearch = TRUE
                         ), 
                         multiple = TRUE
                       )),
                
                
                column(3, actionButton("zastosuj3" , "Zastosuj", 
                                       style="color: #191970; background-color: #e0a031;border-color: #191970;")),
                
                
                mainPanel(width = 12,
                          tabsetPanel(type = "tabs",
                                      
                                      tabPanel("Wykres per okres", 
                                               
                                               
                                               box(title = "Wskaźnik HitRate w zależności od okresu w którym nastąpił kontakt dla kanału reklamy SMS", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("okresy_wykres_SMS", height= "550px"))
                                      ),
                                      tabPanel("Tabela per okresy", 
                                               
                                               
                                               
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności dnia w którym nastąpił kontakt dla kanału reklamy SMS - tabela"),
                                                        downloadLink('download_SMS_1', 'Pobierz'),
                                                        tableOutput("kable_1A_SMS"),
                                                        h3(" W podziale na typ akcji"),
                                                        downloadLink('download_SMS_2', 'Pobierz'),
                                                        tableOutput("kable_2A_SMS")
                                                        
                                                      ))
                                      ),
                                      
                                      tabPanel("Wykres per dzień", 
                                               
                                               
                                               
                                               
                                               div(id = "SMS_legenda_o",
                                                   box(id = 'SMS_legenda', width = 12, 
                                                       title = "Legenda dla kalki działań dla kanału reklamy SMS", status = "success",height = "312", solidHeader = T,
                                                       collapsible = T,
                                                       
                                                       # p("Legenda dla kalki działań dla kanału reklamy SMS:"),
                                                       p(span("Zakres oznaczony kolorem niebieskim ", style = "color:blue"),
                                                         "oznacza SMS QX-SELL - planowe dni działań: 1."),
                                                       p(span("Zakres oznaczony kolorem czerwonym", style = "color:red"),
                                                         "oznacza SMS X-SELL 1 - planowe dni działań: 12 - 14."),
                                                       p(span("Zakres oznaczony kolorem pomarańczowym", style = "color:orange"),
                                                         "oznacza SMS REAKT 1 - planowe dni działań: 20 - 22."),
                                                       p(span("Zakres oznaczony kolorem zielonym", style = "color:green"),
                                                         "oznacza SMS X-SELL 2 - planowe dni działań: 29 - 33."),
                                                       p(span("Zakres oznaczony kolorem fioletowym", style = "color:purple"),
                                                         "oznacza SMS REAKT 2 - planowe dni działań: 44 - 47."),
                                                       p(span("Zakres oznaczony kolorem różowym", style = "color:magenta"),
                                                         "oznacza SMS REAKT 3 - planowe dni działań: 58 - 63."),
                                                       p(span("Zakres oznaczony kolorem szarym", style = "color:silver"),
                                                         "oznacza SMS WB - planowe dni działań: 25 - 90."),
                                                       p(span("Zakres oznaczony kolorem cieno szarym", style = "color:gray"),
                                                         "oznacza SMS AS 1 / SMS FU - planowe dni działań: 150 - 210."),
                                                       p(span("Zakres oznaczony kolorem żółtym", style = "color:yellow"),
                                                         "oznacza SMS AS 2 / SMS FU - planowe dni działań: 210 - 270.")
                                                       
                                                       
                                                   )),
                                               
                                               box(title = "Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy SMS", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("plot_kanaly_SMS", height= "550px"))
                                      ),
                                      
                                      
                                      
                                      
                                      tabPanel("Tabela per dzień", 
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy SMS - tabela"),
                                                        tableOutput("kable_SMS"))))
                                      
                                      
                                      
                          )) 
                ,
                
                
                
                
                
                mainPanel(width = 12,
                          tabsetPanel(type = "tabs",
                                      
                                      tabPanel("Wykres per okres", 
                                               
                                               
                                               box(title = "Wskaźnik HitRate w zależności od okresu w którym nastąpił kontakt dla kanału reklamy INSERT", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("okresy_wykres_INSERT", height= "550px"))
                                      ),
                                      
                                      tabPanel("Tabela per okres",
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności dnia w którym nastąpił kontakt dla kanału reklamy INSERT - tabela"),
                                                        downloadLink('download_INSERT_1', 'Pobierz'),
                                                        tableOutput("kable_1A_INSERT"),
                                                        h3(" W podziale na typ akcji"),
                                                        downloadLink('download_INSERT_2', 'Pobierz'),
                                                        tableOutput("kable_2A_INSERT")
                                                        
                                                      ))),
                                      
                                      
                                      tabPanel("Wykres per dzień", 
                                               div(id = "INSERT_legenda_o",
                                                   box(id = 'INSERT_legenda', width = 12, 
                                                       title = "Legenda dla kalki działań dla kanału reklamy INSERT", status = "success",height = "100", solidHeader = T,
                                                       
                                                       # p("Legenda dla kalki działań dla kanału reklamy SMS:"),
                                                       p(span("Zakres oznaczony kolorem niebieskim ", style = "color:blue"),
                                                         "oznacza INSERT - planowe dni działań: 0.")
                                                       
                                                   )),
                                               
                                               box(title = "Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy INSERT", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("plot_kanaly_INSERT", height= "550px"))),
                                      
                                      tabPanel("Tabela per dzien",
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy INSERT - tabela"),
                                                        tableOutput("kable_INSERT"))))
                                      
                                      
                          )),
                
                
                mainPanel(width = 12,
                          tabsetPanel(type = "tabs",
                                      tabPanel("Wykres per okres", 
                                               
                                               
                                               box(title = "Wskaźnik HitRate w zależności od okresu w którym nastąpił kontakt dla kanału reklamy POST", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("okresy_wykres_POST", height= "550px"))
                                      ),
                                      
                                      tabPanel("Tabela per okres",
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności dnia w którym nastąpił kontakt dla kanału reklamy POST - tabela"),
                                                        downloadLink('download_POST_1', 'Pobierz'),
                                                        tableOutput("kable_1A_POST"),
                                                        h3(" W podziale na typ akcji"),
                                                        downloadLink('download_POST_2', 'Pobierz'),
                                                        tableOutput("kable_2A_POST")
                                                        
                                                      ))),
                                      
                                      tabPanel("Wykres per dzień", 
                                               div(id = "POST_legenda_o",
                                                   box(id = 'POST_legenda', width = 12, 
                                                       title = "Legenda dla kalki działań dla kanału reklamy POST", status = "success",height = "105", solidHeader = T,
                                                       
                                                       # p("Legenda dla kalki działań dla kanału reklamy SMS:"),
                                                       p(span("Zakres oznaczony kolorem niebieskim ", style = "color:blue"),
                                                         "oznacza POST 1 - planowe dni działań: 1 - 150."),
                                                       p(span("Zakres oznaczony kolorem czerwonym ", style = "color:red"),
                                                         "oznacza POST 2 - planowe dni działań: 151 - 360.")
                                                       
                                                   )),
                                               
                                               box(title = "Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy POST", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("plot_kanaly_POST", height= "550px"))),
                                      
                                      
                                      
                                      
                                      tabPanel("Tabela per dzień",
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy POST - tabela"),
                                                        tableOutput("kable_POST"))))
                                      
                                      
                          )),
                
                
                mainPanel(width = 12,
                          tabsetPanel(type = "tabs",
                                      tabPanel("Wykres per okres", 
                                               
                                               
                                               box(title = "Wskaźnik HitRate w zależności od okresu w którym nastąpił kontakt dla kanału reklamy EMAIL", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("okresy_wykres_EMAIL", height= "550px"))
                                      ),
                                      tabPanel("Tabela per okres",
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności dnia w którym nastąpił kontakt dla kanału reklamy EMAIL - tabela"),
                                                        downloadLink('download_EMAIL_1', 'Pobierz'),
                                                        tableOutput("kable_1A_EMAIL"),
                                                        h3(" W podziale na typ akcji"),
                                                        downloadLink('download_EMAIL_2', 'Pobierz'),
                                                        tableOutput("kable_2A_EMAIL")
                                                        
                                                      ))),
                                      
                                      
                                      tabPanel("Wykres per dzień", 
                                               div(id = "EMAIL_legenda_o",
                                                   box(id = 'EMAIL_legenda', width = 12, 
                                                       title = "Legenda dla kalki działań dla kanału reklamy EMAIL", status = "success",height = "180", solidHeader = T,
                                                       
                                                       # p("Legenda dla kalki działań dla kanału reklamy SMS:"),
                                                       p(span("Zakres oznaczony kolorem niebieskim ", style = "color:blue"),
                                                         "oznacza EMAIL X-SELL 1 - planowe dni działań: 7 - 21"),
                                                       p(span("Zakres oznaczony kolorem czerwonym ", style = "color:red"),
                                                         "oznacza EMAIL REAKT 1 - planowe dni działań: 18 - 39"),
                                                       p(span("Zakres oznaczony kolorem pomarańczowym ", style = "color:orange"),
                                                         "oznacza EMAIL X-SELL 2 - planowe dni działań: 55 - 120"),
                                                       p(span("Zakres oznaczony kolorem zielonym ", style = "color:green"),
                                                         "oznacza EMAIL REAKT 2 - planowe dni działań: 80 - 180")
                                                       
                                                   )),
                                               
                                               box(title = "Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy EMAIL", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("plot_kanaly_EMAIL", height= "550px"))),
                                      
                                      
                                      
                                      tabPanel("Tabela per dzień",
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy EMAIL - tabela"),
                                                        tableOutput("kable_EMAIL"))))
                                      
                                      
                          )),
                
                
                mainPanel(width = 12,
                          tabsetPanel(type = "tabs",
                                      tabPanel("Wykres per okres", 
                                               
                                               
                                               box(title = "Wskaźnik HitRate w zależności od okresu w którym nastąpił kontakt dla kanału reklamy TEL", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("okresy_wykres_TEL", height= "550px"))
                                      ),
                                      
                                      tabPanel("Tabela per okres",
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności dnia w którym nastąpił kontakt dla kanału reklamy TEL - tabela"),
                                                        downloadLink('download_TEL_1', 'Pobierz'),
                                                        tableOutput("kable_1A_TEL"),
                                                        h3(" W podziale na typ akcji"),
                                                        downloadLink('download_TEL_2', 'Pobierz'),
                                                        tableOutput("kable_2A_TEL")
                                                        
                                                      ))),
                                      
                                      
                                      tabPanel("Wykres per dzień", 
                                               div(id = "TEL_legenda_o",
                                                   box(id = 'TEL_legenda', width = 12, 
                                                       title = "Legenda dla kalki działań dla kanału reklamy TEL", status = "success",height = "150", solidHeader = T,
                                                       
                                                       # p("Legenda dla kalki działań dla kanału reklamy SMS:"),
                                                       p(span("Zakres oznaczony kolorem niebieskim ", style = "color:blue"),
                                                         "oznacza TEL P2 1 - planowe dni działań: 23 - 37"),
                                                       p(span("Zakres oznaczony kolorem czerwonym ", style = "color:red"),
                                                         "oznacza TEL P2 2 - planowe dni działań: 68 - 96"),
                                                       p(span("Zakres oznaczony kolorem pomarańczowym ", style = "color:orange"),
                                                         "oznacza TEL P3 1 - planowe dni działań: 150 - 360")
                                                   )),
                                               
                                               box(title = "Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy TEL", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("plot_kanaly_TEL", height= "550px"))),
                                      
                                      
                                      
                                      tabPanel("Tabela per dzień",
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy TEL - tabela"),
                                                        tableOutput("kable_TEL"))))
                                      
                                      
                          )),
                
                
                mainPanel(width = 12,
                          tabsetPanel(type = "tabs",
                                      tabPanel("Wykres per okres", 
                                               
                                               
                                               box(title = "Wskaźnik HitRate w zależności od okresu w którym nastąpił kontakt dla kanału reklamy VMS", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("okresy_wykres_VMS", height= "550px"))
                                      ),
                                      
                                      tabPanel("Tabela per okres",
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności dnia w którym nastąpił kontakt dla kanału reklamy VMS - tabela"),
                                                        downloadLink('download_VMS_1', 'Pobierz'),
                                                        tableOutput("kable_1A_VMS"),
                                                        h3(" W podziale na typ akcji"),
                                                        downloadLink('download_VMS_2', 'Pobierz'),
                                                        tableOutput("kable_2A_VMS")
                                                        
                                                      ))),
                                      
                                      tabPanel("Wykres per dzień", 
                                               div(id = "VMS_legenda_o",
                                                   box(id = 'VMS_legenda', width = 12, 
                                                       title = "Legenda dla kalki działań dla kanału reklamy VMS", status = "success",height = "150", solidHeader = T,
                                                       
                                                       # p("Legenda dla kalki działań dla kanału reklamy SMS:"),
                                                       p(span("Zakres oznaczony kolorem niebieskim ", style = "color:blue"),
                                                         "oznacza VMS 1 - planowe dni działań: 22 - 68"),
                                                       p(span("Zakres oznaczony kolorem czerwonym ", style = "color:red"),
                                                         "oznacza VMS 2 - planowe dni działań: 69 - 96"),
                                                       p(span("Zakres oznaczony kolorem pomarańczowym ", style = "color:orange"),
                                                         "oznacza VMS P3 1 / VMS POST FU - planowe dni działań: 151- 360")
                                                   )),
                                               
                                               box(title = "Wskaźnik HitRate w od zależności dnia w którym nastąpił kontakt dla kanału reklamy VMS", 
                                                   width = 12, status = "success",height = "610" ,solidHeader = T, plotly::plotlyOutput("plot_kanaly_VMS", height= "550px"))),
                                      
                                      
                                      
                                      tabPanel("Tabela per dzień",
                                               column(12,
                                                      fluidRow(
                                                        h3(" Wskaźnik HitRate w zależności od dnia w którym nastąpił kontakt dla kanału reklamy VMS - tabela"),
                                                        tableOutput("kable_VMS")))
                                               
                                      )
                                      
                          )),
                
                
                
                
                
                
                
                
                
                
                
                # column(12,
                #        
                #        fluidRow(
                #          h3("Eventy konsumenta "),
                #          
                #          column(2,
                #                 numericInput("select_liczba_krokow", "Liczba kroków", 5, min = 1)),
                #          column(3,
                #                 pickerInput(
                #                   inputId = "select_kanal22", 
                #                   label = "Kanał reklamy PMB w pierwszym kroku",  
                #                   choices = c(sort(unique(kanal_dt[, action_channel])), 'brak udziału w PMB'),
                #                   selected = c(sort(unique(kanal_dt[, action_channel])), 'brak udziału w PMB'),
                #                   options = pickerOptions(
                #                     actionsBox = TRUE, 
                #                     size = 10,
                #                     selectedTextFormat = "count > 3",
                #                     deselectAllText = 'odznacz wszystkie',
                #                     selectAllText = 'zaznacz wszytskie',
                #                     liveSearch = TRUE
                #                   ), 
                #                   multiple = TRUE
                #                 )),
                #          
                #          column(3, actionButton("zastosuj4" , "Zastosuj", 
                #                                 style="color: #191970; background-color: #e0a031;border-color: #191970;")),
                #          
                #          # 
                #          DT::dataTableOutput("tabelka"))),
                
                column(12,
                       
                       fluidRow(
                         h3("Tabela - przecięcia"),
                         
                         
                         column(3,
                                pickerInput(
                                  inputId = "select_agregacja", 
                                  label = "Wybór agregacji danych",  
                                  choices = list('Miesiąc' = 'data_msc',
                                                 'Kanał reklamy' ='action_channel_l',
                                                 'Kraj' = 'campaign_market',
                                                 'Grupa produktowa' = 'campaign_product_group',
                                                 'Kanał reklamy PMB' = 'action_channel',
                                                 'Typ akcji' = 'action_type'),
                                  selected = c('campaign_market', 'campaign_product_group', 'action_channel'),
                                  options = pickerOptions(
                                    actionsBox = TRUE, 
                                    size = 10,
                                    selectedTextFormat = "count > 3",
                                    deselectAllText = 'odznacz wszystkie',
                                    selectAllText = 'zaznacz wszytskie',
                                    liveSearch = TRUE
                                  ), 
                                  multiple = TRUE
                                )),
                         column(3,
                                actionButton("zastosuj5" , "Zastosuj", 
                                             style="color: #191970; background-color: #e0a031;border-color: #191970;")),
                         
                         # 
                         DT::dataTableOutput("tabelka2"),
                         
                         column(12, downloadLink('download_bez_dzialan_EMAIL', 'Pobierz id zamówień bez działań z kanału reklamy EMAIL')),
                         column(12, downloadLink('download_bez_dzialan_VMS', 'Pobierz id zamówień bez działań z kanału reklamy VMS')),
                         column(12, downloadLink('download_bez_dzialan_SMS', 'Pobierz id zamówień bez działań z kanału reklamy SMS')),
                         column(12, downloadLink('download_bez_dzialan_INSERT', 'Pobierz id zamówień bez działań z kanału reklamy INSERT')),
                         column(12, downloadLink('download_bez_dzialan_TEL', 'Pobierz id zamówień bez działań z kanału reklamy TEL')),
                         column(12, downloadLink('download_bez_dzialan_POST', 'Pobierz id zamówień bez działań z kanału reklamy POST')),
                         column(12, downloadLink('download_bez_dzialan', 'Pobierz id zamówień bez działań w ani jednym kanale reklamy'))
                         
                         
                       ))
                
                
                
                
                
                
                
                
                
              ))
      
      
      
      
      
      
      )))






########################################################## SERVER ####################################################################
server <- function(input, output, session) {
  
  passy <- "/home/analizy_crm/dostepy_analizy_crm/pass_analizy_crm_host_anr1"
  #ewa 
  #passy <- "~/R/cred_anr" 
  
  
  observeEvent(input$getLogin, {
    # jeśli js przechwycił login (warunek length(login) > 0) i nie testuję kodu aplikacji z poziomu rstudio (warunek login != 'developer')
    login <- input$getLogin
    
    if(length(login) > 0 && login != 'developer'){
      # jeśli aplikacja jest chronione - to definiujemy w global.R w zmiennej PROTECTED
      if(PROTECTED){
        # sprawdzam czy dana osoba ma wjazd na aplikacje
        access <- accessVerify2(appName = APP_NAME, login = login, host = "anr1.infr.co", db_name = "shiny",
                               table_name = "accesses", creds = passy)
        # jeśli brak dostępu to wyskakuje alert i przekierowanie na kreator zadań w JIRA o dostęp do tej aplikacji
        if(!access){
          jiraLink <- createJiraLink2(appName = APP_NAME, login = login, project = "arms")
          shinyjs::runjs(code = "alert('You dont have access to this app. Click OK to create Jira task.');")
          shinyjs::runjs(code = sprintf("$(location).attr('href', '%s');", jiraLink))
          return(NULL)
        }
      }
      # a jeśli aplikacja nie jest chroniona lub jest ale osoba przeszła weryfikację to zapisujemy wizyte tej osoby na bazie
      addVisits2(appName = APP_NAME, login = login, host = "anr1.infr.co", db_name = "shiny", table_name = "visits", 
                 creds = passy)
      
    }
  })
  
  
  observeEvent(input$login, {
    if(input$login %in% c('sradziszewska', 'golatm', 'boczkowskim','nowakkr',
                          'szymaniake', 'cagaraa', 'pawlowskij',
                          'kisielinskih','gaszczk','kprus','rdzanowskim',
                           'jrodzen', 
                           'szczesnae', 'zeganl', 'kowalczyko', 
                           'ihorbaj', 'niedzielakd'
    )){
      
      
      shinyjs::show(selector = '.sidebar-menu li a[data-value=tab1]')
      shinyjs::show(selector = '.sidebar-menu li a[data-value=tab2]')
      
    }else{ 
      
      shinyjs::hide(selector = '.sidebar-menu li a[data-value=tab1]')
      shinyjs::hide(selector = '.sidebar-menu li a[data-value=tab2]')
    }
  })
  
  
  
  
  ############################################################
  
  
  
  
  
  
  
  
  observeEvent( c(input$zastosuj, input$zastosuj2, input$zastosuj3, input$zastosuj4, input$zastosuj5), {
    
    shinyjs::show(selector = '.progress-group:has(#progres_bar)')
    
    updateProgressBar(session = session, id = "progres_bar", value = 2, title = "Przeprowadzam analizę")
    
    if(input$select_kalka == 'nie'){
      shinyjs::hide(id = "SMS_legenda_o", anim = TRUE)
      shinyjs::hide(id = "EMAIL_legenda_o", anim = TRUE)
      shinyjs::hide(id = "POST_legenda_o", anim = TRUE)
      shinyjs::hide(id = "INSERT_legenda_o", anim = TRUE)
      shinyjs::hide(id = "VMS_legenda_o", anim = TRUE)
      shinyjs::hide(id = "TEL_legenda_o", anim = TRUE)
    }else{
      shinyjs::show(id = "SMS_legenda_o", anim = TRUE)
      shinyjs::show(id = "EMAIL_legenda_o", anim = TRUE)
      shinyjs::show(id = "POST_legenda_o", anim = TRUE)
      shinyjs::show(id = "INSERT_legenda_o", anim = TRUE)
      shinyjs::show(id = "VMS_legenda_o", anim = TRUE)
      shinyjs::show(id = "TEL_legenda_o", anim = TRUE)
    }
    
    
    
    # billy <- if(is.null(input$select_grupa))  {
    #   TRUE
    # } else {
    #   x <- paste0(" campaign_product_group %like% '\\\\<",input$select_grupa,"\\\\>' ", collapse =  "|")
    #   paste0("(", x, ")") 
    # }
    # 
    # 
    # 
    # billy2 <- if(is.null(input$select_kanal_l))  {
    #   TRUE
    # } else {
    #   x <- paste0(" action_channel_l %like% '\\\\<",input$select_kanal_l,"\\\\>' ", collapse =  "|")
    #   paste0("(", x, ")") 
    # }
    # 
    # 
    # billy3 <- if(is.null(input$select_kraj))  {
    #   TRUE
    # } else {
    #   x <- paste0(" campaign_market %like% '\\\\<",input$select_kraj,"\\\\>' ", collapse =  "|")
    #   paste0("(", x, ")") 
    # }
    
    
    
    
    
    # dane2 <- dane_baza[
    #   
    #   typ_sciezki %in% input$select_typ_sciezki 
    #   # RODO %in% input$select_rodo &
    #   # has_address %in% input$select_adres &
    #   # is_valid %in% input$select_email &
    #   # is_mobile_phone %in% input$select_komorka
    #   
    #   , ]
    
    dane2 <- dane_baza
    
    if(input$select_typ_leada == "kontakt"){
      
      dane2 <- dane2[czy_kontakt == 1 ,]
      dane2[, czy_kontakt := NULL]
      
    }else{
      
      dane2 <- dane2[czy_kontakt == 0 ,]
      dane2[, czy_kontakt := NULL]
      
    }
    
    
    
    updateProgressBar(session = session, id = "progres_bar", value = 25, title = "Przeprowadzam analizę")
    
    
    dane2 <- dane2[data >=  as.numeric(gsub('-', '', min(input$select_daty))), ]
    
    
    
    updateProgressBar(session = session, id = "progres_bar", value = 30, title = "Przeprowadzam analizę")
    
    
    dane2 <- dane2[data <=  as.numeric(gsub('-', '', max(input$select_daty))), ]
    
    
    updateProgressBar(session = session, id = "progres_bar", value = 33, title = "Przeprowadzam analizę")
    
    if(length(input$select_grupa) != length(unique(grupy_dt[, product_group]))){
      
      dane2 <- dane2[campaign_product_group %in% input$select_grupa , ]
    }
    
    
    updateProgressBar(session = session, id = "progres_bar", value = 35, title = "Przeprowadzam analizę")
    
    if(length(input$select_kanal_l) != length(kanal_l[, action_channel])){
      
      dane2 <- dane2[action_channel_l %in% input$select_kanal_l, ]
    }
    
    
    if(length(input$select_kraj) != length(kraj_l[, campaign_market])){
      
      dane2 <- dane2[campaign_market %in% input$select_kraj , ]
    }
    
    
    
    
    
    
    updateProgressBar(session = session, id = "progres_bar", value = 40, title = "Przeprowadzam analizę")
    
    dane2[, data_msc := paste0(substring(data,1,4), '-', substring(data,5,6))]
    
    
    #te dane są przefiltrowane tylko filtrami z lewej - dotyczącymi zamówienia
    dane2A <- copy(dane2)

    # stara wersja - jak było tylko filtrowanie typu akcji sms
    # dane2A <- dane2A[is.na(action_channel) |
    #                    (action_channel == 'SMS' & action_type %in%  input$select_action_type_sms)
    #                  | action_channel != 'SMS']
    
    # nowa wersja- filtrowanie action_type dla wszystkich kanałów
    dane2A <- dane2A[ action_type %in%  input$select_action_type_kanaly | is.na(action_type) ]
    
    dane2B <- copy(dane2)
    
    
    
    # 
    # dane2 <- dane2[(action_channel %in% input$select_kanal | is.na(action_channel)) &
    #                  (action_type %in% input$select_action_type | is.na(action_type)) , ]
    
    #dane 2  - filtrujemy jeszcze filtrami dotyczącymi akcji PMB - action_channel i action_type
    dane2[!action_channel %in% input$select_kanal , action_channel := NA]
    dane2[ is.na(action_channel), action_type := NA]
    dane2[ is.na(action_channel), action_date := NA]
    dane2[ is.na(action_channel), program := NA]
    dane2[ is.na(action_channel), liczba_dni_od_daty := NA]
    dane2[!action_type %in% input$select_action_type, action_type := NA]
    
    dane2[ is.na(action_type), action_channel := NA]
    dane2[ is.na(action_type), action_date := NA]
    dane2[ is.na(action_type), program := NA]
    dane2[ is.na(action_type), liczba_dni_od_daty := NA]
    
    if(nrow(dane2) == 0){
      # daneeee <- transpose(as.data.table(rep(0,ncol(dane2)) ))
      # colnames(daneeee) <- colnames(dane2)
      # dane2 <- rbind(dane2, daneeee)
      
      output$selected_var <- renderText({ 
        "Nie ma takiego przecięcia."
      })
      
      output$selected_var2 <- renderText({ 
        "Wybierz inne."
      })
      shinyjs::hide(selector = '.progress-group:has(#progres_bar)')
    }else{
      
      output$selected_var <- renderText({ 
        " "})
      output$selected_var2 <- renderText({ 
        " "})
      ## koniec filtrów
      
      #browser()
      agregat <- input$select_agregacja
      
      #do liczenia HIt rate potzrebuje manownika nez tych dwóch agregatów
      agregat <- agregat[agregat != 'action_channel']
      agregat <- agregat[agregat != 'action_type']
      
      
      
      ## hit rate za cały okres
      
      # dane2[, action_channel2 := action_channel]
      # dane2[is.na(action_date), action_channel2 := '-']
      
      
      
      
      
      
      unique(dane2[,  c('client_id', "id", "typ_sciezki", 'data', 'data_msc', "action_channel_l", "campaign_market", "campaign_product_group", "first_product_group","product_count",         
                        "status")]) -> mianownik_msc
      
      
      
      mianownik_msc2 <- mianownik_msc[, .(mianownik = .N), by = mget(agregat)]
      mianownik_msc <- mianownik_msc[, .(mianownik = .N), by = data_msc]
      
      
      unique(dane2[!is.na(action_date),  c('client_id', "id", "typ_sciezki", 'data', 'data_msc', "action_channel_l", "campaign_market", "campaign_product_group", "first_product_group","product_count",         
                                           "status")]) -> licznik_msc
      
      licznik_msc2 <- licznik_msc[, .(licznik = .N), by = mget(agregat)]
      licznik_msc <- licznik_msc[, .(licznik = .N), by = data_msc]
      
      HR_ogolny <- merge(mianownik_msc, licznik_msc, by = 'data_msc', all.x = T)
      
      HR_ogolny[, hit_rate := licznik/mianownik]
      
      if(length(agregat) == 0){
        HR_ogolny2 <- cbind(mianownik_msc2, licznik_msc2) 
      }else{
        HR_ogolny2 <- merge(mianownik_msc2, licznik_msc2, by = agregat, all.x = T)}
      
      HR_ogolny2[, hit_rate := licznik/mianownik]
      
      
      
      
      unique(dane2[!is.na(action_date),  c('client_id', "id", "typ_sciezki", 'data', 'data_msc', "action_channel_l", "campaign_market", "campaign_product_group", "first_product_group","product_count",         
                                           "status", 'action_channel')]) -> licznik_msc_K
      
      
      if('action_channel' %in% input$select_agregacja & 'action_type' %in% input$select_agregacja){
        
        unique(dane2B[ ,  c('client_id', "id", "typ_sciezki", 'data', 'data_msc', "action_channel_l", "campaign_market", "campaign_product_group", "first_product_group","product_count",         
                            "status", 'action_channel', 'action_type')]) -> licznik_msc_K2
      }else{
        
        if('action_channel' %in% input$select_agregacja & !('action_type' %in% input$select_agregacja)){
          
          unique(dane2B[ ,  c('client_id', "id", "typ_sciezki", 'data', 'data_msc', "action_channel_l", "campaign_market", "campaign_product_group", "first_product_group","product_count",         
                              "status", 'action_channel')]) -> licznik_msc_K2
          
        }else{
          
          if(!('action_channel' %in% input$select_agregacja) & 'action_type' %in% input$select_agregacja){
            
            
            
            unique(dane2B[ ,  c('client_id', "id", "typ_sciezki", 'data', 'data_msc', "action_channel_l", "campaign_market", "campaign_product_group", "first_product_group","product_count",         
                                "status", 'action_type')]) -> licznik_msc_K2
          }else{
            
            
            dane2B[, action_channel2 := '+']
            dane2B[is.na(action_date), action_channel2 := '-']
            
            unique(dane2B[ ,  c('client_id', "id", "typ_sciezki", 'data', 'data_msc', "action_channel_l", "campaign_market", "campaign_product_group", "first_product_group","product_count",         
                                "status", "action_channel2")]) -> licznik_msc_K2
          }
        }
      }
      
      if(!'action_type' %in% input$select_agregacja & !'action_channel' %in% input$select_agregacja){
        
        licznik_msc_K2 <- licznik_msc_K2[, .(licznik = .N), by = mget(c(input$select_agregacja, 'action_channel2'))]}
      else{
        licznik_msc_K2 <- licznik_msc_K2[, .(licznik = .N), by = mget(input$select_agregacja)]}
      
      licznik_msc_K <- licznik_msc_K[, .(licznik = .N), by = c('data_msc', 'action_channel')]
      
      HR_ogolny_K <- merge(mianownik_msc, licznik_msc_K, by = 'data_msc', all.x = T)
      
      HR_ogolny_K[, hit_rate := licznik/mianownik]
      
      
      
      #browser()
      if(length(agregat) == 0){
        HR_ogolny_K2 <- cbind(mianownik_msc2, licznik_msc_K2)}else{  
          HR_ogolny_K2 <- merge(mianownik_msc2, licznik_msc_K2, by = agregat, all.x = T)}
      
      HR_ogolny_K2[, hit_rate := licznik/mianownik]
      
      
      #tutaj liczymy dla hit rate dedykowany
      #każdy kanał ma swoje warunki 
      
      unique(dane2[RODO != 1 & is_valid != 0 & has_email == 1,
                   c("data_msc",  "data", "typ_sciezki", "client_id", "id", "action_channel_l", "campaign_product_group", "campaign_market", "first_product_group","product_count", "status")]) -> mianownik_EMAIL_msc
      
      mianownik_EMAIL_msc[, .N , by = mget(agregat)] -> mianownik_EMAIL_msc2
      mianownik_EMAIL_msc[, .N , by = data_msc] -> mianownik_EMAIL_msc
      
      mianownik_EMAIL_msc[, action_channel := 'EMAIL']
      mianownik_EMAIL_msc2[, action_channel := 'EMAIL']
      
      
      
      unique(dane2[RODO != 1 & sms_negative_response != 1 &  is_mobile_phone != 0,  
                   c("data_msc",  "data", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "campaign_market", "first_product_group","product_count", "status")]) -> mianownik_SMS_msc
      
      mianownik_SMS_msc[, .N , by = mget(agregat)] -> mianownik_SMS_msc2
      mianownik_SMS_msc[, .N , by = data_msc] -> mianownik_SMS_msc
      
      mianownik_SMS_msc[, action_channel := 'SMS']
      mianownik_SMS_msc2[, action_channel := 'SMS']
      
      
      unique(dane2[RODO != 1 & has_address == 1,  
                   c("data_msc", "data", "typ_sciezki", "client_id", "id", "action_channel_l", "campaign_product_group", "campaign_market", "first_product_group","product_count", "status")])-> mianownik_POST_msc
      
      mianownik_POST_msc[, .N , by = mget(agregat)] -> mianownik_POST_msc2
      mianownik_POST_msc[, .N , by = data_msc] -> mianownik_POST_msc
      
      mianownik_POST_msc[, action_channel := 'POST']
      mianownik_POST_msc2[, action_channel := 'POST']
      
      
      unique(dane2[RODO != 1 & last_phone_type != 'BAD' ,  
                   c("data_msc", "data", "typ_sciezki", "client_id", "id", "action_channel_l", "campaign_product_group", "campaign_market", "first_product_group","product_count", "status")]) -> mianownik_VMS_msc
      
      mianownik_VMS_msc[, .N , by = mget(agregat)] -> mianownik_VMS_msc2
      mianownik_VMS_msc[, .N , by = data_msc] -> mianownik_VMS_msc
      
      mianownik_VMS_msc[, action_channel := 'VMS']
      mianownik_VMS_msc2[, action_channel := 'VMS']
      
      
      unique(dane2[RODO != 1 & last_phone_type != 'BAD' & vms_error != 1,  
                   c("data_msc", "data", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "campaign_market", "first_product_group","product_count", "status")]) -> mianownik_TEL_msc
      
      mianownik_TEL_msc[, .N , by = mget(agregat)] -> mianownik_TEL_msc2
      mianownik_TEL_msc[, .N , by = data_msc] -> mianownik_TEL_msc
      
      mianownik_TEL_msc[, action_channel := 'TEL']
      mianownik_TEL_msc2[, action_channel := 'TEL']
      
      
      unique(dane2[,
                   c("data_msc", "data", "typ_sciezki", "client_id", "id", "action_channel_l", "campaign_product_group", "campaign_market", "first_product_group","product_count", "status")]) -> mianownik_INSERT_msc
      
      mianownik_INSERT_msc[, .N , by = mget(agregat)] -> mianownik_INSERT_msc2
      mianownik_INSERT_msc[, .N , by = data_msc] -> mianownik_INSERT_msc
      
      mianownik_INSERT_msc[, action_channel := 'INSERT']
      mianownik_INSERT_msc2[, action_channel := 'INSERT']
      
      mianownik_msc_kanaly <- rbind(mianownik_INSERT_msc, mianownik_EMAIL_msc, mianownik_SMS_msc, mianownik_TEL_msc, mianownik_VMS_msc, mianownik_POST_msc)
      
      mianownik_msc_kanaly2 <- rbind(mianownik_INSERT_msc2, mianownik_EMAIL_msc2, mianownik_SMS_msc2, mianownik_TEL_msc2, mianownik_VMS_msc2, mianownik_POST_msc2)
      
      
      HR_ogolny_K <- merge(HR_ogolny_K, mianownik_msc_kanaly, by = c("data_msc", "action_channel"), all.x = T)
      
      HR_ogolny_K[, hit_rate2 := licznik/N]
      
      
      agr <- input$select_agregacja
      agr2 <- agr[agr != 'action_type']
      agr2[agr2 == "action_channel2"] <- "action_channel"
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "action_channel2"] <- "action_channel"
      
      
      if('action_channel' %in% input$select_agregacja){
        
        HR_ogolny_K2 <- merge(HR_ogolny_K2, mianownik_msc_kanaly2, by = agr2, all.x = T)
        
        
        HR_ogolny_K2[, hit_rate2 := licznik/N]}else{
          
          
          HR_ogolny_K2[, hit_rate2 := hit_rate]   
        }
      
      
      ##
      
      ## rozkald dł ściezek
      unique(dane2[,  c("data", "data_min", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "first_product_group","product_count",         
                        "status", "dl_sciezki")]) -> rozkald_dl_sciezek
      
      rozkald_dl_sciezek <- rozkald_dl_sciezek[, .(ile = .N), by = 'dl_sciezki']
      
      rozkald_dl_sciezek[, total := sum(ile)]
      rozkald_dl_sciezek[, udzial := ile/total]
      
      updateProgressBar(session = session, id = "progres_bar", value = 50, title = "Przeprowadzam analizę")
      
      #obliczenie mianownika dla hit rate
      
      
      
      nrow(unique(dane2[,  c('client_id', 'data')])) -> mianownik
      
      
      nrow(unique(dane2[RODO != 1 & is_valid != 0 & has_email == 1,  
                        c("data", "data_min", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "first_product_group","product_count", "status")])) -> mianownik_EMAIL
      
      nrow(unique(dane2[RODO != 1 & sms_negative_response != 1 &  is_mobile_phone != 0,  
                        c("data", "data_min", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "first_product_group","product_count", "status")])) -> mianownik_SMS
      
      nrow(unique(dane2[RODO != 1 & has_address == 1,  
                        c("data", "data_min", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "first_product_group","product_count", "status")])) -> mianownik_POST
      
      nrow(unique(dane2[RODO != 1 & last_phone_type != 'BAD',
                        c("data", "data_min", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "first_product_group","product_count", "status")])) -> mianownik_VMS
      
      nrow(unique(dane2[RODO != 1 & last_phone_type != 'BAD' & vms_error != 1,  
                        c("data", "data_min", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "first_product_group","product_count", "status")])) -> mianownik_TEL
      
      nrow(unique(dane2[,  
                        c("data", "data_min", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "first_product_group","product_count", "status")])) -> mianownik_INSERT
      
      
      ## licznik w zależności od agregacji / uwaga moze byc filtorwanie kanałów rekalmy dla wskaznika ogolnego
      
      unique(dane2[,  c("data", "data_min", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "first_product_group","product_count", "status", "liczba_dni_od_daty")]) -> licznik_ogolny
      
      licznik_ogolny <- licznik_ogolny[, .(ile = .N) , by = 'liczba_dni_od_daty']
      
      
      unique(dane2[,  c("data", "data_min", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "first_product_group","product_count", "status", "liczba_dni_od_daty", "action_channel")]) -> licznik_kanal
      
      licznik_kanal <- licznik_kanal[, .(ile = .N) , by = c('liczba_dni_od_daty', "action_channel")]
      
      
      unique(dane2A[,  c("data", "data_min", "typ_sciezki", "client_id","id", "action_channel_l", "campaign_product_group", "first_product_group","product_count", "status", "liczba_dni_od_daty", "action_channel")]) -> licznik_kanal_A
      
      
      licznik_kanal_A <- licznik_kanal_A[, .(ile = .N) , by = c('liczba_dni_od_daty', "action_channel")]
      
      
      
      
      
      ##################################################
      
      unique(dane2A[,  c("data", "data_min", "typ_sciezki", "action_channel_l", "client_id","id", "campaign_product_group", "first_product_group","product_count",         
                         "status", "liczba_dni_od_daty", "action_channel", "action_type")]) -> licznik_szcz
      
      licznik_szcz <- licznik_szcz[, .(ile = .N) , by = c('liczba_dni_od_daty', "action_channel", "action_type")]
      
      
      
      
      updateProgressBar(session = session, id = "progres_bar", value = 75, title = "Przeprowadzam analizę")
      #####
      
      
      licznik_ogolny <- cbind(licznik_ogolny, mianownik)
      licznik_ogolny[, hit_rate := ile/mianownik]
      
      licznik_kanal <- cbind(licznik_kanal, mianownik)
      licznik_kanal[, hit_rate := ile/mianownik]
      licznik_kanal <- licznik_kanal[!is.na(action_channel), ]
      
      
      licznik_kanal_A <- cbind(licznik_kanal_A, mianownik)
      licznik_kanal_A[, hit_rate := ile/mianownik]
      licznik_kanal_A <- licznik_kanal_A[!is.na(action_channel), ]
      
      
      licznik_szcz <- cbind(licznik_szcz, mianownik)
      licznik_szcz[, hit_rate := ile/mianownik]
      licznik_szcz <- licznik_szcz[!is.na(action_channel), ]
      
      licznik_szcz <- cbind(licznik_szcz, mianownik_SMS)
      licznik_szcz <- cbind(licznik_szcz, mianownik_POST)
      licznik_szcz <- cbind(licznik_szcz, mianownik_EMAIL)
      licznik_szcz <- cbind(licznik_szcz, mianownik_INSERT)
      licznik_szcz <- cbind(licznik_szcz, mianownik_TEL)
      licznik_szcz <- cbind(licznik_szcz, mianownik_VMS)
      
      
      
      licznik_kanal_A <- cbind(licznik_kanal_A, mianownik_SMS)
      licznik_kanal_A <- cbind(licznik_kanal_A, mianownik_POST)
      licznik_kanal_A <- cbind(licznik_kanal_A, mianownik_EMAIL)
      licznik_kanal_A <- cbind(licznik_kanal_A, mianownik_INSERT)
      licznik_kanal_A <- cbind(licznik_kanal_A, mianownik_TEL)
      licznik_kanal_A <- cbind(licznik_kanal_A, mianownik_VMS)
      
      
      # licznik_ogolny <- licznik_ogolny[liczba_dni_od_daty >=  min(input$select_liczba_dni) & liczba_dni_od_daty <=  max(input$select_liczba_dni), ]
      # 
      # licznik_kanal <- licznik_kanal[liczba_dni_od_daty >=  min(input$select_liczba_dni) & liczba_dni_od_daty <=  max(input$select_liczba_dni), ]
      # 
      # licznik_kanal_A <- licznik_kanal_A[liczba_dni_od_daty >=  min(input$select_liczba_dni) & liczba_dni_od_daty <=  max(input$select_liczba_dni), ]
      # 
      # licznik_szcz <- licznik_szcz[liczba_dni_od_daty >=  min(input$select_liczba_dni) & liczba_dni_od_daty <=  max(input$select_liczba_dni), ]
      
      
      
      
      
      
      updateProgressBar(session = session, id = "progres_bar", value = 85, title = "Przeprowadzam analizę")
      
      
      output$plot_01 <- plotly::renderPlotly({
        
        zz <- ggplot(HR_ogolny, aes(x = data_msc, 
                                    y = hit_rate, 
                                    
                                    label = paste0(round(HR_ogolny$hit_rate*100,2), ' %'),
                                    text = paste('Miesiąc: ', HR_ogolny$data_msc, '<br>',
                                                 'Hit Rate:', paste0(round(HR_ogolny$hit_rate*100,2), ' %'), '<br>',
                                                 'Liczba zamówień:', format(HR_ogolny$mianownik, big.mark = ' '), '<br>')
                                    
                                    
                                    
                                    
                                    
                                    
        )) + #scale_fill_gradient(low = "green", high = "red")+
          geom_bar(stat = 'identity', fill = '#80bebf', color = '#225152') +
          
          geom_text(size = 3, position = position_stack(vjust = 0.5), size=15, check_overlap = T, color = '#225152')+
          
          theme_classic()+
          ggthemes::theme_wsj()+ 
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
          xlab("") + ylab("Hit Rate") +
          scale_y_continuous(labels = scales::percent) 
        
        #
        plotly::ggplotly(zz,tooltip = c("text")) %>% 
          plotly::layout(paper_bgcolor='#ebeef2', plot_bgcolor='#ebeef2')
        
        
      })
      
      
      output$plot_02 <- plotly::renderPlotly({
        
        if(input$select_typ_hit == 'ogólny'){
          
          zz <- ggplot(HR_ogolny_K, aes(x = data_msc, 
                                        y = hit_rate,
                                        fill = action_channel,
                                        label ='',
                                        text = paste('Miesiąc: ', HR_ogolny_K$data_msc, '<br>',
                                                     'Kanał reklamy PMB: ', HR_ogolny_K$action_channel, '<br>',
                                                     'Liczba zamówień:', format(HR_ogolny_K$mianownik, big.mark = " "), '<br>',
                                                     'Liczba zamówień dedykowanych dla kanału:', format(HR_ogolny_K$N, big.mark = " "), '<br>',
                                                     'Hit Rate:', paste0(round(HR_ogolny_K$hit_rate*100,2), ' %'), '<br>',
                                                     'Hit Rate dedykowanych dla kanału:', paste0(round(HR_ogolny_K$hit_rate2*100,2), ' %'), '<br>'
                                        )
                                        
                                        
                                        
                                        
                                        
                                        
          )) + #scale_fill_gradient(low = "green", high = "red")+
            geom_bar(stat="identity", position="dodge") +
            
            geom_text(size = 3, position = position_stack(vjust = 0.5), size=10, check_overlap = T)+
            
            theme_classic()+
            ggthemes::theme_wsj()+ 
            
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
            theme(legend.key = element_rect(fill = "#ebeef2")) +
            theme(legend.background = element_rect(fill="#ebeef2"))+
            theme(legend.title=element_blank()) +
            xlab("") + ylab("Hit Rate") +
            scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
            scale_fill_brewer(palette="Dark2")
          
          plotly::ggplotly(zz,tooltip = c("text")) %>% 
            plotly::layout(paper_bgcolor='#ebeef2', plot_bgcolor='#ebeef2')
        }else{
          
          
          zz <- ggplot(HR_ogolny_K, aes(x = data_msc, 
                                        y = hit_rate2,
                                        fill = action_channel,
                                        label ='',
                                        text = paste('Miesiąc: ', HR_ogolny_K$data_msc, '<br>',
                                                     'Kanał reklamy PMB: ', HR_ogolny_K$action_channel, '<br>',
                                                     'Liczba zamówień:', format(HR_ogolny_K$mianownik, big.mark = " "), '<br>',
                                                     'Liczba zamówień dedykowanych dla kanału:', format(HR_ogolny_K$N, big.mark = " "), '<br>',
                                                     'Hit Rate:', paste0(round(HR_ogolny_K$hit_rate*100,2), ' %'), '<br>',
                                                     'Hit Rate dedykowanych dla kanału:', paste0(round(HR_ogolny_K$hit_rate2*100,2), ' %'), '<br>'
                                        )
                                        
                                        
                                        
                                        
                                        
                                        
          )) + #scale_fill_gradient(low = "green", high = "red")+
            geom_bar(stat="identity", position="dodge") +
            
            geom_text(size = 3, position = position_stack(vjust = 0.5), size=10, check_overlap = T)+
            
            theme_classic()+
            ggthemes::theme_wsj()+ 
            
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
            theme(legend.key = element_rect(fill = "#ebeef2")) +
            theme(legend.background = element_rect(fill="#ebeef2"))+
            theme(legend.title=element_blank()) +
            xlab("") + ylab("Hit Rate") +
            scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
            scale_fill_brewer(palette="Dark2")
          
          plotly::ggplotly(zz,tooltip = c("text")) %>% 
            plotly::layout(paper_bgcolor='#ebeef2', plot_bgcolor='#ebeef2')
        }
        
        
      })
      
      
      
      
      
      output$plot_1 <- plotly::renderPlotly({
        
        zz <- ggplot(licznik_ogolny, aes(x = liczba_dni_od_daty, 
                                         y = hit_rate, 
                                         
                                         label = '',
                                         text = paste('Liczba dni od początku: ', licznik_ogolny$liczba_dni_od_daty, '<br>',
                                                      'Hit Rate:', paste0(round(licznik_ogolny$hit_rate*100,2), ' %'), '<br>',
                                                      'Liczba zamówień:', format(licznik_ogolny$mianownik, big.mark = ' '), '<br>')
                                         
                                         
                                         
                                         
                                         
                                         
        )) + #scale_fill_gradient(low = "green", high = "red")+
          geom_bar(stat = 'identity', fill = '#003366', color = '#add8e6') +
          
          geom_text(size = 3, position = position_stack(vjust = 0.5), size=10, check_overlap = T)+
          
          theme_classic()+
          ggthemes::theme_wsj()+ 
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
          xlab("") + ylab("Hit Rate") +
          scale_y_continuous(labels = scales::percent) 
        #
        plotly::ggplotly(zz,tooltip = c("text")) %>% 
          plotly::layout(paper_bgcolor='#ebeef2', plot_bgcolor='#ebeef2')%>%
          plotly::layout(
            xaxis = list(
              dtick =5, 
              tick0 = 0,
              tickmode = "linear"))
        
      })
      
      
      output$plot_2 <- plotly::renderPlotly({
        
        zz <- ggplot(licznik_kanal, aes(x = liczba_dni_od_daty, 
                                        y = hit_rate,
                                        fill = action_channel,
                                        label ='',
                                        text = paste('Liczba dni od początku : ', licznik_kanal$liczba_dni_od_daty, '<br>',
                                                     'Kanał reklamy PMB : ', licznik_kanal$action_channel, '<br>',
                                                     'Hit Rate :', paste0(round(licznik_kanal$hit_rate*100,2), ' %'), '<br>')
                                        
                                        
                                        
                                        
                                        
                                        
        )) + #scale_fill_gradient(low = "green", high = "red")+
          geom_bar(stat="identity", position="dodge") +
          
          geom_text(size = 3, position = position_stack(vjust = 0.5), size=10, check_overlap = T)+
          
          theme_classic()+
          ggthemes::theme_wsj()+ 
          
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
          theme(legend.key = element_rect(fill = "#ebeef2")) +
          theme(legend.background = element_rect(fill="#ebeef2"))+
          theme(legend.title=element_blank()) +
          xlab("") + ylab("Hit Rate") +
          scale_y_continuous(labels = scales::percent) 
        
        plotly::ggplotly(zz,tooltip = c("text")) %>% 
          plotly::layout(paper_bgcolor='#ebeef2', plot_bgcolor='#ebeef2')%>%
          plotly::layout(
            xaxis = list(
              dtick =5, 
              tick0 = 0,
              tickmode = "linear"))
        
      })
      
      
      
      
      colnames(licznik_szcz) <- c("Liczba dni od daty startu", 
                                  "action_channel", 
                                  "Typ" ,
                                  "ile", 
                                  "mianownik", 
                                  "Hit Rate", 
                                  "mianownik_SMS", 
                                  "mianownik_POST", 
                                  "mianownik_EMAIL" ,  
                                  "mianownik_INSERT", 
                                  "mianownik_TEL", 
                                  "mianownik_VMS")
      
      
      colnames(licznik_kanal_A) <- c("Liczba dni od daty startu", "action_channel",  "ile", "mianownik", "Hit Rate", "mianownik_SMS", 
                                     "mianownik_POST", 
                                     "mianownik_EMAIL" ,  
                                     "mianownik_INSERT", 
                                     "mianownik_TEL", 
                                     "mianownik_VMS")
      
      
      licznik_szcz[, `Hit Rate` := `Hit Rate`*100]
      licznik_kanal_A[, `Hit Rate` := `Hit Rate`*100] 
      
      licznik_szcz[, `Hit Rate SMS` := ile/mianownik_SMS *100]
      licznik_szcz[, `Hit Rate INSERT` := ile/mianownik_INSERT *100]
      licznik_szcz[, `Hit Rate POST` := ile/mianownik_POST *100]
      licznik_szcz[, `Hit Rate VMS` := ile/mianownik_VMS *100]
      licznik_szcz[, `Hit Rate EMAIL` := ile/mianownik_EMAIL *100]
      licznik_szcz[, `Hit Rate TEL` := ile/mianownik_TEL *100]
      
      if(input$select_typ_kanalu_na_wykresie == "tak"){
        dane <- licznik_szcz
      }else{
        
        dane <- licznik_kanal_A
        dane[, Typ := 'wszystkie akcje']
        dane[, `Hit Rate SMS` := ile/mianownik_SMS *100]
        dane[, `Hit Rate INSERT` := ile/mianownik_INSERT *100]
        dane[, `Hit Rate POST` := ile/mianownik_POST *100]
        dane[, `Hit Rate VMS` := ile/mianownik_VMS *100]
        dane[, `Hit Rate EMAIL` := ile/mianownik_EMAIL *100]
        dane[, `Hit Rate TEL` := ile/mianownik_TEL *100]
        
      }
      
      
      
      
      
      ##
      
      czy_kalka <- input$select_kalka
      
      
      
      
      if(input$select_typ_hit_rate == 'ogólny'){
        
        output$plot_kanaly_SMS <- plotly::renderPlotly({
          
          max(dane[action_channel == 'SMS', `Hit Rate`]) + 1 -> ppp
          
          if(czy_kalka == "tak"){
            
            
            
            plotly::plot_ly(dane[action_channel == 'SMS', ], x = ~`Liczba dni od daty startu`, y= ~`Hit Rate`, 
                            color = ~Typ,
                            text = ~paste0(
                              'Liczba dni od daty startu: <b>', dane[action_channel == 'SMS',`Liczba dni od daty startu`],
                              '</b><br>',
                              'Liczba zamówień: <b>', format(dane[action_channel == 'SMS', mianownik], big.mark = ' '),
                              '</b><br>',
                              'Liczba zamówień dedykowanych dla kanału: <b>', format(dane[action_channel == 'SMS', mianownik_SMS], big.mark = ' '),
                              '</b><br>',
                              'Liczba zamówień z działaniem: <b>', format(dane[action_channel == 'SMS', ile], big.mark = ' '),
                              '</b><br>',
                              'Hit Rate ogólny: <b>', paste0(round(dane[action_channel == 'SMS',`Hit Rate`],2), '%'),'</b><br>',
                              'Hit Rate dedykoway dla kanału: <b>', paste0(round(dane[action_channel == 'SMS',`Hit Rate SMS`],2), '%'),'</b><br>',
                              'Typ: <b>', dane[action_channel == 'SMS', Typ],
                              
                              
                              '</b>'
                            ),
                            hoverinfo = 'text',
                            type = 'bar') %>%  plotly::layout(
                              xaxis = list(
                                dtick =5, 
                                tick0 = 0,
                                tickmode = "linear")) %>%
              plotly::layout(paper_bgcolor='#ebeef2', plot_bgcolor='#ebeef2') %>% 
              plotly::layout(shapes = list(
                list(
                  type = "rect",
                  fillcolor = "blue",
                  line = list(color = "blue"),
                  opacity = 0.2,
                  x0 = 1,
                  x1 = 1,
                  y0 = 0, y1 = ppp
                ),  
                list(
                  type = "rect",
                  fillcolor = "red",
                  line = list(color = "red"),
                  opacity = 0.2,
                  x0 = 12,
                  x1 = 14,
                  y0 = 0, y1 = ppp
                ),
                
                list(
                  type = "rect",
                  fillcolor = "tan",
                  line = list(color = "tan"),
                  opacity = 0.2,
                  x0 = 6,
                  x1 = 15,
                  y0 = 0, y1 = ppp +1
                ),
                
                list(
                  type = "rect",
                  fillcolor = "orange",
                  line = list(color = "orange"),
                  opacity = 0.2,
                  x0 = 20,
                  x1 = 22,
                  y0 = 0, y1 = ppp
                ),
                
                list(
                  type = "rect",
                  fillcolor = "green",
                  line = list(color = "green"),
                  opacity = 0.2,
                  x0 = 29,
                  x1 = 33,
                  y0 = 0, y1 = ppp
                ),
                
                list(
                  type = "rect",
                  fillcolor = "purple",
                  line = list(color = "purple"),
                  opacity = 0.2,
                  x0 = 44,
                  x1 = 47,
                  y0 = 0, y1 = ppp
                ),
                
                list(
                  type = "rect",
                  fillcolor = "magenta",
                  line = list(color = "magenta"),
                  opacity = 0.2,
                  x0 = 58,
                  x1 = 63,
                  y0 = 0, y1 = ppp
                ),
                list(
                  type = "rect",
                  fillcolor = "silver",
                  line = list(color = "silver"),
                  opacity = 0.2,
                  x0 = 25,
                  x1 = 80,
                  y0 = 0, y1 = ppp +1
                ),
                
                list(
                  type = "rect",
                  fillcolor = "gray",
                  line = list(color = "gray"),
                  opacity = 0.2,
                  x0 = 150,
                  x1 = 210,
                  y0 = 0, y1 = ppp
                ),
                list(
                  type = "rect",
                  fillcolor = "yellow",
                  line = list(color = "yellow"),
                  opacity = 0.2,
                  x0 = 210,
                  x1 = 270,
                  y0 = 0, y1 = ppp 
                )
                
              )) %>%
              plotly::layout(yaxis = list(range = c(0, ppp+1)))%>%
              plotly::layout(yaxis = list(ticksuffix = "%")) %>%
              plotly::layout(xaxis = list(title = 'Liczba dni od daty startu')) %>%
              plotly::layout(yaxis = list(title = 'Hit Rate'))%>%
              plotly::layout(title="SMS - Hit Rate")
          }else{
            
            plotly::plot_ly(dane[action_channel == 'SMS', ], x = ~`Liczba dni od daty startu`, y= ~`Hit Rate`, 
                            color = ~Typ,
                            text = ~paste0(
                              'Liczba dni od daty startu: <b>', dane[action_channel == 'SMS',`Liczba dni od daty startu`],
                              '</b><br>',
                              'Liczba zamówień: <b>', format(dane[action_channel == 'SMS', mianownik], big.mark = ' '),
                              '</b><br>',
                              'Liczba zamówień dedykowanych dla kanału: <b>', format(dane[action_channel == 'SMS', mianownik_SMS], big.mark = ' '),
                              '</b><br>',
                              'Liczba zamówień z działaniem: <b>', format(dane[action_channel == 'SMS', ile], big.mark = ' '),
                              '</b><br>',
                              'Hit Rate ogólny: <b>', paste0(round(dane[action_channel == 'SMS',`Hit Rate`],2), '%'),'</b><br>',
                              'Hit Rate dedykoway dla kanału: <b>', paste0(round(dane[action_channel == 'SMS',`Hit Rate SMS`],2), '%'),'</b><br>',
                              'Typ: <b>', dane[action_channel == 'SMS', Typ],
                              
                              
                              '</b>'
                            ),
                            hoverinfo = 'text',
                            type = 'bar') %>%  plotly::layout(
                              xaxis = list(
                                dtick =5, 
                                tick0 = 0,
                                tickmode = "linear")) %>%
              plotly::layout(paper_bgcolor='#ebeef2', plot_bgcolor='#ebeef2') %>% 
              plotly::layout(yaxis = list(range = c(0, ppp+1)))%>%
              plotly::layout(yaxis = list(ticksuffix = "%")) %>%
              plotly::layout(xaxis = list(title = 'Liczba dni od daty startu')) %>%
              plotly::layout(yaxis = list(title = 'Hit Rate')) %>%
              plotly::layout(title="SMS - Hit Rate")%>%
              plotly::layout(title="SMS - Hit Rate")
          }
          
          
        }) }else{
          
          
          output$plot_kanaly_SMS <- plotly::renderPlotly({
            
            
            
            max(dane[action_channel == 'SMS', `Hit Rate SMS`]) + 1 -> ppp
            if(czy_kalka == "tak"){
              
              plotly::plot_ly(dane[action_channel == 'SMS', ], x = ~`Liczba dni od daty startu`, y= ~`Hit Rate SMS`, 
                              color = ~Typ,
                              text = ~paste0(
                                'Liczba dni od daty startu: <b>', dane[action_channel == 'SMS',`Liczba dni od daty startu`],
                                '</b><br>',
                                'Liczba zamówień: <b>', format(dane[action_channel == 'SMS', mianownik], big.mark = ' '),
                                '</b><br>',
                                'Liczba zamówień dedykowanych dla kanału: <b>', format(dane[action_channel == 'SMS', mianownik_SMS], big.mark = ' '),
                                '</b><br>',
                                'Liczba zamówień z działaniem: <b>', format(dane[action_channel == 'SMS', ile], big.mark = ' '),
                                '</b><br>',
                                'Hit Rate ogólny: <b>', paste0(round(dane[action_channel == 'SMS',`Hit Rate`],2), '%'),'</b><br>',
                                'Hit Rate dedykoway dla kanału: <b>', paste0(round(dane[action_channel == 'SMS',`Hit Rate SMS`],2), '%'),'</b><br>',
                                'Typ: <b>', dane[action_channel == 'SMS', Typ],
                                
                                
                                '</b>'
                              ),
                              hoverinfo = 'text',
                              type = 'bar') %>%  plotly::layout(
                                xaxis = list(
                                  dtick =5, 
                                  tick0 = 0,
                                  tickmode = "linear")) %>%
                plotly::layout(paper_bgcolor='#ebeef2', plot_bgcolor='#ebeef2') %>% 
                plotly::layout(shapes = list(
                  list(
                    type = "rect",
                    fillcolor = "blue",
                    line = list(color = "blue"),
                    opacity = 0.2,
                    x0 = 1,
                    x1 = 1,
                    y0 = 0, y1 = ppp
                  ),  
                  list(
                    type = "rect",
                    fillcolor = "red",
                    line = list(color = "red"),
                    opacity = 0.2,
                    x0 = 12,
                    x1 = 14,
                    y0 = 0, y1 = ppp
                  ),
                  list(
                    type = "rect",
                    fillcolor = "tan",
                    line = list(color = "tan"),
                    opacity = 0.2,
                    x0 = 6,
                    x1 = 15,
                    y0 = 0, y1 = ppp +1
                  ),
                  
                  list(
                    type = "rect",
                    fillcolor = "orange",
                    line = list(color = "orange"),
                    opacity = 0.2,
                    x0 = 20,
                    x1 = 22,
                    y0 = 0, y1 = ppp
                  ),
                  
                  list(
                    type = "rect",
                    fillcolor = "green",
                    line = list(color = "green"),
                    opacity = 0.2,
                    x0 = 29,
                    x1 = 33,
                    y0 = 0, y1 = ppp
                  ),
                  
                  list(
                    type = "rect",
                    fillcolor = "purple",
                    line = list(color = "purple"),
                    opacity = 0.2,
                    x0 = 44,
                    x1 = 47,
                    y0 = 0, y1 = ppp
                  ),
                  
                  list(
                    type = "rect",
                    fillcolor = "magenta",
                    line = list(color = "magenta"),
                    opacity = 0.2,
                    x0 = 58,
                    x1 = 63,
                    y0 = 0, y1 = ppp
                  ),
                  list(
                    type = "rect",
                    fillcolor = "silver",
                    line = list(color = "silver"),
                    opacity = 0.2,
                    x0 = 25,
                    x1 = 80,
                    y0 = 0, y1 = ppp +1
                  ),
                  
                  list(
                    type = "rect",
                    fillcolor = "gray", #maroon
                    line = list(color = "gray"),
                    opacity = 0.2,
                    x0 = 150,
                    x1 = 210,
                    y0 = 0, y1 = ppp
                  ),
                  list(
                    type = "rect",
                    fillcolor = "yellow",
                    line = list(color = "yellow"),
                    opacity = 0.2,
                    x0 = 210,
                    x1 = 270,
                    y0 = 0, y1 = ppp 
                  )
                  
                ))  %>%
                plotly::layout(yaxis = list(range = c(0, ppp+1)))%>%
                plotly::layout(yaxis = list(ticksuffix = "%")) %>%
                plotly::layout(xaxis = list(title = 'Liczba dni od daty startu')) %>%
                plotly::layout(yaxis = list(title = 'Hit Rate'))%>%
                plotly::layout(title="SMS - Hit Rate")
            }else{
              plotly::plot_ly(dane[action_channel == 'SMS', ], x = ~`Liczba dni od daty startu`, y= ~`Hit Rate SMS`, 
                              color = ~Typ,
                              text = ~paste0(
                                'Liczba dni od daty startu: <b>', dane[action_channel == 'SMS',`Liczba dni od daty startu`],
                                '</b><br>',
                                'Liczba zamówień: <b>', format(dane[action_channel == 'SMS', mianownik], big.mark = ' '),
                                '</b><br>',
                                'Liczba zamówień dedykowanych dla kanału: <b>', format(dane[action_channel == 'SMS', mianownik_SMS], big.mark = ' '),
                                '</b><br>',
                                'Liczba zamówień z działaniem: <b>', format(dane[action_channel == 'SMS', ile], big.mark = ' '),
                                '</b><br>',
                                'Hit Rate ogólny: <b>', paste0(round(dane[action_channel == 'SMS',`Hit Rate`],2), '%'),'</b><br>',
                                'Hit Rate dedykoway dla kanału: <b>', paste0(round(dane[action_channel == 'SMS',`Hit Rate SMS`],2), '%'),'</b><br>',
                                'Typ: <b>', dane[action_channel == 'SMS', Typ],
                                
                                
                                '</b>'
                              ),
                              hoverinfo = 'text',
                              type = 'bar') %>%  plotly::layout(
                                xaxis = list(
                                  dtick =5, 
                                  tick0 = 0,
                                  tickmode = "linear")) %>%
                plotly::layout(paper_bgcolor='#ebeef2', plot_bgcolor='#ebeef2') %>% 
                plotly::layout(yaxis = list(range = c(0, ppp+1)))%>%
                plotly::layout(yaxis = list(ticksuffix = "%")) %>%
                plotly::layout(xaxis = list(title = 'Liczba dni od daty startu')) %>%
                plotly::layout(yaxis = list(title = 'Hit Rate'))
            }
            
            
            
            
            
            
            
            
            
          })
          
          
        }
      
      ##
      
      source(file="server/INSERT.R", local=T) 
      source(file="server/POST.R", local=T) 
      source(file="server/EMAIL.R", local=T) 
      source(file="server/VMS.R", local=T) 
      source(file="server/TEL.R", local=T) 
      
      
      
      
      
      
      
      
      
      # # ####sciezki
      # updateProgressBar(session = session, id = "progres_bar", value = 90, title = "Przeprowadzam analizę")
      # 
      # 
      # 
      # dane3 <- unique(dane2[, c("data", "data_min", "typ_sciezki", "client_id", "action_channel_l", "campaign_product_group", "first_product_group","product_count",
      #                           "status", "liczba_dni_od_daty", "action_channel")])
      # 
      # keycol <-c("data","client_id", "liczba_dni_od_daty", "action_channel")
      # setorderv(dane3, keycol)
      # 
      # dane3[, .(action_channel_pmb = toString(action_channel)), by = c("data",
      #                                                                  "typ_sciezki",
      #                                                                  "client_id",
      #                                                                  "action_channel_l",
      #                                                                  "campaign_product_group",
      #                                                                  "first_product_group",
      #                                                                  "product_count",
      #                                                                  "status")] -> dane4
      # 
      # 
      # 
      # 
      # 
      # dane4[, ile_p := stringr::str_count(action_channel_pmb, ",")]
      # 
      # dane4[,max(ile_p)]->max_col
      # max_col <- max_col + 1
      # 
      # nazwy_col <- c()
      # i=1
      # for(i in 1:max_col){
      #   nazwy_col <- c(nazwy_col, paste0('Krok_', i))
      #   i <- i+1
      # }
      # 
      # tidyr::separate(dane4, action_channel_pmb,
      #                 into = nazwy_col,
      #                 sep = ",") -> dane5
      # 
      # 
      # 
      # kolumny <- c()
      # i=1
      # for(i in 1:input$select_liczba_krokow){
      #   # for(i in 1:4){
      #   kolumny <- c(kolumny, paste0("Krok_", i))
      #   i <- i+1
      #   
      # }
      # 
      # #zabezpieczenie zeby były dostępne wybrane kroki
      # kolumny[kolumny %in% colnames(dane5)] -> kolumny
      # 
      # 
      # 
      # dane5[, .(liczba_sciezek = .N), by = kolumny] -> dane5
      # 
      # 
      # if('brak udziału w PMB' %in% input$select_kanal22){
      #   dane5 <- dane5[Krok_1 %in% input$select_kanal22 | is.na(Krok_1) | Krok_1 == 'NA',]}else{
      #     
      #     dane5 <- dane5[Krok_1 %in% input$select_kanal22,]  
      #   }
      # dane5[Krok_1 == 'NA', Krok_1 := '-']
      # dane5[, ile_total := sum(liczba_sciezek)]
      # 
      # dane5[, `Udział` := liczba_sciezek/ile_total]
      # 
      # dane5[, ile_total := NULL]
      # 
      # 
      # # 
      
      # 
      # 
      # 
      # 
      # 
      # keycol <- colnames(dane5)[colnames(dane5) %like% 'Krok']
      # setorderv(dane5, keycol) -> dane5
      # 
      # i=1
      # for(i in 1:input$select_liczba_krokow){
      #   #for(i in 1:4){
      #   if(! paste0("Krok_", i) %in% colnames(dane5)){
      #     next
      #   }else{
      #     dane5[is.na(get(paste0("Krok_", i))), 
      #           paste0("Krok_", i) := "-"]}
      #   
      # }
      # 
      # colnames(dane5)[colnames(dane5) == 'liczba_sciezek'] <- 'Liczba zamówień'
      
      # output$tabelka <- DT::renderDataTable({
      #   
      #   datatable(dane5,
      #             
      #             rownames=FALSE,
      #             colnames = gsub("_", " ", colnames(dane5)),
      #             options = list(searching = FALSE)
      #   ) %>%
      #     formatRound(columns = 'Liczba zamówień', mark = " ",  digits = 0) %>%
      #     formatPercentage(columns = 'Udział', digits = 0, interval = 2, mark = ",",
      #                      dec.mark = getOption("OutDec"))
      #   
      # })
      
      
      
      
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "action_channel"] <- "Kanał reklamy PMB"
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "action_type"] <- "Typ akcji"
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "campaign_market"] <- "Kraj"
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "campaign_product_group"] <- "Grupa produktowa"
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "mianownik"] <- "Liczba zamówień"
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "licznik"] <- "Liczba zamówień z działaniem"
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "N"] <- "Liczba zamówień dedykowanych"
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "hit_rate"] <- "Hit Rate"
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "hit_rate2"] <- "Hit Rate dedykowany"
      
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "action_channel_l"] <- "Kanał reklamy"
      names(HR_ogolny_K2)[names(HR_ogolny_K2) == "data_msc"] <- "Miesiąc"
      
      
      
      HR_ogolny_K2[,  `Liczba zamówień bez działania` := `Liczba zamówień` - `Liczba zamówień z działaniem`]
      
      if('Kanał reklamy PMB' %in% colnames(HR_ogolny_K2) & 'Typ akcji' %in% colnames(HR_ogolny_K2)){
        HR_ogolny_K2[is.na(`Kanał reklamy PMB`), `Kanał reklamy PMB` := '-']
        HR_ogolny_K2[is.na(`Typ akcji`), `Typ akcji` := '-']
        HR_ogolny_K2[`Kanał reklamy PMB` == '-', `Liczba zamówień bez działania` := `Liczba zamówień z działaniem`]
        HR_ogolny_K2[`Kanał reklamy PMB` == '-', `Liczba zamówień z działaniem` := 0]
        HR_ogolny_K2[`Kanał reklamy PMB` == '-', `Hit Rate dedykowany` := 0]
        HR_ogolny_K2[`Kanał reklamy PMB` == '-', `Hit Rate` := 0]}else{
          if('Kanał reklamy PMB' %in% colnames(HR_ogolny_K2) ){
            HR_ogolny_K2[is.na(`Kanał reklamy PMB`), `Kanał reklamy PMB` := '-']
            HR_ogolny_K2[`Kanał reklamy PMB` == '-', `Liczba zamówień bez działania` := `Liczba zamówień z działaniem`]
            HR_ogolny_K2[`Kanał reklamy PMB` == '-', `Liczba zamówień z działaniem` := 0]
            HR_ogolny_K2[`Kanał reklamy PMB` == '-', `Hit Rate dedykowany` := 0]
            HR_ogolny_K2[`Kanał reklamy PMB` == '-', `Hit Rate` := 0]
          }else{
            
            HR_ogolny_K2[is.na(`Typ akcji`), `Typ akcji` := '-']
            HR_ogolny_K2[`Typ akcji` == '-', `Liczba zamówień bez działania` := `Liczba zamówień z działaniem`]
            HR_ogolny_K2[`Typ akcji` == '-', `Liczba zamówień z działaniem` := 0]
            HR_ogolny_K2[`Typ akcji` == '-', `Hit Rate dedykowany` := 0]
            HR_ogolny_K2[`Typ akcji` == '-', `Hit Rate` := 0]
            
            HR_ogolny_K2[, `Liczba zamówień dedykowanych` := `Liczba zamówień`]}
          
        }
      
      if(!('Liczba zamówień dedykowanych' %in% names(HR_ogolny_K2))){
        
        HR_ogolny_K2[, `Liczba zamówień dedykowanych` := `Liczba zamówień`]
      }
      
      if('Kanał reklamy PMB' %in% colnames(HR_ogolny_K2) ){
        if('+' %in% HR_ogolny_K2[, unique(`Kanał reklamy PMB`)] &  '-' %in% HR_ogolny_K2[, unique(`Kanał reklamy PMB`)] ){
          
          HR_ogolny_K2 <- HR_ogolny_K2[`Kanał reklamy PMB` == '+']
        }}
      
      
      HR_ogolny_K2 <- HR_ogolny_K2[, mget(c(
        c('Miesiąc', 'Kanał reklamy', 'Kraj' , 'Grupa produktowa', "Kanał reklamy PMB", 'Typ akcji')[
          c('Miesiąc', 'Kanał reklamy', 'Kraj' , 'Grupa produktowa', "Kanał reklamy PMB", 'Typ akcji') %in% colnames(HR_ogolny_K2)] , 
        
        "Liczba zamówień"   , "Liczba zamówień dedykowanych",
        "Liczba zamówień z działaniem"  ,
        "Liczba zamówień bez działania",
        "Hit Rate",
        "Hit Rate dedykowany"        ))]
      
      
      if('Kanał reklamy PMB' %in% colnames(HR_ogolny_K2) ){
        if(HR_ogolny_K2[, unique(`Kanał reklamy PMB`)] == '+' & length(HR_ogolny_K2[, unique(`Kanał reklamy PMB`)]) == 1 ){
          
          HR_ogolny_K2[, `Kanał reklamy PMB` := NULL]
        }}
      
      
      output$tabelka2 <- DT::renderDataTable({
        
        datatable(HR_ogolny_K2,
                  
                  filter = 'top',
                  rownames=FALSE,
                  options = list(searching = T, dom = 'Brtip'
                  )
        ) %>%
          formatRound(columns = c('Liczba zamówień', "Liczba zamówień z działaniem", "Liczba zamówień dedykowanych", "Liczba zamówień bez działania") , mark = " ",  digits = 0) %>%
          formatPercentage(columns = c('Hit Rate', 'Hit Rate dedykowany') , digits = 0, interval = 2, mark = ",",
                           dec.mark = getOption("OutDec"))
        
      })
      
      ###
      
      
      
      #################tabela 
      
      
      if(input$select_typ_kanalu_na_wykresie == "nie"){
        
        licznik_kanal_A[, `Hit Rate SMS` := ile/mianownik_SMS]
        dt_SMS <- licznik_kanal_A[action_channel == 'SMS', c('Liczba dni od daty startu', 'mianownik', 'mianownik_SMS', 'ile', 'Hit Rate', 'Hit Rate SMS')]
        if(nrow(dt_SMS) !=0){
          
          
          
          rep(min(unique(dt_SMS$`Liczba dni od daty startu`)):max(unique(dt_SMS$`Liczba dni od daty startu`)))[
            !rep(min(unique(dt_SMS$`Liczba dni od daty startu`)):max(unique(dt_SMS$`Liczba dni od daty startu`))) %in% unique(dt_SMS$`Liczba dni od daty startu`)] -> dni_do_uzupelneinia
          
          dni_do_uzupelneinia <- as.data.table(dni_do_uzupelneinia)
          
          names(dni_do_uzupelneinia) <- "Liczba dni od daty startu"
          
          plyr::rbind.fill(dt_SMS, dni_do_uzupelneinia) -> dt_SMS
          dt_SMS<-as.data.table(dt_SMS)
          
          
          
          #SMS + do 270 dni
          rep(0:270)[!rep(0:270) %in% unique(dt_SMS$`Liczba dni od daty startu`)] -> dni_do_uzupelneinia2
          
          dni_do_uzupelneinia2 <- as.data.table(dni_do_uzupelneinia2)
          
          names(dni_do_uzupelneinia2) <- "Liczba dni od daty startu"
          
          plyr::rbind.fill(dt_SMS, dni_do_uzupelneinia2) -> dt_SMS
          dt_SMS<-as.data.table(dt_SMS)
          
          
          
          dt_SMS[is.na(mianownik), mianownik := unique(dt_SMS[!is.na(mianownik), mianownik])]
          
          dt_SMS[is.na(mianownik_SMS), mianownik_SMS := unique(dt_SMS[!is.na(mianownik_SMS), mianownik_SMS])]
          
          
          
          dt_SMS[is.na(ile), ile := 0]
          
          dt_SMS[is.na(`Hit Rate`), `Hit Rate` := 0]
          
          dt_SMS[is.na(`Hit Rate SMS`), `Hit Rate SMS` := 0]
          
          dt_SMS[, `Liczba zamówień` := as.character(format(mianownik, big.mark = " "))]
          dt_SMS[, `Liczba zamówień dedykowanych dla kanału` := as.character(format(mianownik_SMS, big.mark = " "))]
          dt_SMS[, `Liczba zamówień z działaniem` := as.character(format(ile, big.mark = " "))]
          dt_SMS[, `Hit Rate ogólny` := as.character( paste0(round(`Hit Rate`,2),' %'))]
          dt_SMS[, `Hit Rate dedykowany` := as.character( paste0(round(`Hit Rate SMS`,2),' %'))]
          
          dt_SMS <- dt_SMS[, c("Liczba dni od daty startu", "Liczba zamówień", "Liczba zamówień dedykowanych dla kanału", "Liczba zamówień z działaniem", "Hit Rate ogólny", "Hit Rate dedykowany")]
          
          
          
          dt_SMS <- dt_SMS[order(`Liczba dni od daty startu`)]
          
          data.table(cn = names(dt_SMS), transpose(dt_SMS)) -> dt_SMS2
          
          header.true <- function(df) {
            names(df) <- as.character(unlist(df[1,]))
            df[-1,]
          }
          
          header.true(dt_SMS2) -> dt_SMS3
          
          
          
          as.numeric(colnames(dt_SMS3)[ncol(dt_SMS3)]) -270 -> datkowe_kolumny_SMS
          
          
          
          output$kable_SMS <- function(){
            kableExtra::kable(dt_SMS3,  "html") %>%
              kableExtra::kable_styling(c("striped", "bordered", "hover")) %>%
              kableExtra::column_spec(column = 2:ncol(dt_SMS3), width_min="5em") %>%
              kableExtra::add_header_above(c(" ",
                                             " " = 1,
                                             "SMS QX-SELL" = 1,
                                             " " = 10,
                                             "SMS X-SELL 1" = 3,
                                             " " = 5,
                                             "SMS REAKT 1" = 3,
                                             " " = 6,
                                             "SMS X-SELL 2" = 5,
                                             " " = 10,
                                             "SMS REAKT 2" = 4,
                                             " " =  10,
                                             "SMS REAKT 3" = 6,
                                             " " = 86,
                                             "SMS AS 1 / SMS FU" = 61,
                                             "SMS AS 2 / SMS FU" = 60,
                                             " " = datkowe_kolumny_SMS
                                             
                                             
              )) %>%
              kableExtra::add_header_above(c(" ",
                                             " " = 6,
                                             "SMS FU" = 9,
                                             " " = 10,
                                             "SMS WB" = 66,
                                             " " = ncol(dt_SMS3)-92
                                             
              )) %>%
              kableExtra::scroll_box(width = "100%", height = "500px")
            #})
          }}else{
            
            output$kable_SMS <- function() {
              
              aa <- data.table()
              kableExtra::kable(aa,  "html") %>%
                kableExtra::kable_styling(c("striped", "bordered", "hover"), full_width = F) 
              
              
            }
          }
        
      }else{
        
        
        
        dt_SMS <- licznik_szcz[action_channel == 'SMS', c('Liczba dni od daty startu', 'mianownik', 'mianownik_SMS', 'ile', 'Hit Rate', 'Hit Rate SMS', 'Typ')]
        if(nrow(dt_SMS) != 0){
          
          rep(min(unique(dt_SMS$`Liczba dni od daty startu`)):max(unique(dt_SMS$`Liczba dni od daty startu`)))[
            !rep(min(unique(dt_SMS$`Liczba dni od daty startu`)):max(unique(dt_SMS$`Liczba dni od daty startu`))) %in% unique(dt_SMS$`Liczba dni od daty startu`)] -> dni_do_uzupelneinia
          
          dni_do_uzupelneinia <- as.data.table(dni_do_uzupelneinia)
          names(dni_do_uzupelneinia) <- "Liczba dni od daty startu"
          dni_do_uzupelneinia[, pp :='1']
          
          dt_SMS[, unique(Typ)] -> typy_do_uzu
          typy_do_uzu <- as.data.table(typy_do_uzu)
          names(typy_do_uzu) <- 'Typ'
          typy_do_uzu[, pp :='1']
          
          do_uzupelneinia <- merge(dni_do_uzupelneinia, typy_do_uzu, by = 'pp', allow.cartesian = T)
          do_uzupelneinia[, pp := NULL]
          
          plyr::rbind.fill(dt_SMS, do_uzupelneinia) -> dt_SMS
          dt_SMS<-as.data.table(dt_SMS)
          
          #SMS + do 270 dni
          rep(0:270)[!rep(0:270) %in% unique(dt_SMS$`Liczba dni od daty startu`)] -> dni_do_uzupelneinia2
          
          dni_do_uzupelneinia2 <- as.data.table(dni_do_uzupelneinia2)
          
          names(dni_do_uzupelneinia2) <- "Liczba dni od daty startu"
          dni_do_uzupelneinia2[, pp := '1']
          
          do_uzupelneinia2 <- merge(dni_do_uzupelneinia2, typy_do_uzu, by = 'pp', allow.cartesian = T)
          
          plyr::rbind.fill(dt_SMS, dni_do_uzupelneinia2) -> dt_SMS
          dt_SMS<-as.data.table(dt_SMS)
          
          
          
          dt_SMS[is.na(mianownik), mianownik := unique(dt_SMS[!is.na(mianownik), mianownik])]
          
          dt_SMS[is.na(mianownik_SMS), mianownik_SMS := unique(dt_SMS[!is.na(mianownik_SMS), mianownik_SMS])]
          
          
          
          dt_SMS[is.na(ile), ile := 0]
          
          dt_SMS[is.na(`Hit Rate`), `Hit Rate` := 0]
          
          dt_SMS[is.na(`Hit Rate SMS`), `Hit Rate SMS` := 0]
          
          
          dt_SMS[, `Liczba zamówień` := as.character(format(mianownik, big.mark = " "))]
          dt_SMS[, `Liczba zamówień dedykowanych dla kanału` := as.character(format(mianownik_SMS, big.mark = " "))]
          dt_SMS[, `Liczba zamówień z działaniem` := as.character(format(ile, big.mark = " "))]
          dt_SMS[, `Hit Rate ogólny` := as.character( paste0(round(`Hit Rate`,2),' %'))]
          dt_SMS[, `Hit Rate dedykowany` := as.character( paste0(round(`Hit Rate SMS`,2),' %'))]
          
          dt_SMS <- dt_SMS[, c("Liczba dni od daty startu", "Typ", "Liczba zamówień", "Liczba zamówień dedykowanych dla kanału", "Liczba zamówień z działaniem", "Hit Rate ogólny", "Hit Rate dedykowany")]
          
          
          
          dt_SMS <- dt_SMS[order(`Liczba dni od daty startu`)]
          
          dt_SMS[is.na(Typ), Typ := ' - ']
          
          
          dt_SMS_ls <- dt_SMS[, c("Liczba dni od daty startu", "Typ", "Liczba zamówień")]
          dt_SMS_lszk <- dt_SMS[, c("Liczba dni od daty startu", "Typ", "Liczba zamówień z działaniem")]
          dt_SMS_lddk <- dt_SMS[, c("Liczba dni od daty startu", "Typ", "Liczba zamówień dedykowanych dla kanału")]
          dt_SMS_ho <- dt_SMS[, c("Liczba dni od daty startu", "Typ", "Hit Rate ogólny")]
          dt_SMS_hd <- dt_SMS[, c("Liczba dni od daty startu", "Typ", "Hit Rate dedykowany")]
          
          
          
          
          
          dt_SMS_lszk[is.na(`Liczba zamówień z działaniem`), `Liczba zamówień z działaniem` := 0]
          dt_SMS_ho[is.na(`Hit Rate ogólny`), `Hit Rate ogólny` :=0 ]
          dt_SMS_hd[is.na(`Hit Rate dedykowany`), `Hit Rate dedykowany` :=0 ]
          
          
          
          
          reshape(dt_SMS_ls, idvar = "Typ", timevar = "Liczba dni od daty startu", direction = "wide") -> dt_SMS_ls
          reshape(dt_SMS_lszk, idvar = "Typ", timevar = "Liczba dni od daty startu", direction = "wide") -> dt_SMS_lszk
          reshape(dt_SMS_lddk, idvar = "Typ", timevar = "Liczba dni od daty startu", direction = "wide") -> dt_SMS_lddk
          reshape(dt_SMS_ho, idvar = "Typ", timevar = "Liczba dni od daty startu", direction = "wide") -> dt_SMS_ho
          reshape(dt_SMS_hd, idvar = "Typ", timevar = "Liczba dni od daty startu", direction = "wide") -> dt_SMS_hd
          
          
          
          dt_SMS_ls <- dt_SMS_ls[Typ == ' - ', ]
          dt_SMS_lddk <- dt_SMS_lddk[Typ == ' - ', ]
          
          
          
          dt_SMS_ls[is.na(dt_SMS_ls)] <- format(unique(licznik_szcz[action_channel == 'SMS', mianownik]), big.mark = ' ')
          
          dt_SMS_lddk[is.na(dt_SMS_lddk)] <- format(unique(licznik_szcz[action_channel == 'SMS', mianownik_SMS]), big.mark = ' ')
          
          dt_SMS_lszk[is.na(dt_SMS_lszk)] <- 0
          dt_SMS_ho[is.na(dt_SMS_ho)] <- '0 %'
          dt_SMS_hd[is.na(dt_SMS_hd)] <- '0 %'
          
          names(dt_SMS_ls)[names(dt_SMS_ls) %like% "Liczba zamówień."] <- gsub("Liczba zamówień.", "", names(dt_SMS_ls)[names(dt_SMS_ls) %like% "Liczba zamówień."])
          names(dt_SMS_lszk)[names(dt_SMS_lszk) %like% "Liczba zamówień z działaniem."] <- gsub("Liczba zamówień z działaniem.", "", names(dt_SMS_lszk)[names(dt_SMS_lszk) %like% "Liczba zamówień z działaniem."])
          names(dt_SMS_lddk)[names(dt_SMS_lddk) %like% "Liczba zamówień dedykowanych dla kanału."] <- gsub("Liczba zamówień dedykowanych dla kanału.", "", names(dt_SMS_lddk)[names(dt_SMS_lddk) %like% "Liczba zamówień dedykowanych dla kanału."])
          names(dt_SMS_ho)[names(dt_SMS_ho) %like% "Hit Rate ogólny."] <- gsub("Hit Rate ogólny.", "", names(dt_SMS_ho)[names(dt_SMS_ho) %like% "Hit Rate ogólny."])
          names(dt_SMS_hd)[names(dt_SMS_hd) %like% "Hit Rate dedykowany."] <- gsub("Hit Rate dedykowany.", "", names(dt_SMS_hd)[names(dt_SMS_hd) %like% "Hit Rate dedykowany."])
          
          
          dt_SMS_lszk <- dt_SMS_lszk[ Typ != ' - ']
          dt_SMS_ho <- dt_SMS_ho[ Typ != ' - ',]
          dt_SMS_hd <- dt_SMS_hd[ Typ != ' - ',]
          
          
          # add_column(dt_SMS_ls, d = 4:8, .after = "b")
          
          dt_SMS_ls <- cbind( Metryka = 'Liczba zamówień', dt_SMS_ls)
          dt_SMS_lszk <- cbind( Metryka = 'Liczba zamówień z działaniem', dt_SMS_lszk)
          dt_SMS_lddk <- cbind( Metryka = 'Liczba zamówień dedykowanych dla kanału', dt_SMS_lddk)
          dt_SMS_ho <- cbind( Metryka = 'Hit Rate ogólny', dt_SMS_ho)
          dt_SMS_hd <- cbind( Metryka = 'Hit Rate dedykowany', dt_SMS_hd)
          
          
          
          dt_SMS_ALL_info <- rbind(dt_SMS_ls, dt_SMS_lddk, dt_SMS_lszk, dt_SMS_ho, dt_SMS_hd)
          
          
          
          as.numeric(colnames(dt_SMS_ALL_info)[ncol(dt_SMS_ALL_info)]) -270 -> datkowe_kolumny_SMS
          
          
          #browser()
          output$kable_SMS <- function() {
            kableExtra::kable(dt_SMS_ALL_info,  "html") %>%
              kableExtra::kable_styling(c("striped", "bordered", "hover")) %>%
              kableExtra::column_spec(column = 2:ncol(dt_SMS_ALL_info), width_min="5em") %>%
              kableExtra::add_header_above(c(" ",
                                             " " = 2,
                                             "SMS QX-SELL" = 1,
                                             " " = 10,
                                             "SMS X-SELL 1" = 3,
                                             " " = 5,
                                             "SMS REAKT 1" = 3,
                                             " " = 6,
                                             "SMS X-SELL 2" = 5,
                                             " " = 10,
                                             "SMS REAKT 2" = 4,
                                             " " =  10,
                                             "SMS REAKT 3" = 6,
                                             " " = 86,
                                             "SMS AS 1 / SMS FU" = 61,
                                             "SMS AS 2 / SMS FU" = 60,
                                             " " = datkowe_kolumny_SMS
                                             
                                             
              )) %>%
              kableExtra::add_header_above(c(" ",
                                             " " = 6,
                                             "SMS FU" = 9,
                                             " " = 10,
                                             "SMS WB" = 66,
                                             " " = ncol(dt_SMS_ALL_info)-92
                                             
                                             
                                             
                                             
              )) %>%
              kableExtra::scroll_box(width = "100%", height = "900px")
          }
          
        }else{
          output$kable_SMS <- function() {
            
            aa <- data.table()
            kableExtra::kable(aa,  "html") %>%
              kableExtra::kable_styling(c("striped", "bordered", "hover"), full_width = F) 
            
            
          }
        }
        
      }
      
      
      updateProgressBar(session = session, id = "progres_bar", value = 100, title = "Przeprowadzam analizę")
      
      shinyjs::hide(selector = '.progress-group:has(#progres_bar)')
      
      
      
      
      ############## tabela sub tototale dla okresów
      
      dane2A[action_channel == 'SMS', ] -> dt_SMS_SMS
      if(nrow(dt_SMS_SMS) != 0){
        ## SMS zakłada działania do 270 dni
        dni_uzu <- rep(0:270) 
        dni_uzu <- as.data.table(dni_uzu)
        colnames(dni_uzu) <- "Liczba dni od daty startu"
        dni_uzu[, pp := 1]
        
        typy_uzu <- dt_SMS_SMS[, unique(action_type)]
        typy_uzu <- as.data.table(typy_uzu)
        colnames(typy_uzu) <- "action_type"
        typy_uzu[, pp := 1]
        
        doklej_uzu <- merge(typy_uzu, dni_uzu, by = 'pp', allow.cartesian = T)
        
        doklej_uzu[, pp := NULL]
        
        
        dt_SMS_SMS[, `Liczba dni od daty startu` := liczba_dni_od_daty]
        dt_SMS_SMS[, liczba_dni_od_daty := NULL]
        
        plyr::rbind.fill(doklej_uzu, dt_SMS_SMS) -> dt_SMS_SMS
        
        dt_SMS_SMS <- as.data.table(dt_SMS_SMS)
        
        #dt_SMS_SMS <- rbind(doklej_uzu, dt_SMS_SMS, fill = T,  make.row.names = TRUE)
        
        
        
        # uwaga na nakładające sie okresy jak no SMS WB 
        dt_SMS_SMS[`Liczba dni od daty startu` == 0, okres := "Poza schematem działań (0)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` == 1, okres := "SMS QX-SELL (1)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 2 & `Liczba dni od daty startu` <= 5, okres := "Poza schematem działań (2-5)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 6 & `Liczba dni od daty startu` <= 11, okres := "SMS FU (6-11)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 12 & `Liczba dni od daty startu` <= 14, okres := "SMS X-SELL 1 / SMS FU (12-14)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 15 & `Liczba dni od daty startu` <= 19, okres := "Poza schematem działań (15-19)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 20 & `Liczba dni od daty startu` <= 22, okres := "SMS REAKT 1 (20-22)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 23 & `Liczba dni od daty startu` <= 24, okres := "Poza schematem działań (23-24)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 25 & `Liczba dni od daty startu` <= 28, okres := "SMS WB (25-28)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 29 & `Liczba dni od daty startu` <= 33, okres := "SMS X-SELL 2/SMS WB (29-33)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 34 & `Liczba dni od daty startu` <= 43, okres := "SMS WB (34-43)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 44 & `Liczba dni od daty startu` <= 47, okres := "SMS REAKT 2/SMS WB (44-47)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 48 & `Liczba dni od daty startu` <= 57, okres := "SMS WB (48-57)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 58 & `Liczba dni od daty startu` <= 63, okres := "SMS REAKT 3/SMS WB (58-63)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 64 & `Liczba dni od daty startu` <= 90, okres := "SMS WB (64-90)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 91 & `Liczba dni od daty startu` <= 149, okres := "Poza schematem działań (91-149)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 150 & `Liczba dni od daty startu` <= 210, okres := "SMS AS 1 / SMS FU (150-210)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 211 & `Liczba dni od daty startu` <= 270, okres := "SMS AS 2 / SMS FU (211-270)"]
        
        dt_SMS_SMS[`Liczba dni od daty startu` >= 271, okres := "Poza schematem działań (271+)"]
        
        
        
        
        unique(dt_SMS_SMS[,  c("data", "data_min", "typ_sciezki", "action_channel_l", "client_id","id", "campaign_product_group", "first_product_group","product_count",
                               "status", "okres", "action_channel")]) -> dt_SMS_SMS_1
        
        dt_SMS_SMS_1[, licz := 1]
        dt_SMS_SMS_1[is.na(action_channel), licz := 0]
        
        unique(dt_SMS_SMS[,  c("data", "data_min", "typ_sciezki", "action_channel_l", "client_id","id", "campaign_product_group", "first_product_group","product_count",
                               "status", "okres", "action_channel", "action_type")]) -> dt_SMS_SMS_2
        
        dt_SMS_SMS_2[, licz := 1]
        dt_SMS_SMS_2[is.na(action_channel), licz := 0]
        
        
        
        dt_SMS_SMS_1 <- dt_SMS_SMS_1[, .(ile = sum(licz)), by = okres]
        dt_SMS_SMS_2 <- dt_SMS_SMS_2[, .(ile = sum(licz)), by = c('okres', 'action_type')]
        
        dt_SMS_SMS_1 <-cbind(dt_SMS_SMS_1, mianownik_SMS, mianownik)
        dt_SMS_SMS_2 <-cbind(dt_SMS_SMS_2, mianownik_SMS, mianownik)
        
        
        dt_SMS_SMS_1_w <- copy(dt_SMS_SMS_1)
        dt_SMS_SMS_2_w <- copy(dt_SMS_SMS_2)
        
        
        
        for(i in 1:nrow(dt_SMS_SMS_1_w)){
          if(i<=9){
            dt_SMS_SMS_1_w[i, okres:= paste0("0", i, ". ", okres)]}else{
              
              dt_SMS_SMS_1_w[i, okres:= paste0(i, ". ", okres)]  
            }
          
        }
        
        
        
        
        dt_SMS_SMS_2_w[, okres2 := NA]
        dt_SMS_SMS_2_w[, okres2 := as.character(okres2)]
        
        i=1
        for(i in 1:nrow(dt_SMS_SMS_2_w)){
          
          dt_SMS_SMS_2_w[i, 6 ] <-dt_SMS_SMS_1_w[, okres][substring(dt_SMS_SMS_1_w[, okres],5,100) %in% dt_SMS_SMS_2_w[, okres][i] ] 
          
        }
        
        dt_SMS_SMS_2_w[, okres := okres2]
        dt_SMS_SMS_2_w[, okres2 := NULL]
        
        
        dt_SMS_SMS_1[, `Hit Rate dedykowany` := paste0(round(ile/mianownik_SMS*100,2), ' %')]
        dt_SMS_SMS_2[, `Hit Rate dedykowany` := paste0(round(ile/mianownik_SMS*100,2), ' %')]
        
        
        dt_SMS_SMS_1[, `Hit Rate` := paste0(round(ile/mianownik*100,2), ' %')]
        dt_SMS_SMS_2[, `Hit Rate` := paste0(round(ile/mianownik*100,2), ' %')]
        
        dt_SMS_SMS_1[, `Liczba zamówień z działaniem` := format(ile, big.mark = ' ')]
        dt_SMS_SMS_1[, ile := NULL]
        
        dt_SMS_SMS_2[, `Liczba zamówień z działaniem` := format(ile, big.mark = ' ')]
        dt_SMS_SMS_2[, ile := NULL]
        
        
        dt_SMS_SMS_1[, `Liczba zamówień` := format(mianownik, big.mark = ' ')]
        dt_SMS_SMS_1[, mianownik := NULL]
        
        dt_SMS_SMS_2[, `Liczba zamówień` := format(mianownik, big.mark = ' ')]
        dt_SMS_SMS_2[, mianownik := NULL]
        
        dt_SMS_SMS_1[, `Liczba zamówień dedykowanych dla kanału` := format(mianownik_SMS, big.mark = ' ')]
        dt_SMS_SMS_1[, mianownik_SMS := NULL]
        
        dt_SMS_SMS_2[, `Liczba zamówień dedykowanych dla kanału` := format(mianownik_SMS, big.mark = ' ')]
        dt_SMS_SMS_2[, mianownik_SMS := NULL]
        
        
        
        dt_SMS_SMS_1 <- dt_SMS_SMS_1[, c("okres", 
                                         "Liczba zamówień", 
                                         "Liczba zamówień dedykowanych dla kanału",
                                         "Liczba zamówień z działaniem",
                                         "Hit Rate", 
                                         "Hit Rate dedykowany")]
        
        
        
        data.table(cn = names(dt_SMS_SMS_1), transpose(dt_SMS_SMS_1)) -> dt_SMS_SMS_1A
        
        header.true <- function(df) {
          names(df) <- as.character(unlist(df[1,]))
          df[-1,]
        }
        
        header.true(dt_SMS_SMS_1A) -> dt_SMS_SMS_1A
        
        
        ## kolejnosc kolumn
        
        Kol_kolumn <- c("okres",
                        "Poza schematem działań (0)",
                        "SMS QX-SELL (1)",
                        "Poza schematem działań (2-5)",
                        
                        "SMS FU (6-11)",
                        "SMS X-SELL 1 / SMS FU (12-14)",
                        
                        "Poza schematem działań (15-19)",
                        "SMS REAKT 1 (20-22)",
                        "Poza schematem działań (23-24)",
                        "SMS WB (25-28)",
                        "SMS X-SELL 2/SMS WB (29-33)",
                        "SMS WB (34-43)",
                        "SMS REAKT 2/SMS WB (44-47)",
                        "SMS WB (48-57)",
                        "SMS REAKT 3/SMS WB (58-63)",
                        "SMS WB (64-90)",
                        "Poza schematem działań (91-149)",
                        "SMS AS 1 / SMS FU (150-210)",
                        "SMS AS 2 / SMS FU (211-270)",
                        "Poza schematem działań (271+)")
        
        
        Kol_kolumn1 <- Kol_kolumn[Kol_kolumn %in% colnames(dt_SMS_SMS_1A)]
        Kol_kolumn2 <- Kol_kolumn[Kol_kolumn %in% unique(dt_SMS_SMS_2[, okres])]
        
        
        dt_SMS_SMS_1A <- dt_SMS_SMS_1A[, mget(Kol_kolumn1)]
        
        
        
        dt_SMS_SMS_2_ls <- dt_SMS_SMS_2[, c("okres", 
                                            "action_type",
                                            "Liczba zamówień")]
        
        dt_SMS_SMS_2_lsd <- dt_SMS_SMS_2[, c("okres", 
                                             "action_type",
                                             "Liczba zamówień dedykowanych dla kanału")]
        
        dt_SMS_SMS_2_lzk <- dt_SMS_SMS_2[, c("okres", 
                                             "action_type",
                                             "Liczba zamówień z działaniem")]
        
        dt_SMS_SMS_2_hr <- dt_SMS_SMS_2[, c("okres", 
                                            "action_type",
                                            "Hit Rate")]
        
        dt_SMS_SMS_2_hd <- dt_SMS_SMS_2[, c("okres", 
                                            "action_type",
                                            "Hit Rate dedykowany")]
        
        dt_SMS_SMS_2_ls <- reshape(dt_SMS_SMS_2_ls, idvar = "action_type", timevar = "okres", direction = "wide")
        names(dt_SMS_SMS_2_ls)[names(dt_SMS_SMS_2_ls) %like% "Liczba zamówień."] <- gsub("Liczba zamówień.", "", names(dt_SMS_SMS_2_ls)[names(dt_SMS_SMS_2_ls) %like% "Liczba zamówień."])
        dt_SMS_SMS_2_ls[is.na(dt_SMS_SMS_2_ls),] <- format(mianownik, big.mark = ' ')
        dt_SMS_SMS_2_ls <- dt_SMS_SMS_2_ls[1, ]
        dt_SMS_SMS_2_ls[, action_type := ' - ']
        dt_SMS_SMS_2_ls[, Metryka := "Liczba zamówień"]
        
        
        dt_SMS_SMS_2_lsd <- reshape(dt_SMS_SMS_2_lsd, idvar = "action_type", timevar = "okres", direction = "wide")
        names(dt_SMS_SMS_2_lsd)[names(dt_SMS_SMS_2_lsd) %like% "Liczba zamówień dedykowanych dla kanału."] <- gsub("Liczba zamówień dedykowanych dla kanału.", "", names(dt_SMS_SMS_2_lsd)[names(dt_SMS_SMS_2_lsd) %like% "Liczba zamówień dedykowanych dla kanału."])
        dt_SMS_SMS_2_lsd[is.na(dt_SMS_SMS_2_lsd),] <- format(mianownik_SMS, big.mark = ' ')
        dt_SMS_SMS_2_lsd <- dt_SMS_SMS_2_lsd[1, ]
        dt_SMS_SMS_2_lsd[, action_type := ' - ']
        dt_SMS_SMS_2_lsd[, Metryka := "Liczba zamówień dedykowanych dla kanału"]
        
        dt_SMS_SMS_2_lzk <- reshape(dt_SMS_SMS_2_lzk, idvar = "action_type", timevar = "okres", direction = "wide")
        names(dt_SMS_SMS_2_lzk)[names(dt_SMS_SMS_2_lzk) %like% "Liczba zamówień z działaniem."] <- gsub("Liczba zamówień z działaniem.", "", names(dt_SMS_SMS_2_lzk)[names(dt_SMS_SMS_2_lzk) %like% "Liczba zamówień z działaniem."])
        dt_SMS_SMS_2_lzk[is.na(dt_SMS_SMS_2_lzk),] <- format(mianownik, big.mark = ' ')
        dt_SMS_SMS_2_lzk[, Metryka := "Liczba zamówień z działaniem"]
        
        
        dt_SMS_SMS_2_hr <- reshape(dt_SMS_SMS_2_hr, idvar = "action_type", timevar = "okres", direction = "wide")
        names(dt_SMS_SMS_2_hr)[names(dt_SMS_SMS_2_hr) %like% "Hit Rate."] <- gsub("Hit Rate.", "", names(dt_SMS_SMS_2_hr)[names(dt_SMS_SMS_2_hr) %like% "Hit Rate."])
        dt_SMS_SMS_2_hr[is.na(dt_SMS_SMS_2_hr),] <- format(mianownik, big.mark = ' ')
        dt_SMS_SMS_2_hr[, Metryka := "Hit Rate"]
        
        dt_SMS_SMS_2_hd <- reshape(dt_SMS_SMS_2_hd, idvar = "action_type", timevar = "okres", direction = "wide")
        names(dt_SMS_SMS_2_hd)[names(dt_SMS_SMS_2_hd) %like% "Hit Rate dedykowany."] <- gsub("Hit Rate dedykowany.", "", names(dt_SMS_SMS_2_hd)[names(dt_SMS_SMS_2_hd) %like% "Hit Rate dedykowany."])
        dt_SMS_SMS_2_hd[is.na(dt_SMS_SMS_2_hd),] <- format(mianownik, big.mark = ' ')
        dt_SMS_SMS_2_hd[, Metryka := "Hit Rate dedykowany"]
        
        
        dt_SMS_SMS_2A <- rbind(dt_SMS_SMS_2_ls, dt_SMS_SMS_2_lsd, dt_SMS_SMS_2_lzk, dt_SMS_SMS_2_hr, dt_SMS_SMS_2_hd)
        
        
        Kol_kolumn2 <- c('Metryka', 'action_type', Kol_kolumn2)
        dt_SMS_SMS_2A <- dt_SMS_SMS_2A[, mget(Kol_kolumn2)]
        
        colnames(dt_SMS_SMS_2A)[colnames(dt_SMS_SMS_2A) == 'action_type'] <- 'Typ'
        
        
        output$download_SMS_1 <-downloadHandler(
          filename = function() { "sms_hitrate_okresy.csv" },
          content = function(file) {
            write.csv(dt_SMS_SMS_1A, file, row.names = FALSE, col.names = TRUE)  }  )
        
        
        output$download_SMS_2 <-downloadHandler(
          filename = function() { "sms_hitrate_okresy_typ_akcji.csv" },
          content = function(file) {
            write.csv(dt_SMS_SMS_2A, file, row.names = FALSE, col.names = TRUE)  }  )
        
        
        
        output$kable_1A_SMS <- function() {
          
          kableExtra::kable(dt_SMS_SMS_1A,  "html") %>%
            kableExtra::kable_styling(c("striped", "bordered", "hover"), full_width = T)  %>% kableExtra::column_spec(column = 2:ncol(dt_SMS_SMS_1A), width_min="5em") %>%
            
            kableExtra::scroll_box(width = "100%", height = "400px") 
          
          
        }
        
        output$kable_2A_SMS <- function() {
          
          kableExtra::kable(dt_SMS_SMS_2A,  "html") %>%
            kableExtra::kable_styling(c("striped", "bordered", "hover"), full_width = T)  %>% kableExtra::column_spec(column = 2:ncol(dt_SMS_SMS_2A), width_min="5em") %>%
            
            kableExtra::scroll_box(width = "100%", height = "1000px") 
          
          
        }
        
        
        ## wykres dla okresów działań
        
        dt_SMS_SMS_1_w[, hh := ile/mianownik]
        dt_SMS_SMS_1_w[, hh2 := ile/mianownik_SMS]
        
        dt_SMS_SMS_2_w[, hh := ile/mianownik]
        dt_SMS_SMS_2_w[, hh2 := ile/mianownik_SMS]
        
        
        if(input$select_typ_hit_rate == 'dedykowany dla kanału rekalmy'){
          dt_SMS_SMS_1_w[, hhh := hh2]
          dt_SMS_SMS_2_w[, hhh := hh2]
          
        }else{
          dt_SMS_SMS_1_w[, hhh := hh]
          dt_SMS_SMS_2_w[, hhh := hh]
          
        }
        output$okresy_wykres_SMS <- plotly::renderPlotly({
          
          if(input$select_typ_kanalu_na_wykresie == 'nie'){
            zz <- ggplot(dt_SMS_SMS_1_w, aes(x = okres, 
                                             y = hhh,
                                             label = paste0(round(hhh*100,1), ' %'),
                                             
                                             
                                             text = paste('Okres: ', dt_SMS_SMS_1_w$okres, '<br>',
                                                          'Liczba zamówień:', format(dt_SMS_SMS_1_w$mianownik, big.mark = ' '), '<br>',
                                                          'Liczba zamówień dedykowanych dla kanału:', format(dt_SMS_SMS_1_w$mianownik_SMS, big.mark = ' '), '<br>',
                                                          'Liczba zamówień z działaniem:', format(dt_SMS_SMS_1_w$ile, big.mark = ' '), '<br>',
                                                          'Hit Rate:', paste0(round(dt_SMS_SMS_1_w$hh*100,2), ' %'), '<br>',
                                                          'Hit Rate dedykowany:', paste0(round(dt_SMS_SMS_1_w$hh2*100,2), ' %'), '<br>'
                                             )
                                             
                                             
                                             
                                             
                                             
                                             
            )) + #scale_fill_gradient(low = "green", high = "red")+
              geom_bar(stat = 'identity', fill = '#8098bf', color = '#1a59bd') +
              
              geom_text(size = 3, position = position_stack(vjust = 0.5), size=15, check_overlap = T, color = '#225152')+
              
              theme_classic()+
              ggthemes::theme_wsj()+ 
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
              xlab("") + ylab("Hit Rate") +
              scale_y_continuous(labels = scales::percent) }else{
                
                zz <- ggplot(dt_SMS_SMS_2_w, aes(x = okres, 
                                                 y = hhh,
                                                 label = paste0(round(hhh*100,1), ' %'),
                                                 fill = action_type,
                                                 
                                                 text = paste('Okres: ', dt_SMS_SMS_2_w$okres, '<br>',
                                                              'Liczba zamówień:', format(dt_SMS_SMS_2_w$mianownik, big.mark = ' '), '<br>',
                                                              'Liczba zamówień dedykowanych dla kanału:', format(dt_SMS_SMS_2_w$mianownik_SMS, big.mark = ' '), '<br>',
                                                              'Liczba zamówień z działaniem:', format(dt_SMS_SMS_2_w$ile, big.mark = ' '), '<br>',
                                                              'Hit Rate:', paste0(round(dt_SMS_SMS_2_w$hh*100,2), ' %'), '<br>',
                                                              'Hit Rate dedykowany:', paste0(round(dt_SMS_SMS_2_w$hh2*100,2), ' %'), '<br>'
                                                 )
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                )) + #scale_fill_gradient(low = "green", high = "red")+
                  geom_bar(stat="identity", position="dodge") +
                  
                  # geom_text(size = 3, position = position_stack(vjust = 0.5), size=15, check_overlap = T, color = '#225152')+
                  
                  theme_classic()+
                  ggthemes::theme_wsj()+ 
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
                  xlab("") + ylab("Hit Rate") +
                  scale_y_continuous(labels = scales::percent) 
              }
          
          #
          plotly::ggplotly(zz,tooltip = c("text")) %>% 
            plotly::layout(paper_bgcolor='#ebeef2', plot_bgcolor='#ebeef2') })
        
        
      }else{
        
        aa <- data.table()
        output$download_SMS_1 <-downloadHandler(
          filename = function() { "sms_hitrate_okresy.csv" },
          content = function(file) {
            write.csv(aa, file, row.names = FALSE, col.names = TRUE)  }  )
        
        
        output$download_SMS_2 <-downloadHandler(
          filename = function() { "sms_hitrate_okresy_typ_akcji.csv" },
          content = function(file) {
            write.csv(aa, file, row.names = FALSE, col.names = TRUE)  }  )
        
        output$kable_1A_SMS <- function() {
          
          kableExtra::kable(aa,  "html") %>%
            kableExtra::kable_styling(c("striped", "bordered", "hover"), full_width = T)  
          
          
        }
        
        output$kable_2A_SMS <- function() {
          
          kableExtra::kable(aa,  "html") %>%
            kableExtra::kable_styling(c("striped", "bordered", "hover"), full_width = T) 
          
          
        }
        
        
        
        
        output$okresy_wykres_SMS <- plotly::renderPlotly({
          plot.new()
        })
        
      }
      
      
      source(file="server/VMS_tab.R", local=T)
      source(file="server/TEL_tab.R", local=T)
      source(file="server/POST_tab.R", local=T)
      source(file="server/INSERT_tab.R", local=T)
      source(file="server/EMAIL_tab.R", local=T)
      
      
    }
    
    
    
    bez_dzialan <- dane2A[ , .(ac = toString(unique(action_channel))), by = "id"]
    
    bez_zadnych_dzialan <- bez_dzialan[ac == 'NA', id]
    bez_dzialan_EMAIL <- bez_dzialan[!ac %like%  'EMAIL', id]
    bez_dzialan_VMS <- bez_dzialan[!ac %like%  'VMS', id]
    bez_dzialan_SMS <- bez_dzialan[!ac %like%  'SMS', id]
    bez_dzialan_TEL <- bez_dzialan[!ac %like%  'TEL', id]
    bez_dzialan_POST <- bez_dzialan[!ac %like%  'POST', id]
    bez_dzialan_INSERT <- bez_dzialan[!ac %like%  'INSERT', id]
    
    
    output$download_bez_dzialan <-downloadHandler(
      filename = function() { "id_zamowien_bez_zadnych_dzialan.csv" },
      content = function(file) {
        write.csv(bez_zadnych_dzialan, file, row.names = FALSE, col.names = TRUE)  }  )
    
    
    
    output$download_bez_dzialan_EMAIL <-downloadHandler(
      filename = function() { "id_zamowien_bez_dzialan_z_kanalu_EMAIL.csv" },
      content = function(file) {
        write.csv(bez_dzialan_EMAIL, file, row.names = FALSE, col.names = TRUE)  }  )
    
    output$download_bez_dzialan_VMS <-downloadHandler(
      filename = function() { "id_zamowien_bez_dzialan_z_kanalu_VMS.csv" },
      content = function(file) {
        write.csv(bez_dzialan_VMS, file, row.names = FALSE, col.names = TRUE)  }  )
    
    output$download_bez_dzialan_SMS <-downloadHandler(
      filename = function() { "id_zamowien_bez_dzialan_z_kanalu_SMS.csv" },
      content = function(file) {
        write.csv(bez_dzialan_SMS, file, row.names = FALSE, col.names = TRUE)  }  )
    
    output$download_bez_dzialan_TEL <-downloadHandler(
      filename = function() { "id_zamowien_bez_dzialan_z_kanalu_TEL.csv" },
      content = function(file) {
        write.csv(bez_dzialan_TEL, file, row.names = FALSE, col.names = TRUE)  }  )
    
    output$download_bez_dzialan_POST <-downloadHandler(
      filename = function() { "id_zamowien_bez_dzialan_z_kanalu_POST.csv" },
      content = function(file) {
        write.csv(bez_dzialan_POST, file, row.names = FALSE, col.names = TRUE)  }  )
    
    
    output$download_bez_dzialan_INSERT <-downloadHandler(
      filename = function() { "id_zamowien_bez_dzialan_z_kanalu_INSERT.csv" },
      content = function(file) {
        write.csv(bez_dzialan_INSERT, file, row.names = FALSE, col.names = TRUE)  }  )
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  ##########################################################################################################################################################################
  
  
  
  
  
}



shinyApp(ui = ui, server = server)
