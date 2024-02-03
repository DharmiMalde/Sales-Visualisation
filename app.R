# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(fpp3)
library(tsibble)
library(stats)

# Used for advance visualization
Shop_wise_cat = read.csv("Shop_wise_cat.csv")
Shop_wise_ts = read.csv("Shop_wise_ts.csv")
Oil_price_effect = read.csv("Oil_price_effect.csv")

# use to represent data set
sales = read.csv("sales.csv")
oil = read.csv("oil.csv")
stores = read.csv("stores.csv")
holiday_events = read.csv("holidaysevents.csv")
trans1 = read.csv("trans1.csv")

# use for basic analysis
date_wise_data = read.csv("date_wise_data.csv")
upper_family_data = read.csv("upper_family_data.csv")
lower_family_data = read.csv("lower_family_data.csv")
sorted_store_wise = read.csv("sorted_store_wise.csv")
transaction_sales = read.csv("transaction_sales.csv")
date_wise_data = read.csv("date_wise_data.csv")
cleaned_oil_sales = read.csv("cleaned_oil_sales.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Sales Visualization Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "tab1", icon = icon("lightbulb")),
      menuItem("Data Sets", tabName = "tab2", icon = icon("table")),
      menuItem("Exploratory Analysis", tabName = "addtab", icon = icon("search")),
      menuItem("Store Wise Sales Analysis", tabName = "tab3", icon = icon("bar-chart")),
      menuItem("Category Wise Analysis", tabName = "tab4", icon = icon("line-chart")),
      menuItem("Conclution", tabName = "tab5", icon = icon("check"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
          .tab-content {
            margin: 10px; /* Adjust the margin size as needed */
          }
        ")
      )
    ),
    tabItems(
      tabItem(
        tabName = "tab1",
        h1("Introduction"),
        fluidRow(
          tabPanel("Introduction", 
                   h2("Welcome to the Sales Visualization Project"),
                   p(HTML("<p style='font-size:15px;'> Hello Guys, My name is Dharmi, student of Chennai Mathematical Institute. I have created this
                          project using RStudio with the help of R programming language in November-December 2023.</p>")),
                   p(HTML("<p style='font-size:15px;'>Welcome to the fascinating world of data visualisation, where we embark on a journey into the 
                          heart of retail dynamics. Our focus is on predicting sales for the myriad product families nestled within the Favorita
                          stores of Ecuador. The dataset, a treasure trove of information, unfolds a narrative woven with dates, store 
                          specifics, intricate product details, and the promotional status of each item..</p>")),
                   
                   p(HTML("<p style='font-size:15px;'>In this exploration, we navigate the evolving landscape of consumer behavior, analyzing 
                   how sales have evolved over time and uncovering the captivating seasonal trends that shape 
                   the retail experience. Our focus extends beyond mere observations as we meticulously examine 
                   the group of items contributing to maximum sales, aiming to decipher the key parameters influencing 
                   these sales dynamics.</p>")),
                   
                   p(HTML("<p style='font-size:15px;'>Join us on this venture as we harness the power of data to unravel the secrets of retail success, providing a
                     roadmap for stores to navigate the ever-changing marketplace with precision and foresight.</p>"))
          ),
        )
      ),
      tabItem(
        tabName = "tab2",
        h1("Data Set Description"),
        fluidRow(
          p("In this dataset we have sales for the thousands of product families sold at Favorita stores located in Ecuador. The data includes dates, store and product information, whether that item was being promoted, as well as the sales numbers. Additional files include supplementary information that may be useful in building your models.")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel("Sales.csv", 
                     fluidRow(
                         #width = 12,
                         p("The training data, comprising time series of features store_nbr, family, and onpromotion as well as the target sales."),
                         p("store_nbr identifies the store at which the products are sold."),
                         p("family identifies the type of product sold."),
                         p("sales gives the total sales for a product family at a particular store at a given date. Fractional values are possible since products can be sold in fractional units (1.5 kg of cheese, for instance, as opposed to 1 bag of chips)."),
                         p("onpromotion gives the total number of items in a product family that were being promoted at a store at a given date."),
                         box(width= 7,tableOutput("sales"))
          )
          ),
          tabPanel("Store.csv", 
                   fluidRow(
                     p("Store metadata, including city, state, type, and cluster."),
                     p("cluster is a grouping of similar stores."),
                     box(width= 8, tableOutput("stores"))
                   )
          ),
          tabPanel("Oil.csv", 
                   fluidRow(
                     p("Daily oil price. Includes values during both the train and test data timeframes. (Ecuador is an oil-dependent country and its economical health is highly vulnerable to shocks in oil prices.)"),
                     box(width= 3, tableOutput("oil"))
                   )
          ),
          tabPanel("holidays_events.csv", 
                   fluidRow(
                     p("Holidays and Events, with metadata"),
                     p("NOTE: Pay special attention to the transferred column. A holiday that is transferred officially falls on that calendar day, but was moved to another date by the government. A transferred day is more like a normal day than a holiday. To find the day that it was actually celebrated, look for the corresponding row where type is Transfer."),
                     box(width = 8, tableOutput("holidays_events"))
                   )
          ),
          tabPanel("transaction.csv.csv", 
                   fluidRow(
                     box(width= 5, tableOutput("trans1"))
                   )
        )
      )
      ),
      tabItem(
        tabName = "addtab",
        tabsetPanel(
          type = "tabs",
          tabPanel("Probability Distribution", 
                   fluidRow(
                     plotOutput("histogramPlot1"),
                     p(HTML("<p style='font-size:15px;'>In the preceding visualization, our objective was to gain insights into the daily sales patterns. The analysis
of the graph reveals that the data exhibits a central tendency, with the majority of daily sales hovering
around the 65,000 mark.</p>")),
                   )
          ),
          tabPanel("Top vs Bottom Categories", 
                   fluidRow(
                   column(6, plotOutput("upper")),
                   column(6, plotOutput("lower")),
                   p(HTML("<p style='font-size:15px;'>The preceding diagram provides valuable insights into the categorization of items sold, distinguishing
between the top and bottom categories. Notably, the top category predominantly comprises daily-use items,
while the bottom category predominantly consists of occasional or infrequently purchased items.</p>")),
                   )
          ),
          tabPanel("Top Stores", 
                   fluidRow(
                   plotOutput("store"),
                   p(HTML("<p style='font-size:15px;'>The depicted diagram offers a glimpse into the sales performance of the top 15 stores. Whereas the top 5
stores, identified by their store IDs are 53, 54, 16, 12, and 13.</p>")),
                   )
          ),
          tabPanel("No. of trasactions vs Sales", 
                   fluidRow(
                   plotOutput("trans"),
                   p(HTML("<p style='font-size:15px;'>The scatter plot displayed above reveals a strong correlation between the number of transactions and sales,
suggesting that the average transaction size across stores remains relatively consistent</p>")),
                   )
          ),
          tabPanel("Time Series", 
                   fluidRow(
                   plotOutput("trend101"),
                   plotOutput("seasonal101"),
                   p(HTML("<p style='font-size:15px;'> The seasonal component plot illustrates a noticeable spike in sales leading up to Christmas, followed by a
subsequent decline immediately after the holiday period.</p>")),
                   )
          ),
          tabPanel("Effect of Oil Price", 
                   fluidRow(
                   plotOutput("final_oil"),
                   p(HTML("<p style='font-size:15px;'> The scatter plot presented above, depicting the relationship between oil prices and sales, provides a clear
inference. It suggests that when oil prices exceed 75, there is a notable decrease in sales, whereas when oil
prices fall below this threshold, sales experience an uptick.</p>")),
                   )
          ),
        )
      ),
      
      tabItem(
        tabName = "tab3",
        h1("Shop Wise Analysis"),
        p(HTML("<p style='font-size:15px;'> Store-wise analysis is crucial in this dataset as it enables a granular understanding
               of individual store performance, aiding in optimized inventory management, targeted marketing, and the 
               identification of unique trends and seasonality for each location. This approach facilitates more informed 
               decision-making, allowing retailers to tailor strategies to specific store characteristics and respond effectively 
               to local factors, ultimately enhancing overall retail management and performance.</p>")),
        fluidRow(
          title = "Select Store",
          status = "primary",
          solidHeader = TRUE,
          selectInput("selectedStore", "Choose a store:", choices = unique(Shop_wise_ts$store_nbr), selected = NULL),
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel("Probability Distribution", 
                   fluidRow(
                     p(HTML("<p style='font-size:15px;'> Drawing a probability distribution for store sales in the dataset serves as a 
                            pivotal analytical tool with several advantages. It offers a visual representation of the sales variability, 
                            aiding in the identification of patterns and central tendencies within the data. This visualization is crucial 
                            for understanding the distribution of sales values and pinpointing potential outliers or skewed trends. Moreover, 
                            the probability distribution allows for the assessment of the likelihood of specific sales outcomes, enabling more 
                            informed decision-making in inventory management, promotion planning, and overall retail strategy. By embracing 
                            this statistical approach, retailers can enhance their forecasting accuracy, optimize resource allocation, and 
                            ultimately improve the efficiency and profitability of their operations.</p>")),
                     plotOutput("histogramPlot")
                   )
          ),
          tabPanel("Time Series", 
                   fluidRow(
                     p(HTML("<p style='font-size:15px;'> Engaging in time series analysis provides valuable insights into the temporal patterns and trends within the store sales data. By decomposing 
                            the time series into components such as trend and seasonality, retailers can uncover recurring patterns and 
                            understand how sales evolve over time. This knowledge is instrumental in strategic decision-making, allowing 
                            retailers to anticipate and capitalize on seasonal fluctuations, plan for peak demand periods, and optimize 
                            inventory management. Time series analysis also facilitates the identification of long-term trends, enabling 
                            retailers to adapt to changing market conditions and consumer preferences. Ultimately, leveraging time series 
                            analysis empowers retailers to make data-driven decisions that enhance operational efficiency, improve customer 
                            satisfaction, and contribute to long-term business success.</p>")),
                     plotOutput("trendPlot"),
                     plotOutput("seasonalPlot")
                   )
          ),
          tabPanel("Category Analysis", 
                   p(HTML("<p style='font-size:15px;'>Performing categorical analysis on store-wise data allows for a granular understanding 
                          of the top-selling items within each shop. By aggregating sales data based on product categories or families, 
                          retailers can identify the most popular and profitable items in specific stores. This information is crucial for 
                          inventory management, as it helps optimize stock levels, tailor product assortments to local preferences, and ensure 
                          that high-demand items are consistently available. Categorical analysis empowers retailers to make informed decisions
                          about product placement, promotions, and marketing strategies, ultimately enhancing the shopping experience for 
                          customers and maximizing sales opportunities for individual stores.</p>")),
                   fluidRow(
                     plotOutput("category")
                   )
          )
        )
      ),
      tabItem(
        tabName = "tab4",
        h1("Category Wise Anaysis"),
        p(HTML("<p style='font-size:15px;'>Conducting categorical analysis provides valuable insights into the distribution and 
               performance of different product categories within a dataset. It helps identify the top-selling or most popular 
               items in a given category, enabling retailers to focus their marketing efforts and allocate resources strategically. 
               Categorical analysis also aids in understanding customer preferences and behavior, allowing businesses to tailor their 
               product offerings to meet specific market demands. By segmenting data based on categories, retailers can optimize 
               inventory management, pricing strategies, and promotional campaigns for each product group. Overall, categorical 
               analysis empowers businesses to make informed decisions, enhance customer satisfaction, and improve overall operational 
               efficiency.</p>")),
        fluidRow(
          title = "Category vise Analysis",
          status = "primary",
          solidHeader = TRUE,
          selectInput("family", "Choose a category:", choices = unique(Shop_wise_cat$family), selected = NULL),
          #plotOutput("storeSalesPlot")  # Output for the plot
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel("Time Series", 
                   p(HTML("<p style='font-size:15px;'>Conducting time series analysis on each product category offers several 
                          benefits for retailers. Firstly, it allows for the identification of seasonality patterns specific to 
                          each category, enabling more precise demand forecasting. This insight helps retailers optimize inventory
                          levels, promotional strategies, and staffing based on historical sales trends. Additionally, time series
                          analysis facilitates the detection of long-term trends and irregularities, aiding retailers in 
                          understanding the evolving preferences and market dynamics within individual product categories. 
                          By uncovering patterns over time, retailers can make informed decisions regarding product launches, 
                          discontinuations, and strategic business planning. Ultimately, the application of time series analysis 
                          at the category level enhances retailers' ability to adapt to changing market conditions and meet 
                          customer demands more effectively.</p>")),
                   fluidRow(
                     plotOutput("trendPlot2"),
                     plotOutput("seasonalPlot2")
                   )
          ),
          tabPanel("Effect of oil Price", 
                   p(HTML("<p style='font-size:15px;'>Analyzing the impact of oil prices on each product category provides 
                          valuable insights into potential patterns and correlations between economic factors and consumer 
                          behavior. By examining how sales for different categories fluctuate in response to changes in oil 
                          prices, retailers can uncover trends that may influence purchasing decisions. For instance, certain 
                          categories of products, such as automotive or energy-intensive goods, might be more sensitive to 
                          variations in oil prices. Understanding these relationships enables retailers to adapt their strategies 
                          accordingly, adjusting pricing, promotions, and inventory management to align with economic dynamics.
                          This analysis not only enhances forecasting accuracy but also aids in developing proactive measures to 
                          mitigate the effects of economic fluctuations on specific product categories.</p>")),
                   fluidRow(
                   plotOutput("oilprice")
                   )
          )
        )
      ),
      tabItem(
        tabName = "tab5",
        h1("Conclusion"),
        fluidRow(
          box(width = 12,
            "In this Sales Visualization Project, we delved into the intricacies of retail dynamics, focusing on predicting sales for various product families in Favorita stores across Ecuador. Our exploration journey took us through time series forecasting, store-wise and category-wise analyses, and the impact of external factors such as oil prices and holidays. Here are the key takeaways:",
            tags$ul(
              tags$li("Time series forecasting provides valuable insights into temporal patterns, helping retailers anticipate and capitalize on seasonal fluctuations."),
              tags$li("Store-wise analysis facilitates optimized inventory management, targeted marketing, and the identification of unique trends for each location."),
              tags$li("Categorical analysis empowers retailers to understand customer preferences, optimize inventory, and tailor product assortments to local demands."),
              tags$li("Analyzing the impact of external factors, such as oil prices, enables retailers to adapt strategies, adjust pricing, and mitigate the effects of economic fluctuations."),
              tags$li("This project serves as a roadmap for retailers, leveraging the power of data to make informed decisions and enhance operational efficiency.")
            )
          )
        )
      )
    )
  )
)


# Server
server <- function(input, output) {
  
  output$sales = renderTable({
    sales
  })
  
  output$stores = renderTable({
    stores
  })
  
  output$oil = renderTable({
    oil
  })
  output$trans1 = renderTable({
    trans1
  })
  output$holidays_events = renderTable({
    holiday_events
  })
  
  output$histogramPlot1 <- renderPlot({
    hist(date_wise_data$sales, col = "aquamarine", xlab = "Sales", ylab = "Probability", main = "Daily Sales", probability = T , border = "darkgreen")
    x <- seq(min(date_wise_data$sales), max(date_wise_data$sales), length.out = 1000)
    y <- dnorm(x, mean = mean(date_wise_data$sales), sd = sd(date_wise_data$sales))
    curve(dnorm(x, mean = mean(date_wise_data$sales), sd = sd(date_wise_data$sales)), col = "darkgreen", lwd = 2, add = TRUE)
    abline(v = seq(0, 1500000, by = 250000), col = "gray", lty = 2)
    abline(h = seq(0, 0.00001, by = 0.0000005), col = "gray", lty = 2)
  })
  
  output$upper = renderPlot({
    ggplot(upper_family_data, aes(x = family, y = sales))+
      geom_bar(stat = "identity", fill = "goldenrod2")+
      geom_text(aes(label = sales, vjust = -0.15))+
      labs(x = "Categories", y = "Sales (in lakh)", title = "Sales in bottom categories ")+
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 25,hjust = 1),panel.background = element_rect(fill = "white"),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank())
  })
  
  output$lower = renderPlot({
    ggplot(lower_family_data, aes(x = family, y = sales))+
      geom_bar(stat = "identity", fill = "goldenrod2")+
      geom_text(aes(label = sales, vjust = -0.15))+
      labs(x = "Categories", y = "Sales (in cr)", title = "Sales in top categories ")+
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45,hjust = 1),panel.background = element_rect(fill = "white"),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank())
  })
  
  output$store = renderPlot({
    sorted_store_wise$store_nbr = as.character(sorted_store_wise$store_nbr)
    ggplot(sorted_store_wise[0:15,], aes(x = store_nbr , y = sales))+
      geom_bar(stat = "identity", fill = "deepskyblue2")+
      geom_text(aes(label = sales, vjust = -0.15))+
      labs(x = "Store_ID", y = "Sales (in lakhs)", title = " Sales of top 15 Stores")+
      theme(plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank())
  })
  
  output$trans = renderPlot({
    ggplot(transaction_sales, aes(x = transactions, y = sales)) +
      geom_point(color = "darkolivegreen1",size = 4) +
      labs(title = "No. of transaction vs Sales",
           x = "No. of transaction in each store",
           y = "Sales") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank())
  })
  
  output$trend101 = renderPlot({
    date_wise_data$date = as.Date(date_wise_data$date)
    ts_data = ts( date_wise_data$sales,start=2013,frequency = 365)
    decomposed = stl(ts_data, s.window = "periodic")
    ggplot(data.frame(Date = time(ts_data), Trend = decomposed$time.series[, "trend"]), aes(x = Date, y = Trend)) +
      geom_line(color = "orangered2", size = 1) +
      labs(title = "Trend Component",
           x = "Date",
           y = "Sales") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank()) 
  },height = 300, width = 800)
  
  output$seasonal101 = renderPlot({
    date_wise_data$date = as.Date(date_wise_data$date)
    ts_data = ts( date_wise_data$sales,start=2013,frequency = 365)
    decomposed = stl(ts_data, s.window = "periodic")
    ggplot(data.frame(Date = time(ts_data), Seasonal = decomposed$time.series[, "seasonal"]), aes(x = Date, y = Seasonal)) +
      geom_line(color = "lightgoldenrod", size = 1) +
      labs(title = "Seasonal Component",
           x = "Date",
           y = "Sales") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank()) 
  },height = 300, width = 800)
  
  output$final_oil = renderPlot({
    ggplot(cleaned_oil_sales, aes(x = dcoilwtico, y = sales)) +
      geom_point(alpha = 0.25,color = "mediumorchid3") +
      labs(title = "Effect of oil price on sales",
           x = "Oil Price",
           y = "Sales (in lakhs)") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank())
  })
  
  
  # tab 3.1
  output$histogramPlot <- renderPlot({
    i = input$selectedStore
    tab_3_data = subset(Shop_wise_ts, store_nbr == i)
    x <- seq(min(tab_3_data$sales), max(tab_3_data$sales), length.out = 1000)
    y <- dnorm(x, mean = mean(tab_3_data$sales), sd = sd(tab_3_data$sales))
    hist(tab_3_data$sales, col = "aquamarine", xlab = "Sales", ylab = "Probability", main = "Daily Sales", probability = TRUE, border = "darkgreen", ylim = c(0,max(y)+0.00003))
    curve(dnorm(x, mean = mean(tab_3_data$sales), sd = sd(tab_3_data$sales)), col = "darkgreen", lwd = 2, add = TRUE)
  },height = 400, width = 800)
  
  # tab 3.2
  output$seasonalPlot <- renderPlot({
    i = input$selectedStore
    tab_3_data = subset(Shop_wise_ts, store_nbr == i)
    tab_3_data$date = as.Date(tab_3_data$date)
    ts_data = ts(tab_3_data$sales, start = 2013, frequency = 365)
    decomposed = stl(ts_data, s.window = "periodic")
    ggplot(data.frame(Date = time(ts_data), Seasonal = decomposed$time.series[, "seasonal"]), aes(x = Date, y = Seasonal)) +
      geom_line(color = "lightgoldenrod", size = 1) +
      labs(title = "Seasonal Component", x = "Date", y = "Sales") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_line(color = "gray", linetype = "dashed"), panel.grid.minor = element_blank())
  },height = 300, width = 800)
  output$trendPlot <- renderPlot({
    i = input$selectedStore
    tab_3_data = subset(Shop_wise_ts, store_nbr == i)
    tab_3_data$date = as.Date(tab_3_data$date)
    ts_data = ts(tab_3_data$sales, start = 2013, frequency = 365)
    decomposed = stl(ts_data, s.window = "periodic")
    ggplot(data.frame(Date = time(ts_data), Trend = decomposed$time.series[, "trend"]), aes(x = Date, y = Trend)) +
      geom_line(color = "orangered2", size = 1) +
      labs(title = "Trend Component", x = "Date", y = "Sales") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_line(color = "gray", linetype = "dashed"), panel.grid.minor = element_blank())
  },height = 300, width = 800)
  
  output$oilprice <- renderPlot({
    fam = input$family
    #View(Oil_price_effect)
    #oil_sales$sales = oil_sales$sales/100000
    data = subset(Oil_price_effect, family == fam)
    #View(data)
    cleaned_oil_sales = na.omit(data)
    ggplot(cleaned_oil_sales, aes(x = dcoilwtico, y = sales)) +
      geom_point(alpha = 0.25,color = "mediumorchid3") +
      labs(title = "Effect of oil price on sales",
           x = "Oil Price",
           y = "Sales (in lakhs)") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank())
  })
  
  output$seasonalPlot2 <- renderPlot({
    fam = input$family
    #View(Oil_price_effect)
    #oil_sales$sales = oil_sales$sales/100000
    data = subset(Oil_price_effect, family == fam)
    #View(data)
    #cleaned_oil_sales = na.omit(data)
    data$date = as.Date(data$date)
    
    ts_data = ts(data$sales,start=2013,frequency = 365)
    decomposed = stl(ts_data, s.window = "periodic")
    
    #seasonal_component = stl_result$time.series[, "seasonal"]
    #trend_component = stl_result$time.series[, "trend"]
    #remainder_component = stl_result$time.series[, "remainder"]
    
    ggplot(data.frame(Date = time(ts_data), Seasonal = decomposed$time.series[, "seasonal"]), aes(x = Date, y = Seasonal)) +
      geom_line(color = "lightgoldenrod", size = 1) +
      labs(title = "Seasonal Component",
           x = "Date",
           y = "Sales") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank()) 
  },height = 300, width = 800)
  output$trendPlot2 <- renderPlot({
    fam = input$family
    #View(Oil_price_effect)
    #oil_sales$sales = oil_sales$sales/100000
    data = subset(Oil_price_effect, family == fam)
    #View(data)
    #cleaned_oil_sales = na.omit(data)
    data$date = as.Date(data$date)
    
    ts_data = ts(data$sales,start=2013,frequency = 365)
    decomposed = stl(ts_data, s.window = "periodic")
    ggplot(data.frame(Date = time(ts_data), Trend = decomposed$time.series[, "trend"]), aes(x = Date, y = Trend)) +
      geom_line(color = "orangered2", size = 1) +
      labs(title = "Trend Component",
           x = "Date",
           y = "Sales") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank())
    
  },height = 300, width = 800)
  
  output$category <- renderPlot({
    i = input$selectedStore
    tab_3_data = subset(Shop_wise_cat, store_nbr == i)
    tab_31_data <- tab_3_data[order(tab_3_data$sales), ]
    # Top 5 and Bottom 5 bar diagram
    some = input$categoricalAnalysis
    
    upper_family_data = tab_3_data[30:33,]
    #lower_family_data = sorted_family_wise[29:33,]
    upper_family_data$sales = round(upper_family_data$sales/1000)
    ggplot(upper_family_data, aes(x = family, y = sales))+
      geom_bar(stat = "identity", fill = "goldenrod2")+
      geom_text(aes(label = sales, vjust = -0.15))+
      labs(x = "Categories", y = "Sales (in thousands)", title = "Sales in bottom categories ")+
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 25,hjust = 1),panel.background = element_rect(fill = "white"),panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank())
    
  },height = 400, width = 500)
  
}

# Run the application
shinyApp(ui, server)
