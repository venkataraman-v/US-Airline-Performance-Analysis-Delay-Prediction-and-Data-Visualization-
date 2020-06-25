# UNITED STATES AIRLINE PERFORMANCE ANALYSIS AND VISUALIZATION   
 
The Air Traffic Organization of the United States provides service to more than 44,000 flights and 2.7 million airline passengers across more than 29 million square miles of airspace. Being able to navigate through the delays and cancellations and improve time spent in air travel is customary for airline performance and passenger satisfaction. 
This project aims to analyse the performance of airlines operating in the United States and predict flight delays by considering the cause for delays, flight cancellations and other factors that affect the performance of airlines and build an interactive web application to visualize the same. The data was primarily collected from the Bureau of Transportation Statistics. 
 
## R-Shiny Web Application 
The Application consists of three main components. 
### Airline Performance Analysis Dashboard:  
This part of the application depicts the information about the monthly flight operations of the Airline. Consisting of two interactive plots (bar graph and line graph), this section presents the information on the total number of flights operated, the average number of flights per day for the chosen Airline and month. The click event on the bar graph will open a list of the flights from the origin point. Similarly, the click event on the line graph shows a more detailed picture of the number of flights per month. This tab has two input fields, Airline name and month.  
 
### Travel Delay Analysis: 
This section highlights the percentage of delayed flights from the chosen airport to the destination state. Information is presented in the form of four plots viz map visualization, doughnut chart, multibarred bar chart and line chart. Origin and destination airports on the map are represented with the marker and the dots respectively. Size of the dot indicates the percentage of delay flights to that airport. The doughnut plot compares the total flights and delayed flights between chosen origin and destination airports. Similarly, bar and line charts visualize the total flights and delayed flights with regards to the Airlines and month respectively. This tab allows users to choose from and to airports and allowable delay time. The application also has an alerting mechanism to indicate that there are no flights between the given source and destinations.  
 
### Predictive Analytics: 
This tab is to predict the delay times of the flights based on the historical data. The output for this tab is based on the model being trained using logistic regression Functionality has been built to accept Airline operator name, Month, Air time, Distance between the ports and hour in the day, with the model predicting delay time of each flight, with Logistic Regression in the background. 
