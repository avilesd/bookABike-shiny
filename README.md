bookABike-shiny
===================

*Scenario:* Imagine you have an important appointment tomorrow morning and your don't want to miss it. Since you know there aren't always bikes available at your nearest bike-sharing spot, you wish you could just book a bike and have the certainty that it will be there tomorrow for you. This app shows a proof of concept, in which users can enter their location and desired time in the future to: 
1. Check the predicted and current availability of bikes in stations near you 
2. Get (or not) a recommendation to book a bike based on the results of our algorithm and 
3. Change the search radius and what we call the risiko-aversion. 

*How it works:* The app bases its model on historic demand values for bike-sharing at the specific stations. Based on different factors, such as day, hour, temperature, rain and location it 
calculates a prediction for the number of a bikes on the bike-spots near the given location. If the "Aktuell" box is checked, the real-time data (as provided by the DB-API [Link]) is shown so that users can compare both availabilities and ideally make a better decision.

After you click on 'Suchen' and the results are shown, you see the recommendation results of our model. You can further click on the blue circles to check the predicted number of available bikes at that station and time. The same goes for the red circles, showing the API real-time results. Furthermore, you can change the search radius and increase or decrease the risiko-aversion.

**Disclaimer:* This is a prototype and a proof of concep, it does not provide any kind of service and it is just intended as a demonstration.

This project was created in the context of a university project [FZI Institute Karlsruhe](http://www.fzi.de/startseite/) in cooperation with the [Mindbox](https://www.mindboxberlin.com/index.php/3rdhackathon.html) from the Deutsche Bahn.

Authors: Aviles, Blanck, Schüler, Villarreal, Zaytoun.
