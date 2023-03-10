
## Linear Regression to Predict MPG

Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?

-vehicle_weight, spoiler_angle

Is the slope of the linear model considered to be zero? Why or why not?

-no, because the independent variables are explainig in some proporcion the dependent variable

Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?

- yes, because of the p-value associated with this model, is 5.35e-11 so our model is statistically significative.


## Summary Statistics on Suspension Coils

<img width="440" alt="Captura de Pantalla 2023-03-14 a la(s) 11 28 43" src="https://user-images.githubusercontent.com/72363865/225088769-0db2d9d7-7cac-421a-ad18-96ad88134834.png">

The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch. Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually? Why or why not?
- No, because the lot3 variance is 170.2861224, so is greater than 100 pounds

## T-Tests on Suspension Coils

<img width="531" alt="Captura de Pantalla 2023-03-02 a la(s) 8 11 18" src="https://user-images.githubusercontent.com/72363865/222452310-9ae02cde-b61a-4694-a8d1-1b35764689ac.png">

lot1: p-value:1 mean:1500
lot2: p-value:0.6072 mean:1500.2
lot3: p-value:0.04168 mean:1496.14

We fail to reject the null hypothesis for Lot1 and Lot2, for Lot3 we do reject the null hypothesis.



## Study Design: MechaCar vs Competition

What metric or metrics are you going to test?
-Ground clearence, mpg, AWD

What is the null hypothesis or alternative hypothesis?
-The null hypothesis is we are not better tha the competition and the alternative hypothesis is we are better than the competition

What statistical test would you use to test the hypothesis? And why?
-The statistical test we are going to perform is the Anova test, because I'm using 3 variables

What data is needed to run the statistical test?
<img width="256" alt="Captura de Pantalla 2023-03-06 a la(s) 11 54 49" src="https://user-images.githubusercontent.com/72363865/223191557-2b881bc3-5f99-4692-89f1-48bf2b4848a0.png">


