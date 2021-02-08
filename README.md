## Data challenge for Capital One application process - Airbnb & Zillow data

### Problem Statement
I'm  consulting for a real estate company that has a niche in purchasing properties to rent out short-term as part of their business model specifically within New York City.  The real estate company has already concluded that two bedroom properties are the most profitable; however, they do not know which zip codes are the best to invest in.    

The real estate company has engaged my firm to build out a data product and provide conclusions to help them understand which zip codes would generate the most profit on short term rentals within New York City.
I looked at publicly available data from Zillow and AirBnB:

* Cost data: Zillow provides us an estimate of value for two-bedroom properties

* Revenue data: AirBnB is the medium through which the investor plans to lease out their investment property. Fortunately for you, we are able to see how much properties in certain neighborhoods rent out for in New York City

* US County Level Socio-Economic Data - Gotten from USA.county.data package in R created by  Emil W. Kirkegaard [link](https://www.rdocumentation.org/packages/USA.county.data)

* US Zip Code Data - Gotten from the zipcode package, it contains a database of city, state, latitude, and longitude information for U.S. ZIP codes from the CivicSpace Database (August 2004) augmented by Daniel Coven [link](https://www.rdocumentation.org/packages/zipcode)


#### Assumptions
* No significant difference in housing prices in the same neighborhood
* The investor will pay for the property in cash (i.e. no mortgage/interest rate will need to be accounted for). 
* The time value of money discount rate is 0% (i.e. $1 today is worth the same 100 years from now). 
* All properties and all square feet within each locale can be assumed to be homogeneous (i.e. a 1000 square foot property in a locale such as Bronx or Manhattan generates twice the revenue and costs twice as much as any other 500 square foot property within that same locale.) 
* Occupancy rate of 75% annually
* 50% of guests review the hosts/listings


#### Questions to answer

 The Following are a few questions that I aim to answer through my analysis:
 
- How do prices of listings vary by location?
- How does the demand for Airbnb rentals fluctuate across the year and over years?
- Are the demand and prices of the rentals correlated?
- What are the different types of properties in NYC? Do they vary by neighborhood?
- What localities in NYC are rated highly by guests?
- Are prices equally distributed across the neighborhoods?
- What factors are associated with high return on investment?
- What zip codes make for a good investment

#### Steps Performed

1. Data Importation Quality check
2. Data munging and Preprocessing 
3. Feature Engineering
4. Data exploration and visual data narrative
5. Investment and Factorial Analysis
4. Conclusion and next steps.

#### Note

**There are a lot of dynamic charts created so an html output format is explicitly advised when knitting the R-Markdown document**

**Place the Zillow data set: "Zip_Zhvi_2bedroom_2021.xlsx" raw dataset in the `Raw_Data` folder in order to perform the analysis**

The folder contains a "capital_one_analysis.html" file which is a sample output.