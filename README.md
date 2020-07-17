I analyzed survey data regarding the past 4 presidential elections to analyze what did and did not lead people to go to the voting booth (not literally- mail in ballots count too!). The analysis explores how people’s ideologies, contact with campaigns, demographics, emotions towards the candidates and much more influence voting. 

[Click here for the full analysis](https://medium.com/@mendyzecher/voting-64693ba420b7)

In order to run the statistical analysis please go to [my GitHub page](https://github.com/wagner-mspp-2020/mz2733). Here, you can download the entire code in a Zip file and then open it on your computer. It consists of the raw data, including the survey results of the American National Election Survey and an excel file that has voting access rankings and the margin of victory for all states in the past 4 presidential elections. 

Additionally, this includes the markdown file to run the programming. Open this (R_MARKDOWN_VOTING) in R Studio and run the code as it is presented in that exact order! Included in it is importing the needed packages and raw data, code to clean the data, run the analysis and create the graphs. This will clean the data and lead you to both a Logit and OLS regression, both of which then have code to turn this information into relevant percentages and odds ratios. 
 
Lastly, the graphs created will be downloaded with several other files to help run the doe (renv folder and file, unzip instructions and more). 

I used the American National Election Survey, a survey given every presidential election year. The survey asks eligible voters questions both before and after the election. As mentioned the analysis covers, the survey covers the same items; people’s ideologies, contact with campaigns, demographics, emotions towards the candidates. Additionally, to control for both access to vote and competitiveness of elections I added the [Cost of Voting Index](https://www.liebertpub.com/doi/10.1089/elj.2017.0478) (Quan Li, Michael J. PomanteII, and Scot Schraufnagel) and the margin of victory [by state](https://www.fec.gov/introduction-campaign-finance/election-and-voting-information/).  
 
To conduct the analysis I used a two versions of regressions; a Logit and ordinary least squares. The Logit regression is the most technically correct regression analysis to use in a scenario where the variable one is looking to predict has a binary (X or Y, Win or Lose) outcome. The outcomes from Ordinary Least Squares regressions are more easier translatable than the results the Logit regression naturally expels. Additionally, after one alters a Logit regression to balance its’ flexibility issues, the results tend to align closely with a normal Ordinary Least Squares regression. Consequently, I ran both and used different analyses dependent on the relevant audience.

I wanted to thank Maxwell Austensen for all his work and TIME spent helping work out any issues with the coding and all general advice given! Additionally, thank you Nyx NG and Josh Merfeld for teaching me the nuances of a legit regression.    


