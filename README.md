# R-stats
## 1. Pilot_study_analysis
Dataset from a pilot within-subjects study.
We want to plan a research study about the association between the time of the day and the levels of people’s statistical skills

The dataset from the pilot study contained the following variables
- Participant ID (pp_code)
- Gender (f = female; m = male)
- Age (in years)
- Time of day (morning or evening)
- The score on a statistics skills test (the variable is called "score;" this score has theoretical values ranging between 0 and 100, with higher numbers indicating better skills)

Analysis includes: inspection, removal of NAs, visualization of the dependent variable distribution, t-test and effect size calculations.

## 2. Multiple_regression_analysis
To test research questions involving moderation hypotheses.
Inspect univariate distributions and bivariate associations, perform a multiple linear regression with two- and three-way interactions.

The data set contained these variables:

- gender		participant sex (males = 0; females =1)
- sperform	social performance (0 to 25, with higher scores indicating better performance)
- sskills		social skills (1 = unskilled to 5 = very skilled)
- scomplex	social complexity (1 = low complexity to 5 = high complexity)
- sIQ		social intelligence (1 = low intelligence to 5 = high intelligence)

We had to test gender differences, the moderation of social skills on association between social complexity and performance
as well as the moderation of gender and social intelligence on association between social skills and social complexity.

The script includes the preliminary analysis (part A) and the multiple regression analysis (part B).

##3. Mixed-effects_model_analysis
In this study, each participant was asked to rate the attractiveness of 12 options, each of them being a
specific monetary reward delivered at a specific time.
Interested whether and how the attractiveness of a reward is influenced by the time of delivery and the magnitude of 
the reward (i.e., the amount), and whether these things differ between different age groups.

First dataset:
- pp_code      Participant's code
- Delay        Time of delivery as a continuous variable with values 0, 1, 2, and 3.
- Amount       Amount of reward as a continuous variable with values 10, 50, 90 (as euros).
- Rating       Participants ratings for attractiveness, continuous variable from 0 to 100.
Second dataset:
- pp_code      Participant's code
- AgeGroup     Age of participants, categorical variable with values "children", "adolescents", "adults".    



Script includes all the preparatory steps (such as loading the data,merging data frames, preparing the variables etc.),
diagnostics before running the models, running the models, diagnostics after running the models, obtaining p values and figures.