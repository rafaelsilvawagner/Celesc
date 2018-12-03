The codes are from a problem that I was trying to solve for my former company, there's tons of models here that make forecasts one step ahead for eletricity power demand.

Also have codes for analyse the original series and models performance. Everything is in R!

As I cannot release the dataset an interesting briefing is that the best models are GUM, and Holt Winters, sarima and it's expansions are also interesting. The non-linear models are useless, probably dute to the short number of observations, we were forecasting monthly points with a 20 year data-base, this is enough for linear models but neural networks can't resist overfitting, STAR models were not reasonable but we try just for sports.

Models with exogeneous variables were also tested but they can't generate greater performance due to the fact that the correlations are contemporary and we have to also forecast this exogeneous variables, which in the end became a non-sense exercise.
