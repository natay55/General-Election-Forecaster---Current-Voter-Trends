# UK General Election Forecaster

Having always been interested in politics, I decided to have a go at creating my own MRP model to forecast the outcome of the next UK general election. Specifically, this project answers the following question: "What is the likely outcome of the UK general election if it were held today?". 

Below is an outline of how the model was constructed, with credit given to those who's framework this project is based on.

Note, if you are just following along to view predictions, then I will update the predictions for one of two reasons; one, I plan to update the predictions weekly based on national polling and/or I will update the model when the Wave 31 of the BES panel survey comes out (see below for details).

## 1. Predicting National Vote Share

Following the findings of Chris Hanretty, Ben Lauderdale and Nick Vivyan in their paper titled "Combining national and constituency polling for forecasting" (2015)<sup>[1]</sup>, we model the outcome of each poll y<sub>i</sub> with a Multinomial distribution: y<sub>i</sub> ~ Multinomial(n<sub>i</sub>, π<sub>i</sub>), where n<sub>i</sub> is the sample size of poll i and π<sub>i</sub> is the vector of true underlying vote shares at the time of the poll.

The true vote shares are modelled using a softmax transformation of latent party support scores: π<sub>i</sub> = softmax(α<sub>t<sub>i</sub></sub>, δ<sub>p<sub>i</sub></sub>), where α<sub>t<sub>i</sub></sub> is the vector of latent support scores at time t<sub>i</sub> and δ<sub>p<sub>i</sub></sub> is a vector of house effects for pollster p<sub>i</sub>. We update the latent support through a Bayesian random walk over time: α<sub>t</sub> = α<sub>t-1</sub> + ε<sub>t</sub>, with ε<sub>t</sub> ~ Normal(0, σ<sup>2</sup>). This model is then fitted using the MCMC algorithm via Stan with 4 chains and 2000 iterations per chain.


## 2. Fitting Party Models

England, Wales and Scotland all have their own separate models. However, due to a severe lack of publicly available data necessary to complete the full pipeline of an MRP model, **the seat predictions for Northern Ireland are from Electoral Calculus' custom polling table** - for this, I found a panel of data from April 2026 with the implied national vote share of each Northern Irish party (this can be found below).

Following Chris Hanretty, Ben Lauderdale and Nick Vivyan in their paper titled "Comparing strategies for Estimating Constituency Opinion from National Survey Samples" (2018), we take a set of individual predictors at the demographic and the constituency level (which will be outlined further down the page), where predictor k takes on L<sub>k</sub> possible categorical values. Then, the probability of voting for party i, y<sub>i</sub> is given as P[y<sub>i</sub> = 1] = logit<sup>-1</sup>(α<sup>0</sup> + α<sup>1</sup><sub>l<sub>1[i]</sub></sub> + ... + V<sub>j[i]</sub><sup>constituency</sup>). Here, α<sup>0</sup> is the baseline coefficient for a given party, α<sup>j</sup><sub>l<sub>j[i]</sub></sub> is the effect of individual i being in category l<sub>k</sub> of demographic variable k, and V<sub>j[i]</sub> is the random constituency effect of individual i being in constituency j (please refer to the cited paper for greater depth; I am just paraphrasing the theory that was introduced in the paper)<sup>[2]</sup>.

All data used to train each logit model was from Wave 30 of the British Election Study Internet Survey Panel<sup>[3]</sup>.

### 2.1: Predictor Selection
Throughout 
To capture voting intentions, I decided to start by finding the top 3 political issues that continually polled the highest since 2010. Using data from YouGov<sup>[4]</sup>, the top 3 issues selected were "Immigration and Asylum"; "The Economy"; and "Defence and National Security". With these areas identified, I broke down such categories in such a way that **predicts away from the trend** - for this election cycle, this means that where immigration and asylum are a top issue among the electorate, for example, rather than fitting our party model with indicators that are already well documented among the top polling parties (i.e. those most likely to be anti-immigration are white British who backed Brexit and do not hold a degree), we fit the model with the opposite predictors (all ethnicities, those who voted remain and those who hold a degree).

Along with general predictors (which I have identified as age, gender, level of education (individual and percentage of degree holders in a constituency), individual past vote and vote share in a constituency), the categorical breakdowns are as follows:

For my English model, I have the following predictors: 
 - Immigration and Asylum
   - Ethnicity
   - Individual Brexit Vote (arguably, this plays multiple roles*)
   - Population Density of each Constituency
   - Chris Hanretty estimate of Brexit leave share of a constituency (*)
   - Percentage of Muslims in a constituency (**)
- The Economy
   - Individual housing tenure
   - Percentage of renters in a constituency
   - Index of multiple deprivation
 
My Welsh model was more or less the same, as was my Scottish model; however, the BES internet panel data was extremely thin for both Scotland and Wales, so to avoid overfitting, predictors were reduced compared to England. For Scotland, my model includes the age group; gender; education level; housing tenure; past vote; percentage of renters; percentage of degree holders; party share in the 2024 election and deprivation index. However, instead of attitudes towards Brexit, **I added the constituency estimate for an remain vote (i.e. for Scotland to stay as part of the UK)**, which proved to be extremely useful in picking up the conservative vote. The Welsh model had the same base predictors as the Scottish model, but instead of attitudes to independence / Brexit, **I added the percentage of Welsh speakers in each constituency**; this managed to boost the Plaid Cymru vote shares (which were almost none before adding this to my models)

In addition, I added a geospatial indicator; this was most useful to distinguish between Liberal Democrat and Conservative/Labour voters. This is because Liberal Democrat and Conservative/Labour voters can appear almost identical when comparing demographics; instead, we leverage a spatial indicator to capture the influence of neighbouring constituencies on voting outcomes. This did help improve Liberal Democrat seats, and was especially useful around the South where Liberal Democrats do heavily campaign and pick up seats. To help capture the changing dynamics in constituencies where by-elections have taken place, I added an incumbency indicator such that incumbent parties from 2024 receive their vote share as a predictor, whilst constituencies that had a by-election is given 1 + vote share of the by-election (this also applies to MPs that defected to another party, such as Robert Jenrick defecting to Reform). The effects of this were limited, and we may only see the benefits of this when Wave 31 of the BES panel is released.
     
(*): Brexit has played a role in shaping the immigration system and the disruption of trade and movement across Europe. By including this, we capture the economic effects of small businesses, freedom of employment in Europe and attitudes towards defence and security. You will also note that I have excluded Defence and Security as a distinct category here; that is because it is quite implausible to capture such attitudes demographically or at a constituency level. Given that this is quite an abstract thing, I theorised that border controls and the cost of living could capture such attitudes towards defence and security (e.g. energy dependency was hyper exposed after Russia's invasion of Ukraine, and border controls feeds directly into immigration concerns).

(**): There is good evidence to suggest that Muslims do tend to vote for more left leaning parties, and this has a measured impact. This can be felt in the vote to Labour or Gaza independent candidates <sup>[5]</sup>.

## Poststratification
For poststratification, data protection of small areas meant that I couldn't quite get the joint distributions I would have liked. Since constituency level predictors don't require poststratification, I decided to pick out age, housing tenure, education level, gender and ethnicity (however, ethnicity is England exclusive due to data protection in Scotland and Wales). I was able to get a joint distribution for age x housing tenure and gender x education level x ethnicity for England; for Scotland and Wales, I was able to get age x housing tenure and gender x education level. Since the joint distribution of all desired predictors wasn't available, we must make the assumption that age and tenure are independent of gender, education level (and ethnicity) in influencing the voting intention of a given individual.

Following [3], we then calculate our fitted probabilities as π̂<sub>j</sub> = Σ<sub>s∈j</sub> N<sub>s</sub>π̂<sub>s</sub> / Σ<sub>s∈j</sub> N<sub>s</sub>, where N<sub>s</sub> is the census count for demographic cell s in constituency j, and π̂  is the predicted vote probability from the logit model.

## Calibration and Unwinding

MRP models have a known tendency to produce national vote share estimates that diverge from polling aggregators. This occurs because the demographic model, trained on survey data, does not perfectly reflect current voting intentions, particularly when political conditions have shifted since the survey was conducted. The standard solution is proportional swing calibration: scaling each party's predicted vote shares by the ratio of the aggregator's national estimate to the MRP implied national mean. However, I noticed a problem with applying this uniformly across all parties. For parties whose support is primarily **geographically driven**  - such as the Liberal Democrats and independent candidates - the MRP spatial predictors are doing useful work capturing place-specific effects that go beyond demographics. Applying proportional swing to these parties would destroy that geographic signal, pulling all constituencies toward the national mean and eliminating the spatial concentration that characterises their support.

I therefore propose setting the calibration ratio to 1 for geographically driven parties, trusting the MRP model over the aggregator for these cases. In practice this increased the Liberal Democrat implied national vote share by approximately 2 percentage points relative to the polling aggregator - reflecting their tendency to outperform national polls in their specific incumbency strongholds. This change alone doubled the seat count of the Liberal Democrats in England and much better reflected the current forecasts for the Liberal Democrats.

Furthermore, I propose using an 'unwinding' effect (this wording is taken from YouGov; the exact method is not publicly available, though we can implement a solution based off the general description provided in [6]). In my own implementation, I have taken this to be a Z-score transformation that aims to capture historic trends of party performance in previous elections, with the assumption that some parties will perform as their historical baseline suggests, and others will outperform their previous performances. Such transformations ensure that the vote share is unchanged from the previous calibration stage, and is given as the national mean + (vote share - national mean) * scaling ratio, where the scaling ratio is defined as the aggregator mean divided by the MRP model mean.

Where data is sparse (such as Scotland and Wales), I propose applying symmetric unwinding; that is, there is insufficient data to trust the MRP model over the historical baseline, and we cannot assume that any deviation is anything but an artefact of the model. So, we unwind or compress the standard deviation of each modelled party to match historical deviation. For this reason, I also propose averaging out the historical standard deviation to be the average of the 2019 and the 2024 election standard deviation.

Where data is rich, however, I propose a slightly different approach. Current polling is highly volatile - with Reform neck and neck with Labour, and the Conservatives slowly recovering, whilst the Liberal Democrats retain their historic highs and the Green Party reaching new highs, we need to ensure that we place trust in our MRP model where it is capturing a shift in voting intention. For my English model, I assume that the Labour, Conservative and Reform standard deviation will revert to their historic baseline - hence, I average out the 2019 and 2024 standard deviation for the historic baseline. However, I assume that the Greens, Liberal Democrats and 'Other' will perform **at least** as well as their 2024 baseline, whilst also giving the MRP model space to extend past historic norms. Hence, I apply symmetric unwinding for the Conservatives, Labour and Reform, and I apply asymmetric calibration to the Greens, Liberal Democrats and 'Other'; that is, only unwind to match the historic baseline if the historic deviation exceeds the MRP model.

The above method was extremely beneficial for all parties. It reduced overpredicting Reform seats, whilst giving the Green party space to grow and allowing the liberal democrats to retain their expected highs. This was the single best processing that I did that managed to anchor my predictions in the general consensus from the likes of Electoral Calculus and Nowcast. 

## Model Limitations
Of course, with any model, there's going to be limitations. The most obvious is that I cannot predict Northern Ireland by my own accord, but as previously explained, that is because there is no data to use to create a model from. 

The largest limitation is the BES panel I used to fit each of my models; the latest data was collected in May 2025, which is roughly around the Reform peak. As such, I've had to make the best I could of the data I have available, and some constituencies (especially in Scotland) couldn't capture current shifts to Conservative over Liberal Democrat. Additionally, constituencies such as Makerfield and Gorton and Denton are labelled as Reform / Labour seats; this is not entirely incorrect, considering Makerfield was considered to be a Reform safe seat unless Andy Burnham stood (which he did), and for the Gorton and Denton seat, it appears that the by-election voting intention couldn't be captured amongst those interviewed in the panel.

Furthermore, I couldn't capture the fact that Great Yarmouth is very likely to be Rupert Lowe's safe seat. At the time the data was collected, Lowe was still in Reform (hence why Great Yarmouth is predicted as Reform), but my hope is that Wave 31 of the BES data will capture the intention to vote for Lowe in Great Yarmouth. I also hope that Wave 31 will allow us to capture the shifts in constituencies like Makerfield and Gorton and Denton, but there really isn't anything I can do about it until the new data is available.

With all of this considered, I believe my model captures the general direction of the country. I hope the availability of future data will help improve predictions, but until then, this is the absolute best I could do with my skillset and accessibility to public data.

## Citations
[1]: Chris Hanretty, Ben Lauderdale, Nick Vivyan - "Combining national and constituency polling for forecasting" (2015). Article accessed here: https://www.sciencedirect.com/science/article/pii/S0261379415002267

[2]: Chris Hanretty, Ben Lauderdale, Nick Vivyan - "Comparing strategies for Estimating Constituency Opinion from National Survey Samples" (2018). Article accessed here: https://www.cambridge.org/core/journals/political-science-research-and-methods/article/comparing-strategies-for-estimating-constituency-opinion-from-national-survey-samples/60701055350642BFA9BD5FF6EE469BC2

[3]: British Election Study - https://www.britishelectionstudy.com/data-object/british-election-study-combined-wave-1-30-internet-panel-open-ended-response-data/

[4]: YouGov - https://yougov.com/en-gb/trackers/the-most-important-issues-facing-the-country 

[5]: UK Election Analysis: Changing patterns amongst Muslim voters: the Labour Party, Gaza and voter volatility - https://www.electionanalysis.uk/uk-election-analysis-2024/section-2-voters-polls-and-results/changing-pattern-amongst-muslim-voters-the-labour-party-gaza-and-voter-volatility/

[6]: YouGov - https://yougov.com/en-gb/articles/49061-yougov-mrp-labour-now-projected-to-win-over-400-seats
