## Purpose

*This dashboard walks the user through the whole test & roll framework.*

## Introduction

Test & Roll (Feit & Berman, 2019) is a special Bayesian A/B testing framework aimed at **maximizing expected profit**, rather than minimizing false positives.

Anytime you have an A/B Test where A and B are similar, false positives have low costs, and deploying A has a low cost, you can use Test & Roll! A false positive is when you incorrectly think the treatment performed better than control. 

For example, you are testing new creative in an A/B test. The results for the new creative seem better, so you deploy the new creative. After measuring for a month, you realize the new creative performed the same as the old. Ugh, this is a false positive! No worries, though, because our profits remain the same. In this situation, assuming creating and deploying the new creative wasn't too costly, a false positive doesn't matter! 

In situations like these, Test & Roll will become your best friend. Although it is a Bayesian analysis, it's actually quite intuitive - take what we learned from previous experiments as priors, and use that to design a new A/B test. 

The dashboard will walk you through each step of the process.

## Steps

### 1. <a href="#" onclick="Shiny.setInputValue('nav_click', 'survey_tab', {priority: 'event'}); return false;">Survey Tab</a>

Take the survey to see if your A/B test can be framed using Test & Roll.

Examples of Test & Roll A/B Tests:

* Testing new vs. old creative
* Testing new website feature vs. old website feature
* Direct mail A/B tests
* Email campaign A/B tests

What if my false positive is costly though? 

* Test & Roll can be adjusted to account for this cost. We are working on another dashboard for this scenario. Stay tuned. You can read Section 4 in the paper for more details.

What if I have more than one treatment? 

* Test & Roll can be expanded to multiple treatments, but is more complicated and is out of the scope of this dashboard.

## 2. Set up Priors

Input your previous experiment results to help calculate optimal sample sizes for your next experiment.

## 3. Calculate the Optimal Sample Size

Calculate optimal sample size, and then run your experiment.

## 4. Examine A/B Test Results

After the experiment is run, input and examine your results.

## Resources


## Sources
