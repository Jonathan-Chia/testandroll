## Purpose

*This dashboard walks the user through the whole test & roll framework.*

## Introduction

Test & Roll (Feit & Berman, 2019) is a special Bayesian A/B testing framework aimed at **maximizing expected profit**, rather than minimizing false positives.

Test & Roll is flexible and can be used in a variety of A/B experiments where you are measuring a continuous variable (such as mean revenue) or a rate/percentage (such as conversion rate). 

For example:

* Creative tests (comparing green vs. blue ad)
* Website tests (new website feature vs. old one)
* Direct mail tests (send direct mail to some customers and withhold from others)
* Email tests (like direct mail)

In situations like these, Test & Roll will become your best friend. Although it is a Bayesian analysis, it's actually quite intuitive - take what we learned from previous experiments as priors, and use that to design a new A/B test. 

The dashboard will walk you through each step of the process. 

## Steps

### 1. <a href="#" onclick="Shiny.setInputValue('nav_click', 'survey_tab', {priority: 'event'}); return false;">Survey Tab</a>

Take the survey, and the **dashboard will update based on your needs** or will tell you if Test & Roll should not be applied for your use case.

Todo:

* need to add more survey questions to this

3 different parts to this dashboard:

1. Website example (conversion rate)
2. Display ads (revenue)
3. Catalog ads (revenue but different priors)

4. Bonus: Website example (conversion rate with different priors)?

## 2. <a href="#" onclick="Shiny.setInputValue('nav_click', 'crash_course_tab', {priority: 'event'}); return false;">Crash Course Tab</a>

You will need to understand the intuition behind Test & Roll before running the calculator.

Don't worry, we will skip over the heavy math - you can read the [paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3274875) for that. 

## 3. <a href="#" onclick="Shiny.setInputValue('nav_click', 'priors_tab', {priority: 'event'}); return false;">Priors Tab</a>

Input your previous experiment results to help calculate optimal sample sizes for your next experiment.

Note: Due to the limited compute power of shiny apps environment, if you want to run a Bayesian meta analysis of previous experiments, you will have to run it on your local machine.

## 4. <a href="#" onclick="Shiny.setInputValue('nav_click', 'sample_size_tab', {priority: 'event'}); return false;">Sample Size Tab</a>

Calculate optimal sample size, and then run your experiment.

## 5. <a href="#" onclick="Shiny.setInputValue('nav_click', 'results_tab', {priority: 'event'}); return false;">Results Tab</a>

After the experiment is run, input and examine your results.
