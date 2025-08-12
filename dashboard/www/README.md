## Purpose

*This dashboard walks the user through the whole test & roll framework.*

## Introduction

Test & Roll (Feit & Berman, 2019) is a special Bayesian A/B testing framework aimed at **maximizing expected profit**, rather than minimizing false positives, resulting in smaller required sample sizes, yet higher profits!

Test & Roll is flexible and can be used in a variety of A/B experiments where you are measuring a continuous variable (such as mean revenue) or a rate/percentage (such as conversion rate). 

For example:

* Creative tests (comparing green vs. blue ad)
* Website tests (new website feature vs. old one)
* Direct mail tests (send direct mail to some customers and withhold from others)
* Email tests (like direct mail)

In situations like these, Test & Roll will become your best friend. Although it is a Bayesian analysis, it's actually quite intuitive - take what we learned from previous experiments as priors, and use that to design a new A/B test. 

*Disclaimer: currently, to run this in practice, you will need access to a data scientist who can do meta analysis. Future updates will allow regular stakeholders to run meta analysis.*

## Steps

### 1. <a href="#" onclick="Shiny.setInputValue('nav_click', 'survey_tab', {priority: 'event'}); return false;">Survey Tab</a>

Take the survey to see if Test & Roll fits your use-case. The dashboard will update based on your needs.

## 2. <a href="#" onclick="Shiny.setInputValue('nav_click', 'crash_course_tab', {priority: 'event'}); return false;">Crash Course Tab</a>

You will need to understand the intuition behind Test & Roll before running the calculator.

Don't worry, we will skip over the heavy math - you can read the [paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3274875) for that. 

## 3. <a href="#" onclick="Shiny.setInputValue('nav_click', 'priors_tab', {priority: 'event'}); return false;">Meta Analysis Tab</a>

Input your previous experiment results to get the right parameters to input into the sample size calculator.

## 4. <a href="#" onclick="Shiny.setInputValue('nav_click', 'sample_size_tab', {priority: 'event'}); return false;">Sample Size Tab</a>

Calculate optimal sample size, and then run your experiment.

## 5. <a href="#" onclick="Shiny.setInputValue('nav_click', 'results_tab', {priority: 'event'}); return false;">Results Tab</a>

After the experiment is run, input and examine your results.

## 6. Consulting

Need help running your first Test & Learn? Please reach out to Dr. Elea Feit or Dr. Ron Berman for help.

Want a dashboard like this built in your company? Please reach out to [Pathway Analytics](https://pathwayanalytics.gitbook.io/blog) for consulting help.