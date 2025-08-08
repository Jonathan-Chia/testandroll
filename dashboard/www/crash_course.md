## Introduction

The best way to understand Test & Roll is to see it in action.

This section walks through an example A/B test design using a hypothesis test and then compares it to Test & Roll. 

## Email A/B Test

You have designed a new email with better headers and images and want to test its performance versus the current design.

You have about 40k customers available to send emails to.

What is your optimal sample size - how many people should you send the new email vs. the old email to detect a statistically significant difference (wasn't just from random chance)? 

Ideally, you will only need to test on a smaller subset of this 40k, and then you can deploy the best email design to the rest. 

Let's run a hypothesis test!

## Hypothesis Test

You use the following inputs in the hypothesis test calculator: 

* confidence = 0.95: this keeps our false positives low (type I error)
* power = 0.80: this keeps our false negatives low (type II error)
* baseline control email conversion rate = 68%: how we expect control to perform
* lift = 2%: we want to detect 2% or better lift in conversion rates
* \(s\) = 0.466: the expected standard deviation of the data within the experiment

The calculator outputs an estimated optimal sample size of **18430**.

Great! You send 18430 newly designed emails to the test group, and then send 18430 to the control group. That leaves 3140 people that we can send the best email to. 

After a month, you find that the newly designed emails had a 1% conversion rate lift, but it is not statistically significant (could just be random chance that it did better).

What should you do now? Keep the current design? Extend the test longer? Run another test with a smaller detectable lift percentage? 

It doesn't cost you to deploy the new design and performance would be about the same, so should you just deploy it? 

As you can see, hypothesis testing doesn't answer the following questions:

* What if recommended sample size is larger than available population?   
* Which treatment should be deployed if the difference is non-significant? 
* False positives (we send out the new design when it in reality will perform the same as current design) does not reduce profit in this setting. Why control them?

## Test & Roll is Built Different

Test & Roll adjusts the hypothesis testing framework to focus on profit instead of false positives. Let's use Test & Roll instead of the hypothesis test design above.

### Test

Choose $n_1^*$ and $n_2^*$ customers to send the treatments.  
Collect data on profit for both treatments.  

### Roll
Choose a treatment to deploy to the remaining $N - n_1^* - n_2^*$ customers.

### Objective
Maximize combined profit for test stage and the roll stage.  

## Test & Roll Inputs

Inputs:

* \(N\): 40k: total available population
* \(s\): 0.466: the expected standard deviation of the data within the experiment
* \(\sigma\): 0.03: variation in performance across different past treatments

These are input into the Test & Roll profit-maximizing sample size formula:

$$
n_1 = n_2 =
\sqrt{\frac{N}{4}\left( \frac{s}{\sigma} \right)^2 +
\left( \frac{3}{4} \left( \frac{s}{\sigma} \right)^2 \right)^2 }
- \frac{3}{4} \left(\frac{s}{\sigma} \right)^2
$$

The outputted sample size is **1382**. That leaves us with 38618 customers that we can email with the best design. 

## Test & Roll = More Profit

The authors demonstrate that Test & Roll earns more profit than Hypothesis Testing in 3 different experiments below. 

![1](dashboard/www/website_test_results.png)

![2](dashboard/www/results-table.png)

![3](dashboard/www/catalog_results.png)

See the [paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3274875) for more info.

## Next Steps

Finding the optimal sample size for Test & Roll is easy! 

Head to the <a href="#" onclick="Shiny.setInputValue('nav_click', 'priors_tab', {priority: 'event'}); return false;">Priors Tab</a>.

## Resources

* [Test & Roll Article](https://ron-berman.com/2020/01/26/test-and-roll/)
* [Test & Roll Paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3274875)
* [Test & Roll Walkthrough Youtube Video]()