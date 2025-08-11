## Introduction

The best way to grasp Test & Roll is to watch it boost [profit](#more-profit).

## Email A/B Test

You have designed a new image for your upcoming email campaign and want to test its performance versus the original design.

<table style="width: 100%; table-layout: fixed;">
  <tr>
    <th style="width: 50%; text-align: center;">Test</th>
    <th style="width: 50%; text-align: center;">Control</th>
  </tr>
  <tr>
    <td style="text-align: center;">
      <img src="green_email.png" style="width:100px;">
    </td>
    <td style="text-align: center;">
      <img src="blue_email.png" style="width:100px;">
    </td>
  </tr>
</table>

You have 40,000 customers to email.  

How many should get the test ad vs. the control ad to detect a real difference — not a random difference?  

With a hypothesis test, you can often use only part of the 40,000 for testing, then send the winner to the rest.  

## Hypothesis Test  

Inputs for the calculator:  

| Metric                          | Value  | Explanation                                              |
|---------------------------------|--------|----------------------------------------------------------|
| Confidence                      | 0.95   | Higher confidence lowers false positives (Type I error)                    |
| Power                           | 0.80   | Higher Power lowers false negatives (Type II error)                   |
| Baseline Conversion Rate        | 68%    | Expected control performance                             |
| Lift                            | 2%     | Minimum improvement to detect                            |
| $s$                           | 0.466  | Expected within-experiment standard deviation            |

**Result:** Optimal sample size is **18,430** per group.

That leaves 3,140 customers to send the best-performing email to after testing.  

After a month, the new design shows a 1% lift—**not statistically significant**.  

Now what? Keep the current design? Extend the test? Try again for a smaller detectable lift?  

If switching costs nothing and performance is the same, maybe just deploy it.  

This is where hypothesis testing leaves questions unanswered:  
* What if the recommended sample size is bigger than your population?  
* Which version to deploy if the result is non-significant?  
* If false positives don’t hurt profit, why control them?  

## Test & Roll is Built Different

Let's use Test & Roll instead of the hypothesis test design above.

### Test

Choose $n_1^*$ and $n_2^*$ customers to send the treatments.  
Collect data on profit for both treatments.  

### Roll
Choose a treatment to deploy to the remaining $N - n_1^* - n_2^*$ customers.

### Objective
Maximize combined profit for test stage and the roll stage.  

## Test & Roll Inputs

Inputs:

| Metric | Value  | Description                                                  |
|--------|--------|--------------------------------------------------------------|
| $N$    | 40k    | Total available population                                   |
| $s$    | 0.466  | Expected standard deviation of the data within the experiment |
| $\sigma$ | 0.03 | Variation in performance across different past treatments    |

These are input into the Test & Roll profit-maximizing sample size formula:

$$n_1 = n_2 = \sqrt{\frac{N}{4}\left( \frac{s}{\sigma} \right)^2 + \left( \frac{3}{4} \left( \frac{s}{\sigma} \right)^2  \right)^2 } -  \frac{3}{4} \left(\frac{s}{\sigma} \right)^2$$

**Result**: Optimal sample size is **1382**.

That leaves 38,618 customers to send the best-performing email to after testing.  

## Next Steps

Finding the optimal sample size for Test & Roll is easy once you have $\sigma$ and $s$.

Head to the <a href="#" onclick="Shiny.setInputValue('nav_click', 'priors_tab', {priority: 'event'}); return false;">Priors Tab</a> for help with this!

## More Profit

The authors demonstrate that Test & Roll earns more profit than Hypothesis Testing in 3 different experiments below. 

![1](/cloud/project/dashboard/www/website_test_results.png)

![2](/cloud/project/dashboard/www/display_results.png)

![3](/cloud/project/dashboard/www/catalog_results.png)

See the [paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3274875) for more info.

## Resources

* [Test & Roll Article](https://ron-berman.com/2020/01/26/test-and-roll/)
* [Test & Roll Paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3274875)
* [Test & Roll Walkthrough Youtube Video]()