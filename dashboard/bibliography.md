# Annotated Bibliography

*Jonathan Chia - Johns Hopkins - Data Visualizations*

## 1. Test & Roll: Profit-maximizing A/B tests

Test & Roll (Feit & Berman, 2019) reframes typical hypothesis A/B testing as a Bayesian problem aimed at maximizing expected profit, rather than minimizing Type I error. In many marketing contexts, concerns about false positives are irrelevant. Often, deploying a new digital test (such as making a change to a website or ad creative) has low costs. Whether the new test performs on par or slightly better, profits remain stable. 
Without constraints to control Type I errors (e.g., α = 0.05), Test & Roll can leverage a Bayesian sample-size formulation that maximizes profit while yielding an optimal sample size that is substantially smaller than classical hypothesis-testing requirements! 

### Application

This paper is the foundation of this dashboard. 

Source:

Feit, E. M., & Berman, R. (2019). Test & Roll: Profit-maximizing A/B tests. Marketing Science, 38(6), 1038–1058. https://doi.org/10.1287/mksc.2019.1177

## 2. Visualization in Bayesian Workflow

The authors of this paper demonstrate the importance of visualization in Bayesian analysis workflows. Visualizations are effectively helpful with diagnostics. For example, the authors use prior predictive checks to visualize if the priors they set are reasonable. Another example is utilizing MCMC diagnostic plots along with numerical checks to ensure model convergence. Sometimes just relying on numerical estimates can lead to false positives when the model is actually fine.

### Application

The dashboard needs to have clear guidance on interpreting and using priors. Visualization of priors and displaying prior predictive checks will be key.  

Source:

Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019). Visualization in Bayesian workflow. Journal of the Royal Statistical Society: Series A (Statistics in Society), 182(2), 389–402. https://doi.org/10.1111/rssa.12378

## 3. A Bayesian Cognition Approach to Improve Data Visualization

### Overview
The researchers studied if people can intuitively update their own posterior beliefs accurately like in a bayesian approach. They had the study participants create their own priors, see the actual data, and then make a guess of what the posterior distribution looks like. This study showed that it is very important for data visualizations to account for priors and use bayesian benchmarks to support better reasoning. If a visualization is poorly designed, then the user will end up with incorrect posterior beliefs. 

### Application

The dashboard needs to assist users, so they can set up and visualize their priors. With the correct priors, they will be better equipped to make solid decisions. Eliciting priors clearly also helps users to think deeply and understand the data before starting the analysis. 

Source: 

Kim, Y. S., Walls, L. A., Krafft, P. M., & Hullman, J. (2019). A Bayesian cognition approach to improve data visualization. In Proceedings of the 2019 CHI Conference on Human Factors in Computing Systems (CHI '19) (pp. 1–14). ACM. https://doi.org/10.1145/3290605.3300646

## 4. Mosaic Plots for Teaching Bayes Rule

The authors are bayesian professors who use a mosaic plot to teach and demonstrate Bayes rule effectively. Since 80% of human learning occurs visually, they argue that visualizations should be used to help teach tough topics such as Bayes Rule. The mosaic chart is much more intuitive than the typical conditional probability tree map, and the authors walk through an example of interpreting the mosaic chart. 

### Application

Although this dashboard won't necessarily need to teach Bayes Rule, the dashboard can utilize some of the design principles from this paper. For example, for the posterior probability distribution, it would be good to shade in the probability that the experiment earned us more than $0. Then, the user can instantly see the size of the probability. 

White, E. D., & Warr, R. L. (2021). Teaching Bayes’ rule using mosaic plots [Preprint]. arXiv. https://doi.org/10.48550/arXiv.2112.00162


## 5. 


## 6. Principles of Posterior Visualization

Although this isn't a scientific paper, this scientific article provides useful guidelines for displaying posterior distributions, and in particular, comparing posterior distributions. 

### Application

This paper illustrates one of the key strengths of using Bayesian statistics. Because our posterior beliefs are displayed in a posterior distribution, uncertainty can be displayed through a distribution on a graph instead of as a single point estimate. This can be particularly insightful when the uncertainty distribution is skewed (most of the time when we think of uncertainty we are inherently imagining a normal distribution where uncertainty is balanced on both sides). 

Source: 

Shubin, M. (2015, February 24). Principles of posterior visualization. Scientific Visualization Blog. Retrieved July 26, 2025, from https://ctg2pi.wordpress.com/2015/02/24/principles-of-posterior-visualization/


## Sources

1.	Feit, E. M., & Berman, R. (2019). Test & Roll: Profit-maximizing A/B tests. Marketing Science, 38(6), 1038–1058. https://doi.org/10.1287/mksc.2019.1177
2.	Feit, E. M. (2019). testandroll [GitHub repository]. GitHub. https://github.com/eleafeit/testandroll
3.	Wharton Customer Analytics. (2021, February 26). "How to Test and Roll", with Elea McDonnell Feit and Ron Berman [Video]. YouTube. https://www.youtube.com/watch?v=k3-gsNwWX7E
4. Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019). Visualization in Bayesian workflow. Journal of the Royal Statistical Society: Series A (Statistics in Society), 182(2), 389–402. https://doi.org/10.1111/rssa.12378
5. Kim, Y. S., Walls, L. A., Krafft, P. M., & Hullman, J. (2019). A Bayesian cognition approach to improve data visualization. In Proceedings of the 2019 CHI Conference on Human Factors in Computing Systems (CHI '19) (pp. 1–14). ACM. https://doi.org/10.1145/3290605.3300646
6. White, E. D., & Warr, R. L. (2021). Teaching Bayes’ rule using mosaic plots [Preprint]. arXiv. https://doi.org/10.48550/arXiv.2112.00162

Students are required to submit an annotated bibliography. 

Create an annotated bibliography containing at least 5 references related to your chosen topic.
Write a short (e.g., 3-5 sentence) description of each reference, giving its focus and major contribution. Include a complete citation for each reference.
Write a brief  introduction to your bibliography to define the topic and scope of your bibliography.