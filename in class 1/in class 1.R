
# In class exercise 1

# Import data
bank <- read.csv("bank.csv")


# 1.Using the "bank.csv" file (found here), run a regression with all variables to predict balance.
# You'll need to code the factor variables (or else use R with some ingenuity.)

library("plot3D")

balance = bank$balance

age = bank$age
job = bank$job
marital = bank$marital
education = bank$education
default = bank$default
housing = bank$housing
loan = bank$loan
contact = bank$contact
day = bank$day
month = bank$month
duration = bank$duration
campaign = bank$campaign
previous = bank$previous
y = bank$y

regression <- lm(balance ~ age + job + marital + education + default + housing + loan + contact + day + month + duration + campaign + previous + y)


# 2.Use the F test to remove any variables that do not improve your model fit

regression_age_removed <- lm(balance ~ job + marital + education + default + housing + loan + contact + day + month + duration + campaign + previous + y)
regression_job_removed <- lm(balance ~ age + marital + education + default + housing + loan + contact + day + month + duration + campaign + previous + y)
regression_marital_removed <- lm(balance ~ age + job + education + default + housing + loan + contact + day + month + duration + campaign + previous + y)
regression_education_removed <- lm(balance ~ age + job + marital + default + housing + loan + contact + day + month + duration + campaign + previous + y)
regression_default_removed <- lm(balance ~ age + job + marital + education + housing + loan + contact + day + month + duration + campaign + previous + y)
regression_housing_removed <- lm(balance ~ age + job + marital + education + default + loan + contact + day + month + duration + campaign + previous + y)
regression_loan_removed <- lm(balance ~ age + job + marital + education + default + housing + contact + day + month + duration + campaign + previous + y)
regression_contact_removed <- lm(balance ~ age + job + marital + education + default + housing + loan + day + month + duration + campaign + previous + y)
regression_day_removed <- lm(balance ~ age + job + marital + education + default + housing + loan + contact + month + duration + campaign + previous + y)
regression_month_removed <- lm(balance ~ age + job + marital + education + default + housing + loan + contact + day + duration + campaign + previous + y)
regression_duration_removed <- lm(balance ~ age + job + marital + education + default + housing + loan + contact + day + month + campaign + previous + y)
regression_campaign_removed <- lm(balance ~ age + job + marital + education + default + housing + loan + contact + day + month + duration + previous + y)
regression_previous_removed <- lm(balance ~ age + job + marital + education + default + housing + loan + contact + day + month + duration + campaign + y)
regression_y_removed <- lm(balance ~ age + job + marital + education + default + housing + loan + contact + day + month + duration + campaign + previous)

anova(regression_age_removed,regression)
# P-value is 0.0007463, reject null hypothesis, we should include age variable

anova(regression_job_removed,regression)
# P-value is 0.1048, do not reject null hypothesis, we should exclude job variable

anova(regression_marital_removed,regression)
# P-value is 0.00485, reject null hypothesis, we should include marital variable

anova(regression_education_removed,regression)
# P-value is 0.05599, do not reject null hypothesis, we should exclude education variable

anova(regression_default_removed,regression)
# P-value is 3.845e-05, reject null hypothesis, we should include default variable

anova(regression_housing_removed,regression)
# P-value is 0.6913, do not reject null hypothesis, we should exclude housing variable

anova(regression_loan_removed,regression)
# P-value is 0.001056, reject null hypothesis, we should include loan variable

anova(regression_contact_removed,regression)
# P-value is 0.392, do not reject null hypothesis, we should exclude contact variable

anova(regression_day_removed,regression)
# P-value is 0.9774, do not reject null hypothesis, we should exclude day variable

anova(regression_month_removed,regression)
# P-value is 2.2e-16, reject null hypothesis, we should include month variable

anova(regression_duration_removed,regression)
# P-value is 0.3621, do not reject null hypothesis, we should exclude duration variable

anova(regression_campaign_removed,regression)
# P-value is 0.6465, do not reject null hypothesis, we should exclude campaign variable

anova(regression_previous_removed,regression)
# P-value is 0.6711, do not reject null hypothesis, we should exclude previous variable

anova(regression_y_removed,regression)
# P-value is 0.6643, do not reject null hypothesis, we should exclude y variable

# So, the variables that should be included in this regression are age, marital, default, loan, month

final_regression <- lm(balance ~ age + marital + default + loan + month)


# 3.Discus the resulting final model

# After Using the F test, we find that the reduced model of job, education, housing, contact, day, duration, campaign, previous, y do not significantly differ from the full mode.
# And, the reduced model of age, marital, default, loan, month do significantly differ from the full mode.
# The variables of age, marital, default, loan, month should be included in this regression mode.

# Other things equeal,
# One more year of age is associated with an increase of 25.74 dollars of balance on average.
# A married person has an expected balance of 357.19 dollars higher than divorced person.
# A single person has an expected balance of 613.65 dollars higher than divorced person.
# A customer, who has credit in default, has an expected balance of 1429.33 dollars lower than the customer who does not have any default.
# A customer, who has either a mortgage or another type of loan, has an expected balance of 439.28 dollars lower than the customer who does not have any loan with the bank.
# A customer, who opened the bank account in December, has an expected balance of 1814.54 dollars higher than the customer, who opened the bank account in April.
# A customer, who opened the bank account in February, has an expected balance of 332.24 dollars lower than the customer, who opened the bank account in April.
# ...
# ...
# A customer, who opened the bank account in September, has an expected balance of 142.92 dollars lower than the customer, who opened the bank account in April.



