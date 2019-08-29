### Churnacle project description: ###
The CodeBerry Programming School wanted to know what users should we call, so they didn't churn.
I've created the data preparing, machine learning model training codes in R studio on an AWS server. I also experimented a little bit with neural networks. 
In the end, the output was a list of email addresses of the users who were predicted to churn. It was a lot better than random guessing (2/3 of the predicted users churned for real) but it needed a far more data for the training that the company hadn't got.
I've also created a program in Node.js that sends email to the predicted users for churn through Intercom.

* scripts_old: older scripts
* scripts_last_model: the scripts that build the last model before this one in this folder

Most recent model:
* readfiles.R: readFiles() function reads in users_central.json and users_orange.json files (these files are from backblaze backup and was converted to json from bson)
* preparedata3.R: prepareData3() function does the data cleaning, call the other cleaning functions, set the types of the data fields
* processcentral3.R: processCentral3() extracts the necessary field from central_users, aggregates some variables, indentifies churn state
* processorange3.R: processOrange3() extracts the necessary field from orange_users, aggregates some variables, calculates the time spent with assignments
* traintensor.R: scripts that prepares, trains and evaluates neural network with the clean data
* churn_net.R: hyperparameter tuning methods
* estimators.R: machine learning estimator models (random forest and decision tree algorithms)(which are pretty accurate on training (~99.9%) with this data model that I prepared for the training, unfortunately overfits, needs a lot more data row)
* histograms.R: some scripts that plot some fancy histogram and there is also a correlation calculation
* churn_mlp.R: just the tensorflow model, can be used in hyperparameter tuning
* calculateestimated.R: calculateEstimated() function calculates estimated times for assignments from the cleaned data, creates table with mean/median/stander deviaton, needs lesson hashes
