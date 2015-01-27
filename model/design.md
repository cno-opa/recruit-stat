# Regression modelling of applicant success

First version of model will focus just on CS test performance and overall process performance. A second version should look deeply at performance in NOPD background investigation process. The goal is to identify a set of characteristics which significantly predict applicant success overall, and at specific bottlenecks in the process.

## TODO:

* Meet with CS to determine what data are available
* List variable types
* Research appropriate regression model to use. What model can comprehensively consider two responses, success and time?

## Predictor variables

  * Age
  * Race
  * Gender
  * Location:
    * Urban/suburban/rural
    * Region
    * GNO vs NO vs other LA
  * Education:
    * Degree type
    * Major
    * School ranking
  * Previous work experience
  * Military experience
  * Extracurricular activities

## Response variable

 * Success or failure, estimated at various steps:
  * Overall
  * Document submission
  * MC test attendance
  * MC test passage
  * WE test passage
 * Time to completion
