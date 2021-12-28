# Project timeline


## Week 1
- Setup GitHub and create a repository for the [project](https://github.com/Hussein-albaaj/opportunities-trauma-care)
- Install Rstudio and Slack
- Draft a timeline
- Draft a studyplan

## Week 2
- Revise your study plan based on feedback.
- Read up.

## Week 3
- Start writing code and upload a first script that imports and begin
  to clean the data.
- Draft and upload the progress report.
- Read up.
  
## Week 4
- Revise code based on feedback.
- Add code that creates a first table of sample characteristics.
- Revise progress report based on feedback and submit.
- Read up.

## Week 5-10
- Write code that generate a first set of preliminary results and
  revise weekly based on feedback.

## Week 11-12
- Draft the Results section and upload.
- Revise code based on feedback.

## Week 13-14
- Draft Discussion section and upload.
- Revise Results based on feedback.

## Week 15
- Draft and upload final project paper.

## Week 16 
- Revise final project paper and upload a second version.

## Week 17 
- Revise second version of final project paper and submit.

## Week 18-20
- Revise project paper based on comments and feedback from peers,
  course leaders, examiners, and me

![Rplot](https://user-images.githubusercontent.com/96419736/147613462-004d7887-daa8-4a3b-a6d1-90562290802f.jpeg)


library(timevis)

data <- data.frame(
  id      = 1:4,
  content = c("Item one", "Item two",
              "Ranged item", "Item four"),
  start   = c("2016-01-10", "2016-01-11",
              "2016-01-20", "2016-02-14 15:00:00"),
  end     = c(NA, NA, "2016-02-04", NA)
)

timevis(data)
