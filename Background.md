# Climate Action Tracker (CAT)
Many of our projects seem to revolve around the same kind of data on sectoral emissions, GDP and population. It is therefore reasonable to centralize data handling in order to assure consistency across different projects.

Many data handling tasks at Climate Analytics are carried out using Excel. While Excel is a great tool for *quick and dirty* work with data, it is not very good at the complex projects we carry out with it at Climate Analytics. Daniel Lemire, a computer science professor at the University of Quebec, in a [blog post](http://lemire.me/blog/2014/05/23/you-shouldnt-use-a-spreadsheet-for-important-work-i-mean-it/) on flaws in the analysis carried out by Thomas Piketty in his influential [book](http://www.amazon.de/Capital-Twenty-First-Century-Thomas-Piketty/dp/067443000X/ref=sr_1_1?ie=UTF8&qid=1449845760&sr=8-1&keywords=Capital+in+the+Twenty-First+Century+Thomas+Piketty) identifies three major problems with the use of spreadsheets for serious work:

> * All professional software should contain extensive tests… how do you know that your functions do what you think they do if you do not test them? Yet spreadsheets do not allow testing.
> * Spreadsheets make code review difficult. The code is hidden away in dozens if not hundreds of little cells… If you are not reviewing your code carefully… and if you make it difficult for others to review it, how do expect it to be reliable?
> * Spreadsheets encourage copy-and-paste programming and ad hoc fudging. It is much harder to review, test and maintain such code.

Let us look at the Climateactiontracker (CAT). The CAT requires a lot of manpower just for copying, editing and transforming data. A similar amount of work is needed for reviewing the calculations. Often, the same task has to be done over and over again for each country that is assessed. This is a source both of error and frustration. We should aim for our work to be more on choosing the assumptions underlying the assessment and formulatiing a well understandable country report that adheres to our standards.

Automatization of repetitive and error prone work could free up resources for more interesting and rewarding work.

However, most people at Climate Analytics are pretty aquainted with Excel and hardly at all with R. Moreover, the other institutions we cooperate with mainly rely on Excel and it seems very unlikely that they will change that. Therefore, it must always be kept in mind that IDA needs to be operated by people with little knowledge of R and output must be provided as Excel-files in line with our grown standards.

# Simplified Integrated Assessment Model (SIAM)
