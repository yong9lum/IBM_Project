# IBM Project - A HR Solution

A HR department receives thousands of resumes on a daily basis & evaluates them manually to shortlist candidates for a first-round interview. This is done by assessing if candidates have the right skills & experience, as well as the key qualities & traits required for a position. These various processes are currently very labor-intensive & hence, there is room to improve the effectiveness of current processes given the high turnover for certain roles such as sales. Also, it is difficult to accurately predict the potential fit and future conduct of the candidate. HR is thus seeking for ways to better support its HR team to make the recruitment process more effective with improved accuracy in the matching process.

This project aims to leverage on past available data & non-traditional methods to accurately assess & shortlist candidates with the relevant skillsets, experience & psycho-emotional traits, matching them with relevant job openings to drive operational efficiency & improve accuracy in the matching process.

**In this two-part solution**:

1. Predict Technical Fit
Text mining will be used to sieve out the important keywords from a job description, & a score will be allocated each word based on the frequency of it appearing within that job description. Text mining will then be done on an applicant's resume, & an applicant score will be tabulated based on whether the important keywords appeared in the resume. For a large corporation like IBM who might receive hundreds of applications for one job opening, this solution will enable the HR department to sieve out suitable candidates just by running resumes through a programme. They could choose to invite applicants based on whether their resume passed a benchmark score, or by taking the top __ percentile of applicants.

2. Predicting Attrition
Machine learning models will be used to predict the whether or not an applicant, if hired, will leave the company within four years (four years, based on a research, is the median number of years that an employee is required to work for a company before he starts generating value for them). This binary (0-1) attrition outcome will be predicted based on various particulars of the applicant, such as age, last drawn salary, & even the distance between his house from the workplace. Hiring new employees for IBM means that more resources or costs will be allocated to training that hire & preparing him to work at the company, hence it is important to choose new employees that are more likely to stay bring value to the company before moving on elsewhere.

**Application of the solution**:

When a candidate applies for a job, he will have to fill in a company-specific application form with information about himself, and submit his resume. With the technological advancements that are readily available to use in this day and age, machine learning can be utilised to carry out both layers of screening, saving time and physical work for HR hiring departments so that they can refocus their efforts into the actual interviews.

A computer application can easily be written to help the HR department to obtain the results of the text mining of the applicant’s resume in a few clicks. As for the model to predict attrition, the variables for the CART model can be obtained from the very application form that the candidate fills in during his application for the job. The company just has to ensure that all the necessary variables are captured in the application form.
