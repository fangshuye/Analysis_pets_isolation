/*  SAS code to analyze Compliance.  */


data set1;
  infile 'Compliance.csv' dlm=',' firstobs=2; 
  input workerId $ MostAttach $  Compliance $ time $;
  run;


proc glimmix data=set1 method=quad(qpoints=7);
class workerId MostAttach time(ref='1') Compliance(ref='0');
model Compliance = MostAttach time /s dist=binary link=logit; 
random intercept/subject=workerId;
run; 
