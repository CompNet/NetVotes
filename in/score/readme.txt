These files define the scores used when comparing to MEPs through their votes for a given text. 
In the EP, the outcome of a vote can be one of the following:  
- For: the MEP voted in favor of the text.
- Abstention: the MEP was neither in favor or not in favor of the text.
- Against: the MEP was not in favor of the text.
- NoVote: the MEP was present but decided not to vote (or couldn't vote).
- Absent: the MEP was not present, and did not justify his absence.
- DocAbsent: the MEP was not present and justified his absence.
- NA: the MEP was not holding a mandate when voting the considered text.

A score should be given for all possible combinations of two votes. If certain combinations are not specified, then the associated default value is NA.
When considering a combination of two votes, the order is not important, i.e. For vs. Against is similar to Against vs. For.
The specified scores should be either a value between -1 (full disagreement) and +1 (full agreement), or NA.
Here, NA is interpreted as the will to completely ignore the value when averaging the scores over the considered texts.
If you put several times the same combination (or equivalent combinations) several times in the table, the last one will probably be kept, and the others ignored (no guarantee though).