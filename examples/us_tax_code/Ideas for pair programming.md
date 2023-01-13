# Ideas for pair programming:

* Some functions are very mathematical, and the lawyers won't be able to check that the code is correct, in that case the programmer should write up a plain text specification of the function that the layer should agree to. Then, it's up to the programmer to verify that the code matches the specification.

* Some inputs have to be filled by the user but are regulated as a fact and circumstance, so the user interface has to flag those inputs specially and 
warn the user that what he enters as input is actually governed by strict 
rules

* The difficulty of translating law to code is independent (or even it has a reverse relationship) of the difficulty of grasping the meaning of the law for the lawyer.

* Variables are defined multiple times. We could use default prioritized logic to solve the ordering between those definitions but then it would be 
difficult for the user to refer to a specific point of the chain of definitions, which happens in reality (for instance Section 24 of the US Tax Code according to Sarah). So this feature would be a nice syntactic sugar to avoid defining 6 different variables for the same concept in a simple settings but it wouldn't scale to more complex settings. 

* Whenever we try to code up an article, we identify the notions inside the article, we turn them into variables, and then we connect variables with rules.

* Idea for the paper: compare the Catala implementation with the equivalent using OpenFisca.

* Sometimes we have to turn a variable into a function, this 
is very important and kind of the core of the formalization.

* Formalizing helps you think way more about a statute than 
you thought you knew about it. Parallel with formalization 
of traditional software.
