# Guess-Animals
Guess-Animals is a Racket program that uses the ID3 machine learning algorithm to classify animals

The functions of the code and its technicalities are documented within the program :)

--------------------------------------------------------------------------------------------------
Common questions:

Q: How is the decision tree built? 

A: It is built with the ID3 algorithm which uses entropy to determine the likeliness of each animal 
   having a certain trait.
   
   An explanation of the ID3 algorithm and entropy can be found here: https://en.wikipedia.org/wiki/ID3_algorithm
   
Q: Will the decision tree always be the same given the random animals?

A: No, not always. Due to the nature of randomness there might be slight deviations in the decision trees.
   However, due to the large sample size, the deviations should be very rare and the tests should always pass
