# relationMCQDSL is a DSL to represent domain knowledge with relations and create MCQ quiz from them.

### Project Goal
Goal of this project is to create a domain specific language (DSL) that lets domain expert to represent knowledge i.e. related objects and their characteristics through mathematical relations. Compositional operations on relations facilitate user to describe complicated (higher order) relationships. DSL also includes constructs to create standard multiple choice question (MCQ) or quiz questions over user described relations. Thus, people from aforementioned domains or fields interested in creating MCQ questions can use this DSL can write a program describing related objects and create MCQ questions and executing the program will generate set of quiz questions with answer keys. Quiz questions can be printed in any media of user choice. However, an execution function has been provided which generates interactive quiz and takes students response and check response correctness against the answer keys.


### How to run it
Download or clone the repository and navigate into the relationMCQDSL folder from command prompt.

If you have [stack](https://docs.haskellstack.org/en/stable/README/) installed:

Run `stack build`
    
Now you can run an simple program written in DSL and take interactive quiz
    
Run `stack exec relationMCQDSL-exe filename`

This will display quiz questions one by one and prompt for answer.

Once you are done with all questions, it will display the result.
   
If you do not have stack, you can load Sim into `src/SimpleExample.hs` in ghci and run "execQuiz finalQuiz".

### Design questions
1. I want to randomize the MCQ options. This can be done when options are displayed which makes it easier to hadle randomIO. But I was wondering if I randomize the list and store it in a plain list so that user can later do whatever they want with the list.  
2. In `QuesGeneratorV3.hs`, `Statement` is generated from a `Relation` through smart constructor, then Answers and Distractors are genrated from Statement and Relation. This later Relation should be the one from which Statement has been generated. I am thinking about enforcing that somehoe without overloading datat type i.e. make Relation part of Statement but do it in function level. Should I think about computation monad to achieve this?
