# relationMCQDSL


### Project Goal
Goal of this project is to create a domain specific language (DSL) that lets domain expert to represent knowledge i.e. related objects and their characteristics through mathematical relations. Compositional operations on relations facilitate user to describe complicated (higher order) relationships. DSL also includes constructs to create standard multiple choice question (MCQ) or quiz questions over user described relations. Thus, people from aforementioned domains or fields interested in creating MCQ questions can use this DSL can write a program describing related objects and create MCQ questions and executing the program will generate set of quiz questions. Quiz questions can be printed in any media of user choice. However, an execution function has been provided which generates interactive quiz and takes students response and display results at the end.

For details: [See Project Report](https://github.com/faribaK/relationMCQDSL/blob/master/project%20report/document.pdf)


### How to run it
Download or clone the repository and navigate into the relationMCQDSL folder from command prompt.

If you have [stack](https://docs.haskellstack.org/en/stable/README/) installed:

Run `stack build`
    
Now you can run the quiz `finalQuiz` created in the example (SimpleExample.hs) program written in DSL and take interactive quiz
    
Run `stack exec relationMCQDSL-exe`

Or, you can write your own program in DSL and run your created quiz by replacing `finalQuiz` in `Main.hs` with your quiz name (quiz has type Question a b)

This will display quiz questions one by one and prompt for answer.

Once you are done with all questions, result will be printed.
   
If you do not have stack, you can load `src/SimpleExample.hs` in GHCi and run `runStateT (execQuiz finalQuiz) 0 >>= 
       \(w, _) -> return (runWriter w)  >>= 
       \(a, s) -> putStr (s ++ "\n Total Score: " ++ (show a)++"\n") `.

Project Report includes several example expressions in section 4.3 Functions, which can be copied into GHCi after loading `src/SimpleExample.hs` and see values in action 
