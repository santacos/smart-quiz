module Components.Question exposing (..)

import Html exposing (..)

type alias Question = {
  content: String
  , choices: List Choice
  , correctChoice: String
  , state: QuestionState
}

type alias Choice = String
type Result = Correct Choice | Incorrect Choice | Empty
type QuestionState = NotAnswered | Answered Choice | Validated Result


-- Helper

setSelectedChoice : Choice -> Question -> Question
setSelectedChoice selectedChoice question =
  { question | state = Answered selectedChoice }

validateQuestion : Question -> Question
validateQuestion question =
  case question.state of
    Answered choice -> { question | state = Validated (checkAnswer question) }
    _ -> { question | state = Validated Empty }

checkAnswer : Question -> Result
checkAnswer question =
  case question.state of
    Answered choice -> if choice == question.correctChoice then Correct choice else Incorrect choice
    _ -> Empty


isSelectedChoice : Choice -> QuestionState -> Bool
isSelectedChoice choice state =
    case state of
        Answered selectedChoice -> choice == selectedChoice
        Validated (Correct selectedChoice) -> choice == selectedChoice
        Validated (Incorrect selectedChoice) -> choice == selectedChoice
        _ -> False


-- View

displayResult : Question -> Html msg
displayResult question =
  let result =
    case question.state of
      Validated (Correct answer) -> "Congratulations!! The answer is correct."
      Validated (Incorrect answer) -> "Sorry! The answer is wrong."
      Validated Empty -> "Please select the answer first!"
      _ -> ""
            
  in 
    h5 [] [text result]
