module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )


-- APP
main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Choice = String
type alias Question = {
  content: String
  , choices: List Choice
  , correctChoice: String
  , state: QuestionState
}

type Result = Correct Choice | Incorrect Choice | Empty

type QuestionState = NotAnswered | Answered Choice | Validated Result

type alias Model = { score: Int, question: Question }

-- type alias Model = { questions: List Question, focusQuestion: }

model : Model
model = {
  score = 0
  , question = Question "What is your name?" ["Cos", "Dog", "Green", "Red"] "Cos" NotAnswered
  }

-- Question 

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

updateScore : Int -> Question -> Int
updateScore previousScore question =
  case question.state of
    Validated (Correct answer) -> previousScore + 1
    _ -> previousScore

-- UPDATE

type Msg = NoOp | CheckAnswer | Choose Choice

update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp -> model
    CheckAnswer -> {
      model | question = validateQuestion model.question
      , score = updateScore model.score model.question
    }
    Choose choice -> { model | question = setSelectedChoice choice model.question }


-- View

view : Model -> Html Msg
view model =
  div [ class "container", style [("margin-top", "30px"), ( "text-align", "center" )] ][
    div [ class "row" ][
      div [ class "col-xs-12" ][
        div [ class "quiz" ][
          h3 [class "question"] [text model.question.content]
          , div [class "list-group"] <| List.map (\choice -> choiceButton choice model.question.state) model.question.choices
          , viewResult model.question
          , button [class "check", onClick CheckAnswer] [text "Check"]
        ]
      ]
    ]
  ]

viewResult : Question -> Html msg
viewResult question =
  let result =
    case question.state of
      Validated (Correct answer) -> "Congratulations!! The answer is correct."
      Validated (Incorrect answer) -> "Sorry! The answer is wrong."
      Validated Empty -> "Please select the answer first!"
      _ -> ""
            
  in 
    h5 [] [text result]

choiceButton : Choice -> QuestionState -> Html Msg
choiceButton choice state =
  let
    isSelected = 
      case state of
        Answered selectedChoice -> choice == selectedChoice
        Validated (Correct selectedChoice) -> choice == selectedChoice
        Validated (Incorrect selectedChoice) -> choice == selectedChoice
        _ -> False
  in
    li [class "choice", classList [("active", isSelected)], onClick (Choose choice)] [text choice]