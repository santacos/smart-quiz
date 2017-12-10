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
  , selectedChoice: Maybe Choice
  , state: QuestionState
}

type QuestionState = NotAnswered | Answered Choice | Validated Bool

type alias Model = { score: Int, question: Question, isAnswerCorrect: Maybe Bool }

-- type alias Model = { questions: List Question, focusQuestion: }

model : Model
model = {
  score = 0
  , isAnswerCorrect = Nothing
  , question = Question "What is your name?" ["Cos", "Dog", "Green", "Red"] "Cos" Nothing NotAnswered
  }

-- Question 

setSelectedChoice : Maybe Choice -> Question -> Question
setSelectedChoice selectedChoice question =
  case selectedChoice of
    Just choice -> { question | state = Answered choice }
    Nothing -> question


checkAnswer : Question -> Bool
checkAnswer question =
  case question.state of
    Answered choice -> if choice == question.correctChoice then True else False
    _ -> False

updateScore : Int -> Question -> Int
updateScore previousScore question =
  if checkAnswer question then previousScore + 1
  else previousScore

-- UPDATE
type Msg = NoOp | CheckAnswer | Choose Choice

update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp -> model
    CheckAnswer -> {
      model | score = updateScore model.score model.question, isAnswerCorrect = Just (checkAnswer model.question)
    }
    Choose choice -> { model | question = setSelectedChoice (Just choice) model.question }


-- View

view : Model -> Html Msg
view model =
  div [ class "container", style [("margin-top", "30px"), ( "text-align", "center" )] ][
    div [ class "row" ][
      div [ class "col-xs-12" ][
        div [ class "quiz" ][
          h3 [class "question"] [text model.question.content]
          , viewResult model.isAnswerCorrect
          , div [class "list-group"] <| List.map (\choice -> choiceButton choice model.question.state) model.question.choices
          , button [class "check", onClick CheckAnswer] [text "Check"]
        ]
      ]
    ]
  ]

viewResult : Maybe Bool -> Html msg
viewResult show =
  let result = case show of
            Just isShow -> if isShow then "The answer is correct!" else "Wrong!"
            Nothing -> ""
  in 
    h5 [] [text result]

choiceButton : Choice -> QuestionState -> Html Msg
choiceButton choice state =
  let
    isSelected = 
      case state of
        Answered selectedChoice -> choice == selectedChoice
        _ -> False
  in
    li [class "choice", classList [("active", isSelected)], onClick (Choose choice)] [text choice]