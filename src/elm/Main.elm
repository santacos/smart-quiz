module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Components.Question exposing (..)


-- APP
main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type Page = WelcomePage | QuestionPage
type alias Model = { question: Question, currentPage: Page }

model : Model
model = {
    question = Question "What is your name?" ["Cos", "Dog", "Green", "Red"] "Cos" Components.Question.NotAnswered
    , currentPage = WelcomePage
  }


-- UPDATE

type Msg = CheckAnswer | Choose Choice | StartQuiz

update : Msg -> Model -> Model
update msg model =
  case msg of
    CheckAnswer -> { model | question = validateQuestion model.question }
    Choose choice -> { model | question = setSelectedChoice choice model.question }
    StartQuiz -> { model | currentPage = QuestionPage }


-- View

view : Model -> Html Msg
view model =
  div [ class "container", style [("margin-top", "30px"), ( "text-align", "center" )] ][
    div [ class "row" ][
      div [ class "col-xs-12" ][
        contentView model
      ]
    ]
  ]

contentView : Model -> Html Msg
contentView model =
  case model.currentPage of
    WelcomePage -> welcomeView
    QuestionPage -> questionView model

questionView : Model -> Html Msg
questionView model =
  div [ class "quiz" ][
    h3 [class "question"] [text model.question.content]
    , div [class "list-group"] <| List.map (\choice -> choiceButton choice model.question.state) model.question.choices
    , displayResult model.question
    , checkButton model.question.state
  ]

welcomeView : Html Msg
welcomeView =
  div [ class "quiz" ][
    h3 [class "question"] [text "Welcome to SMART QUIZ SYSTEM"]
    , button [class "check", onClick StartQuiz] [text "START"]
  ]

checkButton : QuestionState -> Html Msg
checkButton state =
  button [
          class "check"
          , classList [("disabled", state == NotAnswered)]
          , onClick CheckAnswer
          ] [text "Check"]

choiceButton : Choice -> QuestionState -> Html Msg
choiceButton choice state =
    li [
        class "choice"
        , classList [("active", isSelectedChoice choice state)]
        , onClick (Choose choice)
      ] [text choice]