module Main exposing (main, newTodo)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type Msg
    = AddTodo
    | EditTodo TodoId String
    | ToggleTodo TodoId
    | DeleteTodo TodoId
    | DoneAll
    | UndoneAll


type alias Model =
    { todoList : List Todo
    , nextId : TodoId
    }


type TodoId
    = TodoId Int


type alias Todo =
    { id : TodoId
    , title : String
    , isDone : Bool
    }


generateNextTodoId : TodoId -> TodoId
generateNextTodoId todoId =
    case todoId of
        TodoId num ->
            TodoId (num + 1)


init : Model
init =
    { todoList = []
    , nextId = TodoId 1
     }


newTodo : TodoId -> Todo
newTodo newId =
    Todo newId "" False


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            { model
                | todoList = List.concat [ model.todoList, [ newTodo model.nextId ] 
                , [ newTodo model.nextId ] ]
                , nextId = generateNextTodoId model.nextId
            }

        EditTodo id newTitle ->
            let
                editTodo : Todo -> Todo
                editTodo todo =
                    if todo.id == id then
                        { todo | title = newTitle }

                    else
                        todo

                newTodoList =
                    model.todoList
                        |> List.map editTodo
            in
            { model
                | todoList = newTodoList
                , nextId = generateNextTodoId model.nextId
            }

        ToggleTodo id ->
            let
                toggleTodo : Todo -> Todo
                toggleTodo todo =
                    if todo.id == id then
                        { todo | isDone = not todo.isDone }

                    else
                        todo

                newTodoList =
                    model.todoList
                        |> List.map toggleTodo
            in
            { model
                | todoList = newTodoList
                , nextId = generateNextTodoId model.nextId
            }

        DeleteTodo id ->
            let
                isTarget : Todo -> Bool
                isTarget todo =
                    not (todo.id == id)

                newTodoList =
                    model.todoList
                        |> List.filter isTarget
            in
            { model
                | todoList = newTodoList
                , nextId = generateNextTodoId model.nextId
            }

        DoneAll ->
            let
                done : Todo -> Todo
                done todo =
                    { todo | isDone = True }

                newTodoList =
                    model.todoList |> List.map done
            in
            { model | todoList = newTodoList }

        UndoneAll ->
            let
                undone : Todo -> Todo
                undone todo =
                    { todo | isDone = False }

                newTodoList =
                    model.todoList |> List.map undone
            in
            { model | todoList = newTodoList }


view : Model -> Html Msg
view model =
    div []
        [ span []
            [ button [ onClick AddTodo ]
                [ text "add" ]
            , button [ onClick DoneAll ] [ text "DoneAll" ]
            , button [ onClick UndoneAll ] [ text "UndoneAll" ]
            ]
        , ul
            []
            (model.todoList |> List.map todoLine)
        ]


todoLine : Todo -> Html Msg
todoLine todo =
    li []
        [ span []
            [ input [ type_ "checkbox", checked todo.isDone, onClick (ToggleTodo todo.id) ] []
            , input [ type_ "text", value todo.title, onInput (EditTodo todo.id) ] []
            , button [ onClick (DeleteTodo todo.id) ] [ text "delete" ]
            ]
        ]
