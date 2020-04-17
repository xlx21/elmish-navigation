module Client

open Fulma
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Elmish.Navigation

module HomePage =
    type Model = { Title : string }
    let init () = { Title = "Welcome! You're in HOME PAGE. " }
    let view model dispatch = Content.content [] [ str model.Title ]

module AddressPage =
    type Model =
        { BuildingNo : int
          Street : string
          City : string
          Postcode : string }

    let init () =
      { BuildingNo = 41
        City = "London"
        Postcode = "P21 1XX"
        Street = "Liverpool St" }

    let view model dispatch =
        Content.content [] [
            str "Address Page"
            str (sprintf "Building No: %d" model.BuildingNo)
            str (sprintf "Street: %s" model.Street)
            str (sprintf "City: %s" model.City)
            str (sprintf "Postcode: %s" model.Postcode)
        ]

module PersonPage =
    type Model = { Name : string }
    let init fullName = { Name = fullName }
    let view model dispatch =
        Content.content [] [
            str "Person Details"
            str (sprintf "Full Name: %s" model.Name)
        ]

type Page =
    | HomePage
    | AddressPage
    | PersonPage of string

type SubModel =
    | HomePageModel of HomePage.Model
    | AddressPageModel of AddressPage.Model
    | PersonPageModel of PersonPage.Model

type Model =
    { NameEntry: string
      CurrentPage: Page
      SubModel: SubModel }

type UpdatePersonName = UpdatePersonName of string

let init page : Model * Cmd<UpdatePersonName> =
    let page = page |> Option.defaultValue HomePage

    let subModel =
        match page with
        | HomePage -> HomePageModel (HomePage.init ())
        | AddressPage -> AddressPageModel (AddressPage.init ())
        | PersonPage name -> PersonPageModel (PersonPage.init name)

    { CurrentPage = page
      SubModel = subModel
      NameEntry = "" }, Cmd.none

let update msg model =
    match msg with
    | UpdatePersonName name -> { model with NameEntry = name }, Cmd.none

let view (model : Model) (dispatch : UpdatePersonName -> unit) =
    Container.container [] [
        Label.label [] [ str "Name" ]
        Input.text [
            Input.Value model.NameEntry
            Input.Props [ OnChange (fun ev -> dispatch (UpdatePersonName !!ev.target?value)) ]
        ]

        let href = sprintf "#person/%s" model.NameEntry

        Button.a [ Button.Props [ Href href ] ] [ str "Person Page" ]
        Button.a [ Button.Props [ Href "#home" ] ] [ str "Home Page" ]
        Button.a [ Button.Props [ Href "#address" ] ] [ str "Address Page" ]

        match model.SubModel with
        | HomePageModel m -> HomePage.view m dispatch
        | AddressPageModel m -> AddressPage.view m dispatch
        | PersonPageModel m -> PersonPage.view m dispatch
    ]

module Navigation =
    open Elmish.UrlParser

    let pageParser : Parser<_,_> =
        oneOf [
            map HomePage (s "home")
            map AddressPage (s "address")
            map PersonPage (s "person" </> str)
        ]

    let urlUpdate (page: Page option) _ =
        let page = page |> Option.defaultValue HomePage
        let model, _ = Some page |> init
        { model with CurrentPage = page }, Cmd.none

Program.mkProgram init update view
|> Program.toNavigable (UrlParser.parseHash Navigation.pageParser) Navigation.urlUpdate
|> Program.withReactBatched "elmish-app"
|> Program.run
