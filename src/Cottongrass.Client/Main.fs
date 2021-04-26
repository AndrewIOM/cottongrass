module Cottongrass.Client.Main

open Elmish
open Bolero
open Bolero.Html

/// Loads all YML files that contain individual consultations.
module ConsultationConfig =

    open FSharp.Configuration
    open System
    open System.Net.Http

    type Consultation = YamlConfig<"templates/consultation.yml">
    type Index = YamlConfig<"templates/index.yml">

    let loadIndex () =
        async {
            use client = new HttpClient(BaseAddress = Uri("http://localhost:5000"))
            let reader = Index ()
            let! data =
                "data/consultations/index.yml"
                |> client.GetStringAsync
                |> Async.AwaitTask
            reader.LoadText data
            return reader.consultations |> seq
        }

    let loadConsultation shortcode =
        async {
            use client = new HttpClient(BaseAddress = Uri("http://localhost:5000"))
            let reader = Consultation()
            let! data =
                ("data/consultations/" + shortcode + ".yml")
                |> client.GetStringAsync
                |> Async.AwaitTask
            reader.LoadText data
            return reader
        }

type Page =
    | Home
    | Consultation of shortcode: string

type Model =
    {
        page: Page
        Consultations: ConsultationConfig.Index.consultations_Item_Type list
        SelectedConsultation: ConsultationConfig.Consultation option
        x: string
        Error: string option
    }

let initModel =
    {
        page = Home
        x = ""
        Consultations = []
        SelectedConsultation = None
        Error = None
    }

type Message =
    | SetPage of Page
    | Ping
    | LoadIndex
    | LoadedIndex of ConsultationConfig.Index.consultations_Item_Type list
    | LoadConsultation of string
    | LoadedConsultation of ConsultationConfig.Consultation
    | Error of exn

let update message model =
    match message with
    | SetPage page -> { model with page = page }, Cmd.none
    | Ping -> model, Cmd.none
    | LoadIndex -> 
        model, 
        Cmd.OfAsync.either
            ConsultationConfig.loadIndex ()
            (fun items -> LoadedIndex (items |> Seq.toList))
            Error
    | LoadedIndex items -> { model with Consultations = items }, Cmd.none
    | LoadConsultation shortcode -> 
        model, 
        Cmd.OfAsync.either
            ConsultationConfig.loadConsultation shortcode
            (fun c -> LoadedConsultation c)
            Error
    | LoadedConsultation data ->
        { model with SelectedConsultation = Some data }, Cmd.none
    | Error e -> { model with Error = e.ToString() |> Some }, Cmd.none

let router = Router.infer SetPage (fun m -> m.page)

let homeView model dispatch =
    div [] [
        text "Hello, world!"
        if model.Error.IsSome then textf "Error: %A" model.Error.Value
        forEach model.Consultations <| fun c ->
            div [] [
                a [ router.HRef <| Consultation c.shortcode
                    on.click (fun _ -> LoadConsultation c.shortcode |> dispatch) ] [ text c.title ]
            ]
    ]

let view model dispatch =
    cond model.page <| function
    | Home -> homeView model dispatch
    | Consultation _ ->
        cond model.SelectedConsultation <| function
        | None -> text "Loading consultation..."
        | Some active ->
            div [] [ 
                h2 [] [ text active.Title ]
                p [] [ text active.Description ]
                p [] [ textf "There are %i sections." active.Questions.Count]
            ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg LoadIndex) update view
        |> Program.withRouter router