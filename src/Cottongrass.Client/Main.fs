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
    type SiteConfig = YamlConfig<"templates/config.yml">

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

    let loadConfig () =
        async {
            use client = new HttpClient(BaseAddress = Uri("http://localhost:5000"))
            let reader = SiteConfig()
            let! data =
                "data/config.yml"
                |> client.GetStringAsync
                |> Async.AwaitTask
            reader.LoadText data
            return reader
        }

type Page =
    | Home
    | Consultation of shortcode: string
    | AnswerForm of shortcode: string

type Model =
    {
        page: Page
        SiteConfig: ConsultationConfig.SiteConfig option
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
        SiteConfig = None
        SelectedConsultation = None
        Error = None
    }

type Message =
    | SetPage of Page
    | Ping
    | LoadSiteConfig
    | LoadedSiteConfig of ConsultationConfig.SiteConfig
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
    | LoadSiteConfig -> 
        model, 
        Cmd.OfAsync.either
            ConsultationConfig.loadConfig ()
            (fun c -> LoadedSiteConfig c)
            Error
    | LoadedSiteConfig data ->
        { model with SiteConfig = Some data }, Cmd.none
    | Error e -> { model with Error = e.ToString() |> Some }, Cmd.none

let router = Router.infer SetPage (fun m -> m.page)

let homeView model dispatch =
    concat [
        if model.Error.IsSome then textf "Error: %A" model.Error.Value
        section [ attr.``class`` "hero is-medium is-link home-hero" ] [
            div [ attr.``class`` "hero-body" ] [
                cond model.SiteConfig <| function
                | Some sc ->
                    concat [
                        p [ attr.``class`` "title" ] [ text sc.Site.LongTitle ]
                        p [ attr.``class`` "subtitle" ] [ text sc.Site.Subtitle ]
                    ]
                | None -> p [ attr.``class`` "title" ] [ text "Loading..." ]
            ]
        ]
        section [ attr.``class`` "section"; attr.name "Consultation list" ] [
            div [ attr.``class`` "container" ] [
                cond model.SiteConfig <| function
                | Some sc -> p [] [ text sc.Site.IntroText ]
                | None -> empty
                h2 [ attr.``class`` "is-size-3" ] [ text "Open consultations" ]
                forEach model.Consultations <| fun c ->
                    div [ attr.``class`` "card" ] [
                        header [ attr.``class`` "card-header" ] [
                            p [ attr.``class`` "card-header-title" ] [
                                a [ router.HRef <| Consultation c.shortcode
                                    on.click (fun _ -> LoadConsultation c.shortcode |> dispatch) ] [ text c.title ]
                            ]
                        ]
                        div [ attr.``class`` "card-content" ] [ c.description |> text ]
                        div [ attr.``class`` "card-footer" ] [
                            a [ router.HRef <| Consultation c.shortcode
                                on.click (fun _ -> LoadConsultation c.shortcode |> dispatch) ] [ text "See more" ]
                        ]
                    ]
            ]
        ]
    ]

let detailView shortcode (active:ConsultationConfig.Consultation) model dispatch =
    concat [
        section [ attr.``class`` "hero is-primary"; attr.name "consultation-title" ] [
            div [ attr.``class`` "hero-body" ] [
                p [ attr.``class`` "title" ] [ text active.Title ]
            ]
        ]
        section [ attr.name "consultation-details" ] [
            div [ attr.``class`` "columns" ] [
                div [ attr.``class`` "column is-three-quarters" ] [
                    div [ attr.``class`` "block" ] [ text active.Description ]
                    p [] [ text active.Description ]
                    p [] [ textf "There are %i sections." active.Questions.Count]
                    div [ attr.``class`` "block" ] [
                        a [ 
                            attr.``class`` "button is-light is-primary is-medium"
                            router.HRef <| AnswerForm shortcode ] [
                            text "Start now"
                        ]
                    ]
                ]
                div [ attr.``class`` "column" ] [
                    article [ attr.``class`` "message" ] [
                        div [ attr.``class`` "message-body" ] [
                            h2 [] [ text "Who is consulting?" ]
                            p [] [ text "Bla bla bla "]
                            h2 [] [ text "Where can I find more information?" ]
                            p [] [ text "Bla bla bla..." ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let navbar model dispatch =
    nav [ attr.``class`` "navbar"; attr.aria "label" "main navigation" ] [
        div [ attr.``class`` "navbar-brand" ] [
            a [ attr.``class`` "navbar-item has-text-weight-bold is-size-5"; router.HRef Home ] [
                img [ attr.style "height:40px"; attr.src "/images/logo-small.png" ]
                text " Cottongrass"
            ]
        ]
    ]

/// The answer form renders form fields from yaml in the selected
/// language.
let answerFormView model dispatch =
    div [] [ text "This is an answer form!" ]


let view model dispatch =
    concat [
        navbar model dispatch
        cond model.page <| function
        | Home -> homeView model dispatch
        | Consultation _ ->
            cond model.SelectedConsultation <| function
            | None -> text "Loading consultation..."
            | Some active -> detailView "" (* TODO *) active model dispatch
        | AnswerForm code -> answerFormView model dispatch
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.batch [Cmd.ofMsg LoadIndex; Cmd.ofMsg LoadSiteConfig ]) update view
        |> Program.withRouter router