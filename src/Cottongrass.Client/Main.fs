module Cottongrass.Client.Main

open Elmish
open Bolero
open Bolero.Html

type Page =
    | Home
    | Consultation of shortcode: string
    | AnswerForm of shortcode: string * section: int

type Model =
    {
        page: Page
        SiteConfig: ConsultationConfig.SiteConfig option
        Consultations: ConsultationConfig.Index.consultations_Item_Type list
        SelectedConsultation: ActiveConsultation option
        LanguageCode: string
        Error: string option
    }

and ActiveConsultation = {
    ShortCode: string
    Config: ConsultationConfig.Consultation
    CurrentSection: int
    Answers: Map<int*int,Form.DynamicQuestionAnswer>
}

let initModel =
    {
        page = Home
        Consultations = []
        SiteConfig = None
        SelectedConsultation = None
        LanguageCode = "en"
        Error = None
    }

type Message =
    | SetPage of Page
    | LoadSiteConfig
    | LoadedSiteConfig of ConsultationConfig.SiteConfig
    | LoadIndex
    | LoadedIndex of ConsultationConfig.Index.consultations_Item_Type list
    | LoadConsultation of string
    | LoadedConsultation of ConsultationConfig.Consultation * string
    | Error of exn
    | SetDynamicAnswer of int * int * Form.DynamicQuestionAnswer // Section * Question * Answer

let update message model =
    match message with
    | SetPage page -> { model with page = page }, Cmd.none
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
            (fun c -> LoadedConsultation (c,shortcode))
            Error
    | LoadedConsultation (data,sc) ->
        { model with SelectedConsultation = Some { Config = data; CurrentSection = 1; ShortCode = sc; Answers = Map.empty } }, Cmd.none
    | LoadSiteConfig -> 
        model, 
        Cmd.OfAsync.either
            ConsultationConfig.loadConfig ()
            (fun c -> LoadedSiteConfig c)
            Error
    | LoadedSiteConfig data ->
        { model with SiteConfig = Some data }, Cmd.none
    | Error e -> { model with Error = e.ToString() |> Some }, Cmd.none
    | SetDynamicAnswer(section,question,answer) -> 
        match model.SelectedConsultation with
        | None -> model, Cmd.none
        | Some c -> { model with SelectedConsultation = Some { c with Answers = c.Answers |> Map.add (section,question) answer } }, Cmd.none
        
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

let heroHeading title subtitle =
    section [ attr.``class`` "hero is-primary" ] [
        div [ attr.``class`` "hero-body" ] [
            p [ attr.``class`` "title" ] [ text title ]
            if Option.isSome subtitle then p [ attr.``class`` "subtitle" ] [ text subtitle.Value ]
        ]
    ]

let detailView shortcode (active:ConsultationConfig.Consultation) model dispatch =
    concat [
        heroHeading active.Title None
        section [ attr.name "consultation-details" ] [
            div [ attr.``class`` "columns" ] [
                div [ attr.``class`` "column is-three-quarters" ] [
                    div [ attr.``class`` "block" ] [ text active.Description ]
                    p [] [ text active.Description ]
                    p [] [ textf "There are %i sections." active.Questions.Count]
                    div [ attr.``class`` "block" ] [
                        a [ 
                            attr.``class`` "button is-light is-primary is-medium"
                            router.HRef <| AnswerForm (shortcode,1) ] [
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

        // Use conditional elements on model properties?

        // - INPUT: 
        // What do we need to compile?
        // - A node that contains all rendered fields
        // - Fields have actions attached to update model values

        // let formSection languageCode model =
        //     binaryChoice "" "" "" false
        //     |> lift
        //     |> andDependentQuestion (textQuestion "Label" ["placeholder"] "value") ((=) true)
        //     |> andQuestion (textQuestion "Label" ["placeholder"] "value")
        //     |> compile




let aboutYouSection title dispatch =
    concat [
        // Render some form elements
        form [] [
            // Form.textField "First name" "" None
            // Form.textField "Last name" "" None

            //if Option.isSome model.Organisation

            // Conditional - organisation name?
        ]
    ]

/// The answer form renders form fields from yaml in the selected
/// language.
let answerFormView shortcode section (model:Model) dispatch =
    cond model.SelectedConsultation <| function
    | None -> div [] [ text "Loading..." ]
    | Some con ->
        let currentSection = con.Config.Questions |> Seq.tryItem (section - 2)
        let subtitle =
            if section = 1 then "About You" |> Some
            else if currentSection.IsSome
            then (currentSection.Value.Name |> Seq.find(fun (d:ConsultationConfig.Consultation.Questions_Item_Type.Name_Item_Type) -> d.Language = model.LanguageCode)).Translation |> Some
            else None
        let description =
            if currentSection.IsSome
            then (currentSection.Value.Description |> Seq.find(fun (d:ConsultationConfig.Consultation.Questions_Item_Type.Description_Item_Type) -> d.Language = model.LanguageCode)).Translation |> Some
            else None
        concat [
            heroHeading con.Config.Title subtitle
            // Each answer form has sections of questions.
            // Display a sidebar containing the section progress.
            div [ attr.``class`` "container" ] [
                div [ attr.``class`` "columns" ] [
                    div [ attr.``class`` "column is-one-quarter" ] [
                        ol [] [
                            concat [
                                li [ on.click (fun _ -> AnswerForm(shortcode,1) |> SetPage |> dispatch ) ] [ text "About you" ]
                                forEach [1 .. con.Config.Questions.Count] (fun i -> li [ on.click (fun _ -> AnswerForm(shortcode,i+1) |> SetPage |> dispatch ) ] [ text (con.Config.Questions.[i-1].Name |> Seq.find(fun d -> d.Language = model.LanguageCode)).Translation ])
                            ]
                        ]
                    ]
                    div [ attr.``class`` "column" ] [
                        if description.IsSome then p [] [ text description.Value ]
                        if section = 1 
                        then aboutYouSection con.Config.Title dispatch
                        else 
                            cond (con.Config.Questions |> Seq.tryItem (section - 2)) <| function
                            | None -> text "Error"
                            | Some s -> Form.Parser.parseYaml "en" section (s.Questions |> Seq.toList) con.Answers (SetDynamicAnswer >> dispatch)
                        button [ attr.``class`` "button"; on.click (fun _ -> AnswerForm(shortcode,section+1) |> SetPage |> dispatch ) ] [ text "Next" ]
                    ]
                ]
            ]
        ]


let view model dispatch =
    concat [
        navbar model dispatch
        cond model.page <| function
        | Home -> homeView model dispatch
        | Consultation _ ->
            cond model.SelectedConsultation <| function
            | None -> text "Loading consultation..."
            | Some con -> detailView con.ShortCode con.Config model dispatch
        | AnswerForm (code,section) -> answerFormView code section model dispatch
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.batch [Cmd.ofMsg LoadIndex; Cmd.ofMsg LoadSiteConfig ]) update view
        |> Program.withRouter router
        |> Program.withConsoleTrace