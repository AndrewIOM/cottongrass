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
    | AnswerForm of shortcode: string * section: int

type DynamicQuestionAnswer =
    | BinaryChoice of bool
    | Text of string
    | Choice of string

type Model =
    {
        page: Page
        SiteConfig: ConsultationConfig.SiteConfig option
        Consultations: ConsultationConfig.Index.consultations_Item_Type list
        SelectedConsultation: (ConsultationConfig.Consultation * string * int) option
        LanguageCode: string
        Error: string option
    }

let initModel =
    {
        page = Home
        Consultations = []
        SiteConfig = None
        SelectedConsultation = None
        LanguageCode = "en-GB"
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
    | SetDynamicAnswer of DynamicQuestionAnswer // Bind question answers from dynamic model (in a map?)

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
        { model with SelectedConsultation = Some (data,sc,1) }, Cmd.none
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

module Form =

    let field labelText contents =
        div [ attr.``class`` "field" ] [
            label [ attr.``class`` "label" ] [ text labelText ]
            contents
        ]

    let textField labelText placeholder helpText bindHole =
        field labelText (concat [ 
            div [ attr.``class`` "control" ] [
            input [ attr.``class`` "input"
                    attr.``type`` "text"
                    attr.placeholder placeholder
                    bindHole ]
            ]
            if Option.isSome helpText
            then p [ attr.``class`` "help" ] [ text helpText.Value ]
        ])

    let optionField id labelText optionNames bindHole =
        field labelText (concat [
            div [ attr.``class`` "control" ] [
                forEach optionNames <| fun opt ->
                    label [ attr.``class`` "radio" ] [
                        input [ attr.``type`` "radio"
                                attr.name id
                                bindHole ]
                        text opt
                    ]
            ]
        ])

    module Parser =

        type OptionField = {
            Options: string list
            Selected: string
        }

        type Question = {
            Answer: DynamicQuestionAnswer
            View: Attr -> Node
            ChangeHandler: Attr 
        }

        type QuestionSectionBuilder = {
            CurrentQ: Question * bool // indicates if visible
            PreviousQs: Node
            PreviousQuestionModel: Map<int,Question>
        }

        let lift question = { CurrentQ = question, true; PreviousQs = empty; PreviousQuestionModel = Map.empty }

        let binaryChoice name question labelOne labelTwo value =
            { View = optionField name question [ labelOne; labelTwo ]
              Answer = BinaryChoice value
              ChangeHandler = bind.``checked`` value ignore }

        let choiceQuestion name question choices value =
            { View = optionField name question choices
              Answer = Choice value
              ChangeHandler = bind.change.string value ignore }

        let textQuestion label placeholder value =
            { View = textField label placeholder None
              Answer = Text value
              ChangeHandler = bind.input.string value ignore }

        let andDependentQuestion (nextQuestion:Question) condition (builder:QuestionSectionBuilder) =
            let display = condition (fst builder.CurrentQ)
            { CurrentQ = nextQuestion, display
              PreviousQs = (fst builder.CurrentQ).View (fst builder.CurrentQ).ChangeHandler
              PreviousQuestionModel = Map.add 1 nextQuestion builder.PreviousQuestionModel }

        let compile builder =
            concat [
                builder.PreviousQs
                if snd builder.CurrentQ then (fst builder.CurrentQ).View (fst builder.CurrentQ).ChangeHandler
            ]

        let andQuestion nextQuestion builder =
            let n = builder.PreviousQuestionModel.Count + 1
            { CurrentQ = nextQuestion, true
              PreviousQs = compile builder
              PreviousQuestionModel = builder.PreviousQuestionModel |> Map.add n nextQuestion }

        let parseQuestion identifier langCode (q:ConsultationConfig.Consultation.Questions_Item_Type.Questions_Item_Type) =
            let qText =
                match q.Question |> Seq.tryFind(fun q -> q.Language = langCode) with
                | Some q -> q.Translation
                | None -> failwith "No translation for question found"
            match q.Type with
            | "binary choice" ->
                let labels =
                    match q.Choices |> Seq.tryFind(fun q -> q.Language = langCode) with
                    | Some choices -> choices.Translation
                    | None -> failwith "No translation for choices found"
                if labels.Count <> 2 then failwith "Binary choice must have two options"
                binaryChoice (sprintf "q-%i" identifier) qText labels.[0] labels.[1] false
            | "choice" ->
                let labels =
                    match q.Choices |> Seq.tryFind(fun q -> q.Language = langCode) with
                    | Some choices -> choices.Translation
                    | None -> failwith "No translation for choices found"
                choiceQuestion (sprintf "q-%i" identifier) qText labels ""
            | "freetext"
            | _ -> textQuestion qText "" ""

        // Test using yaml:

        let rec parseYaml' langCode (remainingQs:ConsultationConfig.Consultation.Questions_Item_Type.Questions_Item_Type list) (builder:QuestionSectionBuilder option) =
            match Seq.isEmpty remainingQs with
            | true -> 
                if Option.isSome builder
                then builder.Value |> compile
                else empty
            | false ->
                let question = parseQuestion (remainingQs.Length) langCode (remainingQs |> Seq.head)
                if Option.isNone builder
                then question |> lift |> Some |> parseYaml' langCode remainingQs.Tail
                else builder |> Option.map (andQuestion question) |> parseYaml' langCode remainingQs.Tail

        // We need to connect it to a current model?
        // Can we construct the model dynamically as a dictionary?
        // NB: Does this run every time the view needs to render?
        let parseYaml language section =
            parseYaml' language section None
            

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

let questionSection title (section:ConsultationConfig.Consultation.Questions_Item_Type) =
    concat [
        Form.Parser.parseYaml "en" (section.Questions |> Seq.toList)
    ]

/// The answer form renders form fields from yaml in the selected
/// language.
let answerFormView shortcode section (model:Model) dispatch =
    cond model.SelectedConsultation <| function
    | None -> div [] [ text "Loading..." ]
    | Some (con,_,_) ->
        let currentSection = con.Questions |> Seq.tryItem (section - 2)
        let subtitle =
            if section = 1 then "About You" |> Some
            else if currentSection.IsSome
            then (currentSection.Value.Name |> Seq.find(fun (d:ConsultationConfig.Consultation.Questions_Item_Type.Name_Item_Type) -> d.Language = "en")).Translation |> Some
            else None
        let description =
            if currentSection.IsSome
            then (currentSection.Value.Description |> Seq.find(fun (d:ConsultationConfig.Consultation.Questions_Item_Type.Description_Item_Type) -> d.Language = "en")).Translation |> Some
            else None
        concat [
            heroHeading con.Title subtitle
            // Each answer form has sections of questions.
            // Display a sidebar containing the section progress.
            div [ attr.``class`` "container" ] [
                div [ attr.``class`` "columns" ] [
                    div [ attr.``class`` "column is-one-quarter" ] [
                        ol [] [
                            concat [
                                li [ on.click (fun _ -> AnswerForm(shortcode,1) |> SetPage |> dispatch ) ] [ text "About you" ]
                                forEach [1 .. con.Questions.Count] (fun i -> li [ on.click (fun _ -> AnswerForm(shortcode,i+1) |> SetPage |> dispatch ) ] [ text (con.Questions.[i-1].Name |> Seq.find(fun d -> d.Language = "en")).Translation ])
                            ]
                        ]
                    ]
                    div [ attr.``class`` "column" ] [
                        if description.IsSome then p [] [ text description.Value ]
                        if section = 1 
                        then aboutYouSection con.Title dispatch
                        else con.Questions |> Seq.tryItem (section - 2) |> (fun o -> if o.IsSome then questionSection con.Title o.Value else text "Error")
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
            | Some (active,sc,_) -> detailView sc active model dispatch
        | AnswerForm (code,section) -> answerFormView code section model dispatch
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.batch [Cmd.ofMsg LoadIndex; Cmd.ofMsg LoadSiteConfig ]) update view
        |> Program.withRouter router