module Cottongrass.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Microsoft.JSInterop

type Page =
    | Home
    | Consultation of shortcode: string
    | AnswerForm of shortcode: string * section: int
    | ThankYou

type Model =
    {
        page: Page
        SiteConfig: ConsultationConfig.SiteConfig option
        Consultations: ConsultationConfig.Index.consultations_Item_Type list
        SelectedConsultation: ActiveConsultation option
        CultureCode: string
        Error: string option
        BaseUri: System.Uri
    }

and ActiveConsultation = {
    ShortCode: string
    Config: ConsultationConfig.Consultation
    CurrentSection: int
    Answers: Map<int*int,Form.DynamicQuestionAnswer>
    RequiredQuestions: int list
}

and Required =
    | Required
    | NotRequired

let initialCulture = "en-GB"

let initModel baseUri =
    {
        page = Home
        Consultations = []
        SiteConfig = None
        SelectedConsultation = None
        CultureCode = initialCulture
        Error = None
        BaseUri = baseUri
    }

type Message =
    | SetPage of Page
    | SetLanguage of string
    | GetBrowserLanguage
    | GotBrowserLanguages of string[]
    | LoadSiteConfig
    | LoadedSiteConfig of ConsultationConfig.SiteConfig
    | LoadIndex
    | LoadedIndex of ConsultationConfig.Index.consultations_Item_Type list
    | LoadConsultation of string
    | LoadedConsultation of ConsultationConfig.Consultation * string
    | Error of exn
    | CheckValidation of int
    | SetDynamicAnswer of int * int * Form.DynamicQuestionAnswer // Section * Question * Answer
    | SendCompletedAnswers
    | SentCompletedAnswers

let currentLanguage (cultureCode:string) = cultureCode.Split("-").[0]
let currentCountry (cultureCode:string) = cultureCode.Split("-").[1]

module Markdown =

    open Markdig

    let pipeline = MarkdownPipelineBuilder().UseAdvancedExtensions().Build()
    let htmlFromMarkdown (str:string) =
        Markdown.ToHtml(str, pipeline) |> RawHtml

let translateStr english (config:ConsultationConfig.SiteConfig option) cultureCode : string =
    match config with
    | None -> english
    | Some sc ->
        let lang = currentLanguage cultureCode
        match (cultureCode = initialCulture) with
        | true -> english
        | false -> 
            match (sc.Translations |> Seq.tryFind(fun x -> x.It = english && x.In = lang)) with
            | Some t -> t.Is
            | None -> english

/// Get strings from translation lookup
let translate english (config:ConsultationConfig.SiteConfig option) cultureCode : Node =
    cond config <| function
    | None -> text english
    | Some sc ->
        let lang = currentLanguage cultureCode
        cond (cultureCode = initialCulture) <| function
        | true -> text english
        | false -> 
            cond (sc.Translations |> Seq.tryFind(fun x -> x.It = english && x.In = lang)) <| function
            | Some t -> text t.Is
            | None -> text english

let update (js: IJSRuntime) message model =
    match message with
    | SetPage page -> 
        { model with page = page },
        match page with
        | AnswerForm (_,section) -> Cmd.ofMsg (CheckValidation section)
        | _ -> Cmd.none
    | SetLanguage l -> 
        { model with CultureCode = l }, Cmd.none
    | GetBrowserLanguage ->
        let cmd = Bolero.Remoting.Client.Cmd.OfJS.either js "navigatorLanguages" [||] GotBrowserLanguages Error
        model, cmd
    | GotBrowserLanguages languages ->
        match model.SiteConfig with
        | Some sc ->
            let available = sc.Languages |> Seq.map(fun l -> l, currentLanguage l)
            let overlapping = 
                languages 
                |> Seq.where(fun l -> available |> Seq.map fst |> Seq.contains l || available |> Seq.map snd |> Seq.contains l)
                |> Seq.map(fun l -> available |> Seq.find(fun (c,a) -> c = l || a = l) |> fst)
            if overlapping |> Seq.isEmpty 
            then model, Cmd.none
            else model, Cmd.ofMsg (SetLanguage (overlapping |> Seq.head))
        | None -> model, Cmd.none
    | LoadIndex -> 
        model, 
        Cmd.OfAsync.either
            ConsultationConfig.loadIndex model.BaseUri
            (fun items -> LoadedIndex (items |> Seq.toList))
            Error
    | LoadedIndex items -> { model with Consultations = items }, Cmd.none
    | LoadConsultation shortcode -> 
        model, 
        Cmd.OfAsync.either
            ConsultationConfig.loadConsultation (model.BaseUri,shortcode)
            (fun c -> LoadedConsultation (c,shortcode))
            Error
    | LoadedConsultation (data,sc) -> { model with SelectedConsultation = Some { Config = data; CurrentSection = 1; ShortCode = sc; Answers = Map.empty; RequiredQuestions = [] } }, Cmd.none
    | LoadSiteConfig -> 
        model, 
        Cmd.OfAsync.either
            ConsultationConfig.loadConfig model.BaseUri
            (fun c -> LoadedSiteConfig c)
            Error
    | LoadedSiteConfig data ->
        { model with SiteConfig = Some data; CultureCode = data.Languages.[0] }, Cmd.ofMsg GetBrowserLanguage
    | Error e -> { model with Error = e.ToString() |> Some }, Cmd.none
    | CheckValidation section ->
        match model.SelectedConsultation with
        | None -> model, Cmd.none
        | Some c -> 
            let required =
                if section = 1
                then 
                    let translations = if model.SiteConfig.IsSome then model.SiteConfig.Value.Translations |> Seq.toList else []
                    (Form.aboutYouSection c.Answers (fun t -> translateStr t model.SiteConfig model.CultureCode) (fun _ -> ignore)).RequiredQuestions
                else
                    let currentSection = c.Config.Questions |> Seq.tryItem (section - 2)
                    Form.Parser.requiredQuestions section c.Answers (currentSection.Value.Questions |> Seq.toList) |> Seq.toList
            { model with SelectedConsultation = Some { c with RequiredQuestions = required } }, Cmd.none
    | SetDynamicAnswer(section,question,answer) -> 
        match model.SelectedConsultation with
        | None -> model, Cmd.none
        | Some c -> 
            let required =
                if section = 1
                then 
                    let translations = if model.SiteConfig.IsSome then model.SiteConfig.Value.Translations |> Seq.toList else []
                    (Form.aboutYouSection c.Answers (fun t -> translateStr t model.SiteConfig model.CultureCode) (fun _ -> ignore)).RequiredQuestions
                else
                    let currentSection = c.Config.Questions |> Seq.tryItem (section - 2)
                    Form.Parser.requiredQuestions section c.Answers (currentSection.Value.Questions |> Seq.toList) |> Seq.toList
            { model with SelectedConsultation = Some { c with Answers = c.Answers |> Map.add (section,question) answer; RequiredQuestions = required } }, Cmd.none
    | SendCompletedAnswers -> 
        match model.SelectedConsultation with
        | None -> model, Cmd.none
        | Some c -> 
            model, 
            Cmd.OfAsync.either
                (RemoteFormHandler.trySend c.Config.Endpoint) c.Answers
                (fun _ -> SentCompletedAnswers)
                Error
    | SentCompletedAnswers -> model, Cmd.ofMsg (SetPage ThankYou)

let router = Router.infer SetPage (fun m -> m.page)

let homeView model dispatch =
    concat [
        if model.Error.IsSome then textf "Error: %A" model.Error.Value
        section [ attr.``class`` "hero is-medium is-link home-hero" ] [
            div [ attr.``class`` "hero-body" ] [
                cond model.SiteConfig <| function
                | Some sc ->
                    cond (sc.Site |> Seq.tryFind (fun o -> o.Language = currentLanguage model.CultureCode)) <| function
                    | Some sc ->
                        concat [
                            p [ attr.``class`` "title" ] [ text sc.LongTitle ]
                            p [ attr.``class`` "subtitle" ] [ text sc.Subtitle ]
                        ]
                    | None -> empty
                | None -> p [ attr.``class`` "title" ] [ translate "Loading..." model.SiteConfig model.CultureCode ]
            ]
        ]
        section [ attr.``class`` "section"; attr.name "Consultation list" ] [
            div [ attr.``class`` "container content" ] [
                cond model.SiteConfig <| function
                | None -> empty
                | Some sc ->
                    cond (sc.Site |> Seq.tryFind (fun o -> o.Language = currentLanguage model.CultureCode)) <| function
                    | Some sc -> Markdown.htmlFromMarkdown sc.IntroText
                    | None -> empty
                h2 [ attr.``class`` "is-size-3" ] [ translate "Open consultations" model.SiteConfig model.CultureCode ]
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
                                on.click (fun _ -> LoadConsultation c.shortcode |> dispatch) ] [ translate "See more" model.SiteConfig model.CultureCode ]
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

let startButton shortcode model =
    a [ 
        attr.``class`` "button is-light is-primary is-medium"
        router.HRef <| AnswerForm (shortcode,1) ] [
        translate "Start now" model.SiteConfig model.CultureCode
    ]

let detailView shortcode (active:ConsultationConfig.Consultation) model dispatch =
    let activeInfo = active.Information |> Seq.tryFind(fun l -> l.Language = currentLanguage model.CultureCode)
    cond activeInfo <| function
    | None -> translate "The consultation is not configured correctly. Please contact the administrator." model.SiteConfig model.CultureCode
    | Some activeInfo ->
        concat [
            heroHeading activeInfo.Title None
            section [ attr.name "consultation-details" ] [
                div [ attr.``class`` "container" ] [
                    div [ attr.``class`` "columns" ] [
                        div [ attr.``class`` "column is-three-quarters" ] [
                            div [ attr.``class`` "content" ] [ Markdown.htmlFromMarkdown activeInfo.Description ]
                            p [] [ textf "There are %i sections." active.Questions.Count]
                            div [ attr.``class`` "block" ] [ startButton shortcode model ]
                        ]
                        div [ attr.``class`` "column" ] [
                            article [ attr.``class`` "message" ] [
                                div [ attr.``class`` "message-body content" ] [
                                    h4 [] [ translate "Who is consulting?" model.SiteConfig model.CultureCode ]
                                    p [] [
                                        text activeInfo.WhoIsConsulting
                                        a [ attr.href activeInfo.WhoIsConsultingUrl; attr.target "_blank" ] [ translate "See more" model.SiteConfig model.CultureCode ] ]
                                    h4 [] [ translate "How long will it take?" model.SiteConfig model.CultureCode ]
                                    p [] [ textf "It should take about %i minutes." active.TimeEstimateToCompleteInMinutes ]
                                    h4 [] [ translate "Where can I find more information?" model.SiteConfig model.CultureCode ]
                                    p [] [ a [ attr.href active.MoreInformationUrl; attr.target "_blank" ] [ translate "More information is available here." model.SiteConfig model.CultureCode ] ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

let flagFromCountryCode (country:string) =
    country.ToUpper() 
    |> Seq.map(fun c -> (int c) + 0x1F1A5 |> System.Char.ConvertFromUtf32)
    |> String.concat ""

let navbar model dispatch =
    nav [ attr.``class`` "navbar"; attr.aria "label" "main navigation" ] [
        div [ attr.``class`` "navbar-brand" ] [
            a [ attr.``class`` "navbar-item has-text-weight-bold is-size-5"; router.HRef Home ] [
                img [ attr.style "height:40px"; attr.src (model.BaseUri.AbsoluteUri + "images/logo-small.png") ]
                cond model.SiteConfig <| function
                | Some sc ->
                    cond (sc.Site |> Seq.tryFind (fun o -> o.Language = currentLanguage model.CultureCode)) <| function
                    | Some sc -> text sc.ShortTitle
                    | None -> text " Cottongrass"
                | None -> text " Cottongrass"
            ]
        ]
        // Language selection:
        cond model.SiteConfig <| function
        | None -> empty
        | Some c ->
            div [ attr.``class`` "navbar-end" ] [
                div [ attr.``class`` "navbar-item" ] [
                    div [ attr.``class`` "dropdown is-hoverable" ] [
                        div [ attr.``class`` "dropdown-trigger" ] [
                            button [ attr.``class`` "button" ] [
                                span [] [
                                    model.CultureCode
                                    |> currentCountry
                                    |> flagFromCountryCode 
                                    |> text
                                    text (System.Globalization.CultureInfo.GetCultureInfo(model.CultureCode).DisplayName)
                                ]
                                span [ attr.``class`` "icon is-small" ] [
                                    i [ attr.``class`` "fas fa-angle-down" ] []
                                ]
                            ]
                            div [ attr.``class`` "dropdown-menu" ] [
                                div [ attr.``class`` "dropdown-content" ] [
                                    forEach c.Languages <| fun l ->
                                        a [ attr.``class`` "dropdown-item"
                                            on.click(fun _ -> l |> SetLanguage |> dispatch) ] [
                                            l |> currentCountry
                                            |> flagFromCountryCode 
                                            |> text
                                            text (System.Globalization.CultureInfo.GetCultureInfo(l).DisplayName)
                                        ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
    ]

/// The answer form renders form fields from yaml in the selected
/// language.
let answerFormView shortcode section (model:Model) dispatch =
    cond model.SelectedConsultation <| function
    | None -> div [] [ translate "Loading..." model.SiteConfig model.CultureCode ]
    | Some con ->
        let currentSection = con.Config.Questions |> Seq.tryItem (section - 2)
        let subtitle =
            if section = 1 then (translateStr "About You" model.SiteConfig model.CultureCode) |> Some
            else if currentSection.IsSome
            then (currentSection.Value.Name |> Seq.tryFind(fun (d:ConsultationConfig.Consultation.Questions_Item_Type.Name_Item_Type) -> d.Language = currentLanguage model.CultureCode)) |> Option.map (fun l -> l.Translation)
            else None
        let description =
            if currentSection.IsSome
            then (currentSection.Value.Description |> Seq.tryFind(fun (d:ConsultationConfig.Consultation.Questions_Item_Type.Description_Item_Type) -> d.Language = currentLanguage model.CultureCode)) |> Option.map (fun l -> l.Translation)
            else None
        concat [
            cond (con.Config.Information |> Seq.tryFind(fun l -> l.Language = currentLanguage model.CultureCode)) <| function
            | Some i -> heroHeading i.Title subtitle
            | None -> heroHeading "Consultation" subtitle
            // Each answer form has sections of questions.
            // Display a sidebar containing the section progress.
            div [ attr.``class`` "container" ] [
                div [ attr.``class`` "columns" ] [
                    div [ attr.``class`` "column" ] [
                        cond description.IsSome <| function
                        | true -> 
                            div [ attr.``class`` "content" ] [ 
                                Markdown.htmlFromMarkdown description.Value
                                hr [] ]
                        | false -> empty
                        cond (section = 1) <| function
                        | true -> 
                            concat [
                                Form.Parser.compile (Form.aboutYouSection con.Answers (fun t -> translateStr t model.SiteConfig model.CultureCode) (fun qn q -> (1,qn,q) |> SetDynamicAnswer |> dispatch))
                                cond ( Set.isSubset (Set.ofList con.RequiredQuestions) (con.Answers |> Seq.map(fun k -> k.Key) |> Seq.where(fun (a,_) -> a = section) |> Seq.map snd |> Set.ofSeq)) <| function
                                | false -> 
                                    article [ attr.``class`` "message is-warning" ] [
                                        div [ attr.``class`` "message-body" ] [ 
                                            textf "Please answer the required questions above before continuing (marked by a red asterisk). There are %i remaining." 
                                                (con.RequiredQuestions.Length - (con.Answers |> Seq.map(fun k -> k.Key) |> Seq.where(fun (a,_) -> a = section) |> Seq.map snd |> Seq.where(fun i -> con.RequiredQuestions |> Seq.contains i) |> Seq.length))
                                        ]
                                    ]
                                | true -> button [ attr.``class`` "button"; on.click (fun _ -> AnswerForm(shortcode,section+1) |> SetPage |> dispatch ) ] [ translate "Next" model.SiteConfig model.CultureCode ]
                            ]
                        | false -> 
                            cond (con.Config.Questions |> Seq.tryItem (section - 2)) <| function
                            | None -> translate "There was a problem: this section doesn't exist." model.SiteConfig model.CultureCode
                            | Some s -> 
                                let form, _ = Form.Parser.parseYaml (currentLanguage model.CultureCode) section (s.Questions |> Seq.toList) con.Answers (SetDynamicAnswer >> dispatch)
                                concat [ form;
                                    cond ( Set.isSubset (Set.ofList con.RequiredQuestions) (con.Answers |> Seq.map(fun k -> k.Key) |> Seq.where(fun (a,_) -> a = section) |> Seq.map snd |> Set.ofSeq)) <| function
                                    | false -> 
                                        article [ attr.``class`` "message is-warning" ] [
                                            div [ attr.``class`` "message-body" ] [ 
                                                textf "Please answer the required questions above before continuing (marked by a red asterisk). There are %i remaining." 
                                                    (con.RequiredQuestions.Length - (con.Answers |> Seq.map(fun k -> k.Key) |> Seq.where(fun (a,_) -> a = section) |> Seq.map snd |> Seq.where(fun i -> con.RequiredQuestions |> Seq.contains i) |> Seq.length))
                                            ]
                                        ]
                                    | true ->
                                        cond (section = con.Config.Questions.Count + 1) <| function
                                        | true ->
                                            div [ attr.``class`` "box" ] [
                                                p [] [ translate "Please confirm below to send your responses to us and complete the consultation." model.SiteConfig model.CultureCode ]
                                                button [ attr.``class`` "button"; on.click (fun _ -> SendCompletedAnswers |> dispatch ) ] [ translate "Finish" model.SiteConfig model.CultureCode ]
                                            ]
                                        | false -> button [ attr.``class`` "button"; on.click (fun _ -> AnswerForm(shortcode,section+1) |> SetPage |> dispatch ) ] [ translate "Next" model.SiteConfig model.CultureCode ]
                                ]
                    ]
                    div [ attr.``class`` "column is-one-quarter with-left-line" ] [
                        label [] [ textf "Section %i of %i" section (con.Config.Questions.Count+1) ]
                        progress [ attr.``class`` "progress is-primary"
                                   attr.value section
                                   attr.max (con.Config.Questions.Count+1) ] [ textf "%i%%" (section / (con.Config.Questions.Count+1)) ]
                        ul [ attr.``class`` "bar" ] [
                            concat [
                                if section = 1 then li [] [ translate "About You" model.SiteConfig model.CultureCode ]
                                else li [] [ a [ on.click (fun _ -> AnswerForm(shortcode,1) |> SetPage |> dispatch ) ] [ translate "About you" model.SiteConfig model.CultureCode ]]
                                forEach [1 .. con.Config.Questions.Count] (fun i -> 
                                    if (i+1) = section then li [] [ text (con.Config.Questions.[i-1].Name |> Seq.find(fun d -> d.Language = currentLanguage model.CultureCode)).Translation ]
                                    else if (i+1) >= section then li [] [ text (con.Config.Questions.[i-1].Name |> Seq.find(fun d -> d.Language = currentLanguage model.CultureCode)).Translation ]
                                    else li [] [ a [ on.click (fun _ -> AnswerForm(shortcode,i+1) |> SetPage |> dispatch ) ] [ text (con.Config.Questions.[i-1].Name |> Seq.find(fun d -> d.Language = currentLanguage model.CultureCode)).Translation ]])
                            ]
                        ]
                    ]
                ]
            ]
        ]

let thankYouView model dispatch =
    p [] [ translate "Thank you for your responses." model.SiteConfig model.CultureCode ]

let view model dispatch =
    concat [
        navbar model dispatch
        cond model.page <| function
        | Home -> homeView model dispatch
        | Consultation _ ->
            cond model.SelectedConsultation <| function
            | None -> translate "Loading consultation..." model.SiteConfig model.CultureCode
            | Some con -> detailView con.ShortCode con.Config model dispatch
        | AnswerForm (code,section) -> answerFormView code section model dispatch
        | ThankYou -> thankYouView model dispatch
    ]

open Microsoft.AspNetCore.Components

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val NavigationManager = Unchecked.defaultof<NavigationManager> with get, set

    override this.Program =
        let baseUri = System.Uri this.NavigationManager.BaseUri
        Program.mkProgram (fun _ -> initModel baseUri, Cmd.batch [
            Cmd.ofMsg LoadIndex
            Cmd.ofMsg LoadSiteConfig ]) (update this.JSRuntime) view
        |> Program.withRouter router
        //|> Program.withConsoleTrace