module Cottongrass.Client.Form

open Bolero
open Bolero.Html

type DynamicQuestionAnswer =
    | BinaryChoice of bool
    | Text of string
    | Choice of int * string

let field labelText required contents =
    div [ attr.``class`` "field box question-box" ] [
        (if required then label [ attr.``class`` "label required" ] [ text labelText ]
        else label [ attr.``class`` "label" ] [ text labelText ])
        contents
    ]

let textField name req labelText placeholder helpText value dispatch =
    field labelText req (concat [ 
        div [ attr.``class`` "control" ] [
        input [ attr.``class`` "input"
                attr.``type`` "text"
                attr.``name`` name
                attr.placeholder placeholder
                bind.change.string value dispatch ]
        ]
        if Option.isSome helpText
        then p [ attr.``class`` "help" ] [ text helpText.Value ]
    ])

let textAreaField name req labelText placeholder helpText value dispatch =
    field labelText req (concat [ 
        div [ attr.``class`` "control" ] [
        textarea [  attr.``class`` "textarea"
                    attr.``name`` name
                    attr.placeholder placeholder
                    bind.change.string value dispatch ] []
        ]
        if Option.isSome helpText
        then p [ attr.``class`` "help" ] [ text helpText.Value ]
    ])

let binaryOptionField id req labelText optionTrue optionFalse value dispatch =
    field labelText req (concat [
        div [ attr.``class`` "control" ] [
            div [ attr.``class`` "radio inline" ] [
                input [ attr.``type`` "radio"
                        attr.name id
                        attr.``value`` true
                        bind.change.string (string true) (fun _ -> true |> dispatch) ]
                label [ attr.``class`` "radio-label" ] [ text optionTrue ]
            ]
            div [ attr.``class`` "radio inline" ] [
                input [ attr.``type`` "radio"
                        attr.name id
                        attr.``value`` false
                        bind.change.string (string false) (fun _ -> false |> dispatch) ]
                label [ attr.``class`` "radio-label" ] [ text optionFalse ]
            ]
        ]
    ])

let optionField id req labelText (optionNames:string list) value dispatch =
    field labelText req (concat [
        div [ attr.``class`` "control" ] [
            forEach [ 1 .. optionNames.Length ] <| fun opt ->
                div [ attr.``class`` "radio" ] [
                    input [ attr.``type`` "radio"
                            attr.name id
                            if value |> Option.isSome then
                                if opt = (fst value.Value) then attr.``checked`` "checked"
                            bind.change.string (optionNames.[opt-1]) (fun s -> dispatch (opt,s)) ]
                    label [ attr.``for`` id; attr.``class`` "radio-label" ] [ text (optionNames.[opt-1]) ]
                ]
        ]
    ])

module Parser =

    type OptionField = {
        Options: string list
        Selected: string
    }

    type Question = {
        Answer: DynamicQuestionAnswer option
        View: Node
    }

    type QuestionSectionBuilder = {
        CurrentQ: Question * bool // indicates if visible
        PreviousQs: Node
        PreviousQuestionModel: Map<int,Question>
        RequiredQuestions: int list
    }

    module Visibility =

        open System
        open System.Text.RegularExpressions

        let isVisible (previousAnswer:DynamicQuestionAnswer option) s =
            match s with
            | s when s = "always" -> true
            | s when Regex.IsMatch(s, "previous question (is not|is) (true|false|choice ([1-9]{1,2})|.*)") ->
                let m = Regex.Match(s, "previous question (is not|is) (true|false|choice ([1-9]{1,2})|.*)")
                if m.Groups.[2].Value = "true" || m.Groups.[2].Value = "false"
                then
                    match previousAnswer with
                    | None -> false
                    | Some a -> 
                        match a with
                        | BinaryChoice b ->
                            if m.Groups.[1].Value = "is" && b = Boolean.Parse m.Groups.[2].Value then true
                            else if m.Groups.[1].Value = "is not" && b <> Boolean.Parse m.Groups.[2].Value then true
                            else false
                        | _ -> false
                else
                    match previousAnswer with
                    | None -> false
                    | Some a -> 
                        match a with
                        | Choice (i,c) ->
                            if not (String.IsNullOrEmpty(m.Groups.[3].Value))
                            then
                                if m.Groups.[1].Value = "is" 
                                then i = int(m.Groups.[3].Value)
                                else i <> int(m.Groups.[3].Value)
                            else if m.Groups.[1].Value = "is" then c = m.Groups.[2].Value
                            else c <> m.Groups.[2].Value
                        | Text c ->
                            if m.Groups.[1].Value = "is" then c = m.Groups.[2].Value
                            else c <> m.Groups.[2].Value
                        | _ -> false
            | _ -> false

    let lift isRequired question = { CurrentQ = question, true; PreviousQs = empty; PreviousQuestionModel = Map.empty; RequiredQuestions = if isRequired then [1] else [] }

    let binaryChoice name question labelOne labelTwo value req dispatchAnswer =
        { View = binaryOptionField name req question labelOne labelTwo value (BinaryChoice >> dispatchAnswer)
          Answer = value |> Option.map BinaryChoice }

    let choiceQuestion name question choices value req dispatch =
        { View = optionField name req question choices value (Choice >> dispatch)
          Answer = value |> Option.map Choice }

    let textQuestion name label placeholder value req dispatchAnswer =
        { View = textField name req label placeholder None (if value |> Option.isSome then value.Value else "") (Text >> dispatchAnswer)
          Answer = value |> Option.map Text }

    let longTextQuestion name label placeholder value req dispatchAnswer =
        { View = textAreaField name req label placeholder None (if value |> Option.isSome then value.Value else "") (Text >> dispatchAnswer)
          Answer = value |> Option.map Text }

    let compile builder =
        concat [
            builder.PreviousQs
            if snd builder.CurrentQ then (fst builder.CurrentQ).View
        ]

    let andQuestion nextQuestion displayCondition isRequired builder =
        let n = builder.PreviousQuestionModel.Count + 2
        let visible = Visibility.isVisible (fst builder.CurrentQ).Answer displayCondition
        { CurrentQ = nextQuestion, visible
          PreviousQs = compile builder
          PreviousQuestionModel = builder.PreviousQuestionModel |> Map.add n nextQuestion
          RequiredQuestions = if visible && isRequired then n :: builder.RequiredQuestions else builder.RequiredQuestions }

    let parseQuestion (identifier:int) langCode answer (q:ConsultationConfig.Consultation.Questions_Item_Type.Questions_Item_Type) =
        let qText =
            match q.Question |> Seq.tryFind(fun q -> q.Language = langCode) with
            | Some q -> q.Translation
            | None -> failwith "No translation for question found"
        let name = sprintf "q-%i" identifier
        match q.Type with
        | "binary choice" ->
            let labels =
                match q.Choices |> Seq.tryFind(fun q -> q.Language = langCode) with
                | Some choices -> choices.Translation
                | None -> failwith "No translation for choices found"
            if labels.Count <> 2 then failwith "Binary choice must have two options"
            let current =
                answer |> Option.map(fun a ->
                    match a with
                    | BinaryChoice v -> v
                    | _ -> false )
            binaryChoice name qText labels.[0] labels.[1] current q.Required
        | "choice" ->
            let labels =
                match q.Choices |> Seq.tryFind(fun q -> q.Language = langCode) with
                | Some choices -> choices.Translation |> Seq.toList
                | None -> failwith "No translation for choices found"
            let current =
                answer |> Option.map(fun a ->
                    match a with
                    | Choice (i,v) -> i,v
                    | _ -> 0,"" )
            choiceQuestion name qText labels current q.Required
        | "freetext" ->
            match answer with
            | None -> longTextQuestion name qText "" None q.Required
            | Some a -> 
                match a with
                | Text t -> longTextQuestion name qText "" (Some t) q.Required
                | _ -> longTextQuestion name qText "" None q.Required
        | "short text"
        | _ -> 
            match answer with
            | None -> textQuestion name qText "" None q.Required
            | Some a -> 
                match a with
                | Text t -> textQuestion name qText "" (Some t) q.Required
                | _ -> textQuestion name qText "" None q.Required

    let rec parseYaml' (n:int) langCode sectionId (remainingQs:ConsultationConfig.Consultation.Questions_Item_Type.Questions_Item_Type list) answerMap dispatch (builder:QuestionSectionBuilder option) =
        match Seq.isEmpty remainingQs with
        | true -> 
            if Option.isSome builder
            then builder.Value |> compile, builder.Value.RequiredQuestions
            else empty, []
        | false ->
            let handler q = (sectionId,n,q) |> dispatch
            let currentAnswer = answerMap |> Map.tryFind (sectionId, n)
            let question = parseQuestion n langCode currentAnswer (remainingQs |> Seq.head)
            let required = (remainingQs |> Seq.head).Required
            if Option.isNone builder
            then question handler |> lift required |> Some |> parseYaml' (n+1) langCode sectionId remainingQs.Tail answerMap dispatch
            else builder |> Option.map (andQuestion (question handler) (remainingQs |> Seq.head).Visible required) |> parseYaml' (n+1) langCode sectionId remainingQs.Tail answerMap dispatch

    let parseYaml language sectionId section answers dispatch =
        parseYaml' 1 language sectionId section answers dispatch None
        
    let requiredQuestions 
        (section: int)
        (answers:Map<int * int,DynamicQuestionAnswer>) 
        (questions:ConsultationConfig.Consultation.Questions_Item_Type.Questions_Item_Type list) =
        questions
        |> Seq.mapi(fun i q -> 
            let answer = answers |> Map.tryFind (section, i)
            match Visibility.isVisible answer q.Visible with
                | true -> if q.Required then Some (i+1) else None
                | false -> None )
        |> Seq.choose id


let textAnswer i answers = 
    match answers |> Map.tryFind (1,i) with
    | Some a ->
        match a with
        | DynamicQuestionAnswer.Text s -> Some s
        | _ -> None
    | None -> None

let choiceAnswer i answers = 
    match answers |> Map.tryFind (1,i) with
    | Some a ->
        match a with
        | DynamicQuestionAnswer.Choice (i,s) -> Some (i,s)
        | _ -> None
    | None -> None

let binaryAnswer i answers =
    match answers |> Map.tryFind (1,i) with
    | Some a ->
        match a with
        | DynamicQuestionAnswer.BinaryChoice s -> Some s
        | _ -> None
    | None -> None

let aboutYouSection answers translate handler =
    Parser.textQuestion "firstname" (translate "First Name") "" (textAnswer 1 answers) true (handler 1)
    |> Parser.lift true
    |> Parser.andQuestion (Parser.textQuestion "lastname" (translate "Last Name") "" (textAnswer 2 answers) true (handler 2)) "always" true
    |> Parser.andQuestion (Parser.binaryChoice "org" (translate "Are you representing an organisation?") (translate "Yes") (translate "No") (binaryAnswer 3 answers) true (handler 3)) "always" true
    |> Parser.andQuestion (Parser.textQuestion "orgname" (translate "Organisation Name") "" (textAnswer 4 answers) true (handler 4)) "previous question is true" true
    |> Parser.andQuestion (Parser.choiceQuestion "publishmethod" (translate "Responses from all respondants will be collated and analysed together. As part of this work, we may publish a summary of the responses. We will only publish your responses if you give consent.") [ translate "Do not publish my responses"; translate "Publish anonymised response in full"; translate "Publish response in full with name (and organisation if applicable)" ] (choiceAnswer 5 answers) true (handler 5)) "always" true
    |> Parser.andQuestion (Parser.choiceQuestion "contactmethod" (translate "If we would like to follow-up your answers, what is the best method of contact?") [ translate "Telephone"; translate "Email"; translate "I don't want to be contacted" ] (choiceAnswer 6 answers) true (handler 6)) "always" true
    |> Parser.andQuestion (Parser.textQuestion "contactdetail" (translate "Your telephone number or email") "" (textAnswer 7 answers) true (handler 7)) "previous question is not choice 3" true
    |> Parser.andQuestion (Parser.choiceQuestion "workinggroup" (translate "As part of this project, we would like to involve the community so that our findings are most relevant and well-suited to all that may value them. Can we keep in contact with you for this purpose?") [ translate "Yes, I would like to be involved"; translate "No, but I would like to recieve updates"; translate "No, I would not like to be involved or recieve updates" ] (choiceAnswer 8 answers) true (handler 8)) "always" true
