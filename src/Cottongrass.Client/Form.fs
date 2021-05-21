module Cottongrass.Client.Form

open Bolero
open Bolero.Html

type DynamicQuestionAnswer =
    | BinaryChoice of bool
    | Text of string
    | Choice of string

let field labelText contents =
    div [ attr.``class`` "field" ] [
        label [ attr.``class`` "label" ] [ text labelText ]
        contents
    ]

let textField name labelText placeholder helpText value dispatch =
    field labelText (concat [ 
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

let binaryOptionField id labelText optionTrue optionFalse value dispatch =
    field labelText (concat [
        div [ attr.``class`` "control" ] [
            label [ attr.``class`` "radio" ] [
                input [ attr.``type`` "radio"
                        attr.name id
                        attr.``value`` true
                        bind.change.string (string true) (fun _ -> true |> dispatch) ]
                text optionTrue
            ]
            label [ attr.``class`` "radio" ] [
                input [ attr.``type`` "radio"
                        attr.name id
                        attr.``value`` false
                        bind.change.string (string false) (fun _ -> false |> dispatch) ]
                text optionFalse
            ]
        ]
    ])

let optionField id labelText optionNames value dispatch =
    field labelText (concat [
        div [ attr.``class`` "control" ] [
            forEach optionNames <| fun opt ->
                label [ attr.``class`` "radio" ] [
                    input [ attr.``type`` "radio"
                            attr.name id
                            bind.change.string opt dispatch ]
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
        View: Node
    }

    type QuestionSectionBuilder = {
        CurrentQ: Question * bool // indicates if visible
        PreviousQs: Node
        PreviousQuestionModel: Map<int,Question>
    }

    module Visibility =

        open System
        open System.Text.RegularExpressions

        let isVisible (q:Question) s =
            printfn "Previous is %A" q.Answer
            match s with
            | s when s = "always" -> true
            | s when Regex.IsMatch(s, "previous question (is not|is) (true|false|.*)") ->
                let m = Regex.Match(s, "previous question (is not|is) (true|false|.*)")
                printfn "R1 is %s" m.Groups.[1].Value
                printfn "R2 is %s" m.Groups.[2].Value
                if m.Groups.[2].Value = "true" || m.Groups.[2].Value = "false"
                then
                    match q.Answer with
                    | BinaryChoice b ->
                        printfn "Binary choice. Current is %A" b
                        if m.Groups.[1].Value = "is" && b = Boolean.Parse m.Groups.[2].Value then true
                        else if m.Groups.[1].Value = "is not" && b <> Boolean.Parse m.Groups.[2].Value then true
                        else false
                    | _ -> false
                else
                    match q.Answer with
                    | Choice c
                    | Text c ->
                        if m.Groups.[1].Value = "is" then c = m.Groups.[2].Value
                        else c <> m.Groups.[2].Value
                    | _ -> false
            | _ -> false


    let lift question = { CurrentQ = question, true; PreviousQs = empty; PreviousQuestionModel = Map.empty }

    let binaryChoice name question labelOne labelTwo value dispatchAnswer =
        { View = binaryOptionField name question labelOne labelTwo value (BinaryChoice >> dispatchAnswer)
          Answer = BinaryChoice value }

    let choiceQuestion name question choices value dispatch =
        { View = optionField name question choices value (Choice >> dispatch)
          Answer = Choice value }

    let textQuestion name label placeholder value dispatchAnswer =
        { View = textField name label placeholder None value (Text >> dispatchAnswer)
          Answer = Text value }

    let compile builder =
        concat [
            builder.PreviousQs
            if snd builder.CurrentQ then (fst builder.CurrentQ).View
        ]

    let andQuestion nextQuestion condition builder =
        let n = builder.PreviousQuestionModel.Count + 1
        let display = Visibility.isVisible (fst builder.CurrentQ) condition
        { CurrentQ = nextQuestion, display
          PreviousQs = compile builder
          PreviousQuestionModel = builder.PreviousQuestionModel |> Map.add n nextQuestion }

    let parseQuestion identifier langCode answer (q:ConsultationConfig.Consultation.Questions_Item_Type.Questions_Item_Type) =
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
            let current = // TODO Make values optional
                match answer with
                | None -> false
                | Some a -> 
                    match a with
                    | BinaryChoice v -> v
                    | _ -> false
            binaryChoice name qText labels.[0] labels.[1] current
        | "choice" ->
            let labels =
                match q.Choices |> Seq.tryFind(fun q -> q.Language = langCode) with
                | Some choices -> choices.Translation
                | None -> failwith "No translation for choices found"
            let current =
                match answer with
                | None -> ""
                | Some a -> 
                    match a with
                    | Choice v -> v
                    | _ -> ""
            choiceQuestion name qText labels current
        | "freetext"
        | _ -> 
            match answer with
            | None -> textQuestion name qText "" ""
            | Some a -> 
                match a with
                | Text t -> textQuestion name qText "" t
                | _ -> textQuestion name qText "" ""

    let rec parseYaml' langCode sectionId (remainingQs:ConsultationConfig.Consultation.Questions_Item_Type.Questions_Item_Type list) answerMap dispatch (builder:QuestionSectionBuilder option) =
        match Seq.isEmpty remainingQs with
        | true -> 
            if Option.isSome builder
            then builder.Value |> compile
            else empty
        | false ->
            let handler q = (sectionId,remainingQs.Length,q) |> dispatch
            let currentAnswer = answerMap |> Map.tryFind (sectionId, remainingQs.Length)
            let question = parseQuestion (remainingQs.Length) langCode currentAnswer (remainingQs |> Seq.head)
            if Option.isNone builder
            then question handler |> lift |> Some |> parseYaml' langCode sectionId remainingQs.Tail answerMap dispatch
            else builder |> Option.map (andQuestion (question handler) (remainingQs |> Seq.head).Visible) |> parseYaml' langCode sectionId remainingQs.Tail answerMap dispatch

    let parseYaml language sectionId section answers dispatch =
        parseYaml' language sectionId section answers dispatch None
        