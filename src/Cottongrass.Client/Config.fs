module Cottongrass.Client.ConsultationConfig

open FSharp.Configuration
open System
open System.Net.Http

type Consultation = YamlConfig<"templates/consultation.yml">
type Index = YamlConfig<"templates/index.yml">
type SiteConfig = YamlConfig<"templates/config.yml">

let loadIndex baseUri =
    async {
        use client = new HttpClient(BaseAddress = baseUri)
        let reader = Index ()
        let! data =
            "data/consultations/index.yml"
            |> client.GetStringAsync
            |> Async.AwaitTask
        reader.LoadText data
        return reader.consultations |> seq
    }

let loadConsultation (baseUri,shortcode) =
    async {
        use client = new HttpClient(BaseAddress = baseUri)
        let reader = Consultation()
        let! data =
            ("data/consultations/" + shortcode + ".yml")
            |> client.GetStringAsync
            |> Async.AwaitTask
        reader.LoadText data
        return reader
    }

let loadConfig baseUri =
    async {
        use client = new HttpClient(BaseAddress = baseUri)
        let reader = SiteConfig()
        let! data =
            "data/config.yml"
            |> client.GetStringAsync
            |> Async.AwaitTask
        reader.LoadText data
        return reader
    }
