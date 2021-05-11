module Cottongrass.Client.ConsultationConfig

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
