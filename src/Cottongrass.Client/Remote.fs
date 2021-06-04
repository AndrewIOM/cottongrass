module Cottongrass.Client.RemoteFormHandler

open System.Net.Http
open System.Net.Http.Json
open Form

// TODO Need to turn Map of DQA DU type into key value dictionary.
let flattenValues (con:Map<int * int,DynamicQuestionAnswer>) =
    con|> Seq.map(fun pair ->
        let key = sprintf "section%i-question%i" (fst pair.Key) (snd pair.Key)
        let value =
            match pair.Value with
            | Text s -> s
            | BinaryChoice c -> c.ToString()
            | Choice c -> c
        key, value )

let trySend endpoint answers =
    async {
        use client = new HttpClient(BaseAddress = endpoint)
        let! post =
            client.PostAsJsonAsync(endpoint, answers |> flattenValues)
            |> Async.AwaitTask
        if post.IsSuccessStatusCode
        then return Ok ()
        else return Result.Error "Could not post form data to external service"
    }
