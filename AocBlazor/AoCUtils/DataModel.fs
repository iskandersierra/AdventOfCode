namespace AoCUtils.DataModel

open System
open System.IO
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open System.Threading
open System.Threading.Tasks
open System.Text
open System.Text.RegularExpressions

type IInputSample =
    abstract DisplayName : string
    abstract GetText : ?cancel: CancellationToken -> Task<string>

module InputSample =
    type CustomInputSample(displayName, getText) =
        interface IInputSample with
            member this.DisplayName = displayName

            member this.GetText cancel =
                cancel
                |> Option.defaultValue CancellationToken.None
                |> getText

    let loadFileSamples year day =
        let prefix = $"Year%i{year}-Day%i{day}-"
        let filePattern = $"{prefix}*.txt"

        let regexPattern =
            $@"^{Regex.Escape(prefix)}(?<name>.+?)\.txt$"

        let regex =
            Regex(regexPattern, RegexOptions.Compiled)

        seq {
            let cwd =
                Assembly.GetExecutingAssembly().Location
                |> Path.GetDirectoryName

            let files =
                Directory.EnumerateFiles(cwd, filePattern)

            for file in files do
                let getText cancel = File.ReadAllTextAsync(file, cancel)
                let fileName = Path.GetFileName(file)
                match regex.Match(fileName) with
                | m when m.Success ->
                    let name = m.Groups.["name"].Value
                    yield CustomInputSample(name, getText) :> IInputSample
                | _ -> ()
        }

type IExercise =
    abstract Year : int
    abstract Day : int
    abstract Title : string
    abstract Link : string
    abstract GetInputSamples : unit -> IInputSample seq
