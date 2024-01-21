module FsAutoComplete.Lint

// open FSharpLint.Framework
// open FSharpLint.Application
open FsAutoComplete.Logging
open FSharp.Compiler.Text
open FSharpLint.Client.Contracts
open FSharpLint.Client.LSPFSharpLintService
open FsToolkit.ErrorHandling
open FSharpLint.Client.LSPFSharpLintServiceTypes
open System

let logger = LogProvider.getLoggerByName "FSharpLint"

let fsharpLintService: FSharpLintService =
  new LSPFSharpLintService() :> FSharpLintService

type EnrichedLintWarning =
  { Warning: ClientLintWarning
    HelpUrl: string
    Code: string }

let pageForLint (identifier: string) =
  sprintf "http://fsprojects.github.io/FSharpLint/how-tos/rules/%s.html" identifier

/// In addition we add the url to the matching help page for fsharplint
let enrichLintWarning (w: ClientLintWarning) : EnrichedLintWarning =
  { Code = w.RuleIdentifier
    HelpUrl = pageForLint w.RuleIdentifier
    Warning = w }

/// Attempts to load the F#Lint configuration from the closest available settings file to the given project file.
// let loadConfiguration (workspaceRoot: string option) (lintConfigFileRelativePath: string option) =
//   let expectedConfigPath = defaultArg lintConfigFileRelativePath "fsharplint.json"

//   match workspaceRoot with
//   | Some root ->
//     let fullPath = System.IO.Path.Combine(root, expectedConfigPath)

//     if System.IO.File.Exists fullPath then
//       ConfigurationParam.FromFile fullPath
//     else
//       ConfigurationParam.Default
//   | None -> ConfigurationParam.Default

let lintRangeToLsp (range: ClientRange): Ionide.LanguageServerProtocol.Types.Range =
  { Start =
      { Line = range.StartLine - 1
        Character = range.StartColumn }
    End =
      { Line = range.EndLine - 1
        Character = range.EndColumn } }

let lintFile ast (sourceCode: ISourceText) filePath typeCheckResults =
  asyncResult {
    //TODO: do nothing if no fsharplint.json ? (what about default config)
    let _ = ast
    let _ = sourceCode
    let _ = typeCheckResults

    let req =
      { FilePath = filePath
        LintConfigPath = None //TODO: get proper config from project or overriden by ionide config ?
      }
    let! ct = Async.CancellationToken

    // Async.Sleep (TimeSpan.FromSeconds 10) |> ignore

    try
      let! res = fsharpLintService.LintFileAsync(req, ct)
      match LanguagePrimitives.EnumOfValue res.Code, res.Result with
      | FSharpLintResponseCode.OkLint, LintResult warnings ->
          let splitWarnings = warnings |> List.map enrichLintWarning
          return! Ok splitWarnings
      | FSharpLintResponseCode.OkLintError, Content message ->
          logger.error (
            Log.setMessage "Fatal error in linter: {message}"
            >> Log.addContextDestructured "message" message)
          return! Error(sprintf "Something went wrong, linter failed")
      | code, _ ->
          logger.error (Log.setMessage "Fatal unknown error in linter: {code}"
            >> Log.addContextDestructured "code" code)
          return! Error(sprintf "Something went wrong, linter failed")
    with e ->
      logger.error (
        Log.setMessage "Fatal error in linter: {message}"
        >> Log.addContextDestructured "message" e.Message
        >> Log.addExn e
      )

      return! Error(sprintf "Something went wrong, linter failed: %s" e.Message)
  }
