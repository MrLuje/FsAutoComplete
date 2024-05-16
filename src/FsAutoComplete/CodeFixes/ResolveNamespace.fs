module FsAutoComplete.CodeFix.ResolveNamespace

open System
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling
open FsAutoComplete.LspHelpers
open FsAutoComplete
open FSharp.Compiler.Text
open FSharp.Compiler.EditorServices
open System.Text.RegularExpressions

type LineText = string

let undefinedName =
  [ "not define"
    "nedefinuje|Není definovaný|Není definované|Není definovaná|Nemáte definovaný"
    "definiert nicht|nicht.*? definiert"
    "no define|no está definido|no está definida"
    "ne définit|n'est pas défini"
    "non definisce|non è definito|non è definita"
    "定義(され|し)ていません"
    "정의(하지 않|되지 않았|되어 있지 않)습니다"
    "nie definiuje|Nie zdefiniowano|nie jest zdefiniowany"
    "não define|não está definido"
    "не определяет|не определено|не определены|не определен"
    "tanımlamıyor|tanımlı değil"
    "未.*?定义"
    "未定義" ]
  |> List.map (fun i ->
    let regex = Regex(i, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    fun (j: string) -> regex.IsMatch(j))

/// a codefix the provides suggestions for opening modules or using qualified names when an identifier is found that needs qualification
let fix
  (getParseResultsForFile: GetParseResultsForFile)
  (getNamespaceSuggestions:
    ParseAndCheckResults
      -> FcsPos
      -> LineText
      -> OpenStatementInsertionPoint
      -> Async<CoreResponse<string * list<string * string * InsertionContext * bool> * list<string * string>>>)
  (openNamespacePreference: OpenStatementInsertionPoint)
  =

  /// insert a line of text at a given line
  let insertLine line col lineStr =
    { Range =
        { Start = { Line = line; Character = col }
          End = { Line = line; Character = col } }
      NewText = lineStr }

  let qualifierFix file diagnostic qual =
    { SourceDiagnostic = Some diagnostic
      Edits =
        [| { Range = diagnostic.Range
             NewText = qual } |]
      File = file
      Title = $"Use %s{qual}"
      Kind = FixKind.Fix }

  let openFix
    (text: ISourceText)
    file
    diagnostic
    (word: string)
    (_ast: FSharp.Compiler.Syntax.ParsedInput)
    (_pos: FcsPos)
    (ns, name: string, ctx, _multiple)
    : Fix =
    
    match openNamespacePreference with
    | OpenStatementInsertionPoint.Nearest ->
      let insertion = OpenNamespace.insertNearest ns ns _ast _pos.Line (fun p -> text.GetLineString (Conversions.fcsPosToLsp p).Line)
      let edits = [| yield insertLine (insertion.Line) (insertion.Column) (insertion.InsertText) |]

      { Edits = edits
        File = file
        SourceDiagnostic = Some diagnostic
        Title = insertion.DisplayText
        Kind = FixKind.Fix }

    | OpenStatementInsertionPoint.TopLevel -> 
      let actualOpen =
        if name.EndsWith(word, StringComparison.Ordinal) && name <> word then
          let prefix = name.Substring(0, name.Length - word.Length).TrimEnd('.')

          $"%s{ns}.%s{prefix}"
        else
          ns

      let insertion = OpenNamespace.insertAtTop text actualOpen ns ctx 
      let edits = [| yield insertLine (insertion.Line) 0 (insertion.InsertText) |]

      { Edits = edits
        File = file
        SourceDiagnostic = Some diagnostic
        Title = insertion.DisplayText
        Kind = FixKind.Fix }

  Run.ifDiagnosticByCheckMessage undefinedName (fun diagnostic codeActionParameter ->
    asyncResult {
      let pos = protocolPosToPos diagnostic.Range.Start

      let filePath = codeActionParameter.TextDocument.GetFilePath() |> Utils.normalizePath

      let! tyRes, line, lines = getParseResultsForFile filePath pos

      match! getNamespaceSuggestions tyRes pos line openNamespacePreference with
      | CoreResponse.InfoRes _msg
      | CoreResponse.ErrorRes _msg -> return []
      | CoreResponse.Res(word, opens, qualifiers) ->
        let quals =
          qualifiers
          |> List.map (fun (_, qual) -> qualifierFix codeActionParameter.TextDocument diagnostic qual)

        let ops =
          opens
          |> List.map (openFix lines codeActionParameter.TextDocument diagnostic word tyRes.GetAST pos)
          // |> List.map (openFix lines codeActionParameter.TextDocument diagnostic word)

        return [ yield! ops; yield! quals ]
    })
