namespace ClassicASPNetCore.ASP.Parser
open FParsec

type Languages =
    | DefaultLanguage
    | SpecificLanguage of name: string

type IncludeType =
    | Virtual = 0
    | File = 1

type ASPBlocks =
    | HTML of content: string
    | Code of body: string
    | ProcessingDirective of attributes: (string * string) seq
    | OutputDirective of body: string
    | Script of language: Languages * body: string
    | ScriptFile of language: Languages * path: string
    | Include of includeType: IncludeType * path: string

module ASPParser =

    let private cNameStart c =
        isAsciiLetter c || c = '_' || c = ':'
    let private cNameContinue c =
        cNameStart c || isDigit c || c = '-' || c = '.'
    let private identOpts = IdentifierOptions(isAsciiIdStart = cNameStart,
                                              isAsciiIdContinue = cNameContinue)
    let private attrIdent = identifier identOpts

    let private pAttribute =
        tuple2
            (between spaces spaces attrIdent)
            (
                opt (
                    skipChar '='
                    >>. spaces
                    >>. skipChar '"'
                    >>. manyCharsTill anyChar (skipChar '"')
                )
            )
        .>> spaces

    let private pOutputDirective: Parser<ASPBlocks, Unit> =
        between
            (skipString "<%" >>. spaces >>. skipChar '=')
            spaces
            (manyCharsTill anyChar (skipString "%>"))
        |>> OutputDirective

    let private pProcessingDirective: Parser<ASPBlocks, unit> =
        between
            (skipString "<%" >>. spaces >>. skipChar '@')
            (skipString "%>" .>> spaces)
            (many1 pAttribute)
        |>> List.map (fun (name, value) ->
            match value with
            | Some (value) -> (name, value)
            | None -> (name, Unchecked.defaultof<_>)
        )  |>> seq |>> ProcessingDirective

    let private pDefaultCode: Parser<ASPBlocks, Unit> =
        between
            (skipString "<%")
            spaces
            (manyCharsTill anyChar (skipString "%>"))
        |>> Code

    let private pInclude: Parser<ASPBlocks, Unit> =
        between
            (skipString "<!--" >>. spaces >>. skipStringCI "#include")
            (skipString "-->" .>> spaces)
            (many1 pAttribute)
        |>> fun c ->
            let el = c |> List.find (fun (a: string * string option) ->
                let (b, path) = a
                let name = b.ToLower()
                path.IsSome && (name = "virtual" || name = "file")
            )
            let (incType, path) = el
            if incType.ToLower() = "virtual" then
                Include(includeType = IncludeType.Virtual, path = path.Value)
            else
                Include(includeType = IncludeType.File, path = path.Value)
            



    let private pScriptCode: Parser<ASPBlocks, Unit> =
        let pScriptTag = skipChar '<'
                         >>. spaces
                         >>. skipStringCI "script"
                         >>. spaces

        let pStartTag =
            between
                (pScriptTag)
                (skipChar '>')
                (many1 pAttribute)

        let pEndTag = skipChar '<'
                      >>. spaces
                      >>. skipChar '/'
                      >>. spaces
                      >>. skipStringCI "script"
                      >>. spaces
                      >>. skipChar '>'
                      >>. spaces

        let hasAttr (attr:string) =
            List.exists (fun (attribs: string * string option) ->
                let (attrName, value) = attribs
                attrName.ToLower() = attr
            )

        let getAttr (attr:string) list : string option =
            if hasAttr attr list then
                List.find (fun (attribs: string * string option) ->
                    let (attrName, _) = attribs
                    attrName.ToLower() = attr.ToLower()
                ) list |> snd
            else
                None

        let hasRunatServer list =
            let res = getAttr "runat" list
            if res.IsSome && res.Value.ToLower() = "server" then true
            else false

        let getSrcAttr = getAttr "src"

        let getLanguageAttr = getAttr "language"

        pStartTag >>= (fun attribs ->
            if (hasRunatServer attribs) then
                let src = getSrcAttr attribs
                let language = getLanguageAttr attribs
                if (src.IsSome) then
                    if (language.IsNone) then
                        fail "No language"
                    else
                        preturn (
                            ScriptFile(
                                language = SpecificLanguage(language.Value),
                                path = src.Value
                            )
                        )
                else
                    manyCharsTill anyChar pEndTag
                    |>> (fun code ->
                        if(language.IsSome) then
                            Script(language = SpecificLanguage(language.Value),
                                   body = code)
                        else
                            Script(language = DefaultLanguage, body = code)
                    ) 
            else
                fail "No runat=server"
        )

    let private pCode = choice [
        attempt pProcessingDirective
        attempt pOutputDirective
        attempt pDefaultCode
        attempt pScriptCode
        attempt pInclude
    ]

    let private pHTML: Parser<ASPBlocks, Unit> =
        many1CharsTill anyChar (eof <|> followedBy pCode)
        |>> HTML

    let private pblocks = choice [
        pCode
        pHTML
    ]

    let parseString text =
        match runParserOnString (many pblocks) () "" text with
        | Success (result, _, _) -> List.toSeq result
        | Failure (error, _, _) -> raise (System.Exception(error))