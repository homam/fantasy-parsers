{text, parse, lang} = require \bennu
{map} = require \prelude-ls

a-or-b = parse.choice do
    parse.attempt text.character 'a' 
    parse.attempt text.character 'h'
    parse.attempt text.string 'hello'


non-quoted-string = (quote) -> do ->
    arr <- parse.bind parse.eager parse.many1 (text.noneOf [quote])
    parse.always arr.reduce (+)

between-quotation = (quote) -> lang.between do
    text.character quote
    text.character quote
    non-quoted-string quote

fmap = (f, p) --> p.map f

to-string = (fmap (.reduce (+))) . parse.eager

token = parse.manyTill text.anyChar, text.space

one-letter-shell-opt = do ->
    _ <- parse.bind text.character '-'
    name <- parse.bind text.anyChar
    parse.always {name}


many-one-letter-shell-opt = do ->
    {name:l} <- parse.bind one-letter-shell-opt
    name <- parse.bind to-string token
    parse.always <| ((l + name).split '') |> map (-> name: it)

command = do ->
    command-name <- parse.bind to-string token
    <- parse.bind text.space
    opts <- parse.bind parse.eager parse.many parse.choice do
        many-one-letter-shell-opt
        one-letter-shell-opt
    # <- parse.bind text.character '-'
    parse.always {command-name, opts}

# console.log <| parse.run a-or-b, 'hello'
# console.log <| parse.run between-quotation, '"he"llo' 
# parse.parse do
#   between-quotation '"'
#   '"hello"'
#   null
#   (result) -> console.log "done #{result}"
#   (err) -> console.error err

parse.parse do
    command
    'curl -Lk -O '
    null
    (result) -> 
        console.log "done"
        console.log JSON.stringify result, null, 4
    (err) -> console.error err