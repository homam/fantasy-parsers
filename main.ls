{text, parse, lang} = require \bennu
{id, initial, last, map, concat-map} = require \prelude-ls

non-quoted-string = (quote) -> do ->
    arr <- parse.bind parse.eager parse.many1 (text.noneOf [quote])
    parse.always arr.reduce (+)

between-quotation = (quote) -> lang.between do
    text.character quote
    text.character quote
    non-quoted-string quote

quoted-string = do ->
    parse.choice do
        parse.attempt between-quotation \'
        parse.attempt between-quotation \"

fmap = (f, p) --> p.map f

to-string = (fmap (.reduce (+))) . parse.eager

word = to-string parse.many1 text.letter

space = to-string parse.many1 text.space

token = (p) ->
    a <- parse.bind p
    _ <- parse.bind space
    parse.always a

one-letter-shell-opt = do ->
    _ <- parse.bind text.character '-'
    name <- parse.bind text.letter
    parse.always [{name}]


many-one-letter-shell-opt = do ->
    [{name:l}] <- parse.bind one-letter-shell-opt
    name <- parse.bind word
    parse.always <| ((l + name).split '') |> map (-> name: it)

many-one-letter-shell-opt-with-value = do ->
    names <- parse.bind token one-letter-shell-opt
    value <- parse.bind parse.choice do 
        parse.attempt quoted-string 
        parse.attempt word
    parse.always <| (initial names) ++ [{name: (last names).name, value: value}]

multi-letter-shell-opt = do ->
    <- parse.bind lang.times 2, (text.character '-')
    name <- parse.bind word
    parse.always {name}

multi-letter-shell-opt-with-value = do ->
    {name} <- parse.bind token multi-letter-shell-opt
    value <- parse.bind parse.choice do 
        parse.attempt quoted-string
        parse.attempt word
    parse.always {name, value}

main-shell-opt = do ->
    s <- parse.bind parse.choice quoted-string, (to-string parse.many1 (text.noneOf [' ', '`']))
    parse.always {main-opt: s}

command = do ->
    command-name <- parse.bind token word
    opts <- parse.bind parse.eager lang.sepBy do 
        space
        parse.choice do
            parse.attempt many-one-letter-shell-opt-with-value
            parse.attempt many-one-letter-shell-opt
            parse.attempt one-letter-shell-opt
            parse.attempt multi-letter-shell-opt-with-value
            parse.attempt multi-letter-shell-opt
            main-shell-opt
    
    parse.always {command-name, opts: concat-map id, opts}



parse.parse do
    command
    """curl -Lm -O -ndo -H "accept: text/json" --compressed -s --data "beep" --header 'Length: 2' http://www.google.com"""
    # """
    #     curl  
    #         -X GET 
    #         https://furqanzafar-.sharepoint.com/_api/web/lists/getbytitle(%27Countries%27)/items 
    #         -H 'accept: application/json' 
    #         -H 'Authorization: Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsIng1dCI6Ik1uQ19WWmNBVGZNNXBPWWlKSE1iYTlnb0VLWSJ9.eyJhdWQiOiIwMDAwMDAwMy0wMDAwLTBmZjEtY2UwMC0wMDAwMDAwMDAwMDAvZnVycWFuemFmYXIuc2hhcmVwb2ludC5jb21AMWE1Mzk3MjAtNGNlMC00MTBmLTg4NTMtYTA1Y2Y5MjU3YWZiIiwiaXNzIjoiMDAwMDAwMDEtMDAwMC0wMDAwLWMwMDAtMDAwMDAwMDAwMDAwQDFhNTM5NzIwLTRjZTAtNDEwZi04ODUzLWEwNWNmOTI1N2FmYiIsIm5iZiI6MTQ1ODY3MTk1MCwiZXhwIjoxNDU4NzE1MTUwLCJuYW1laWQiOiIxMDAzYmZmZDk2ZGYzYTQ0IiwiYWN0b3IiOiI4YThjNWFmNy01NmFmLTQxODYtYTU1Zi0yMzI5MzdhNmYzYWFAMWE1Mzk3MjAtNGNlMC00MTBmLTg4NTMtYTA1Y2Y5MjU3YWZiIiwiaWRlbnRpdHlwcm92aWRlciI6InVybjpmZWRlcmF0aW9uOm1pY3Jvc29mdG9ubGluZSJ9.LBpzZde4SGJpaz2whgvxIrNViFf4K_CcAn7wKOrQSuQp1hC66wPFQ29HwvgDSV9rQuNbgKoZEWoJxlXd6N6KZ60OqwrOL7Tqiw-OLpcWvR3CWaf2IPnki_UT5gGcl3ZrS--7z9NczhayBUwRwoXcEXaXlq6AYmJk8JTf5VhFXfakH2nofBK47A0D6vDqOix1VpNsyeyubO25CbFeMho6EXbc7EPdOOQ6axJmXnXZ9sEfDEfeyltUwpnRiBk-n0zjdCozaaQXqWoJT9bg4pKpu789Jt1yZF4RDDdQkRBfLC-HlRFNUUpcQHxu3UqSNRW5nti8tk2BZW6mOcSEm-5L9Q' 
    #         -L
    # """.trim!
    null
    (result) -> 
        console.log "done"
        console.log JSON.stringify result, null, 4
    (err) -> console.error err