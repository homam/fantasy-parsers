{alt, seqMap, seq, regex, string, optWhitespace, lazy, fail, none-of} = require 'Parsimmon'

a = string 'a'
b = string 'b'
a-or-b = alt a, b
# console.log <| a-or-b.parse 'c'

quotation = string '"'

non-quoted-string = (quote) -> 
    (none-of quote).many1!
    .map (.reduce (+))

quoted-string = (quote) ->
    seqMap do 
        string quote
        non-quoted-string quote
        string quote
        (s, v, e) -> v

# console.log <| non-quoted-string '"' .parse 'hello'

console.log <| quoted-string '"' .parse '"hello"'