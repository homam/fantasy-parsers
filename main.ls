{text, parse, lang} = require \bennu

a-or-b = parse.choice do
	parse.attempt text.character 'a' 
	parse.attempt text.character 'h'
	parse.attempt text.string 'hello'


non-quoted-string = parse.bind do 
	parse.eager parse.many1 (text.noneOf ['"'])
	(arr) -> parse.always arr.reduce (+)

between-quotation = lang.between do
	text.character '"'
	text.character '"'
	non-quoted-string


# console.log <| parse.run a-or-b, 'hello'
# console.log <| parse.run between-quotation, '"he"llo' 
parse.parse do
	between-quotation
	'"hel"lo'
	null
	(result) -> console.log "done #{result}"
	(err) -> console.error err