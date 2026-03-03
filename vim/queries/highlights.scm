; HTTP methods
(method) @keyword

; URL
(url) @string.special.url

; Section / option keywords
"query"    @keyword
"json"     @keyword
"form"     @keyword
(insecure) @keyword.modifier

; Key-value structure
(bare_key) @property
(key_value ":" @punctuation.delimiter)

; String literals (quoted keys and values)
(string) @string

; Option block delimiters
(option_block "{" @punctuation.bracket)
(option_block "}" @punctuation.bracket)

; JSON body braces
(json_body   "{" @punctuation.bracket)
(json_body   "}" @punctuation.bracket)
(json_value  "{" @punctuation.bracket)
(json_value  "}" @punctuation.bracket)

; query / form block braces
(query_block "{" @punctuation.bracket)
(query_block "}" @punctuation.bracket)
(form_body   "{" @punctuation.bracket)
(form_body   "}" @punctuation.bracket)
