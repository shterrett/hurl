module.exports = grammar({
  name: 'hurl',

  extras: $ => [/\s+/],

  rules: {
    source_file: $ => $.request,

    request: $ => seq(
      $.method,
      $.url,
      optional($.option_block),
    ),

    method: $ => choice(
      'GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'HEAD', 'OPTIONS',
    ),

    // Greedy non-whitespace — includes optional ?key=val&… query string
    url: $ => /[^\s]+/,

    option_block: $ => seq(
      '{',
      repeat($.block_item),
      '}',
    ),

    block_item: $ => choice(
      $.query_block,
      $.json_body,
      $.form_body,
      $.insecure,
    ),

    query_block: $ => seq(
      'query',
      '{',
      repeat($.key_value),
      '}',
    ),

    // Outer braces = option block; inner braces = the JSON value itself
    json_body: $ => seq(
      'json',
      '{',
      $.json_value,
      '}',
    ),

    // Recursive balanced-brace content — captures nested objects/arrays
    json_value: $ => seq(
      '{',
      repeat($._json_char),
      '}',
    ),

    _json_char: $ => choice(
      /[^"{}[\]]+/,
      $.string,
      seq('{', repeat($._json_char), '}'),
      seq('[', repeat($._json_char), ']'),
    ),

    form_body: $ => seq(
      'form',
      '{',
      repeat($.key_value),
      '}',
    ),

    insecure: $ => 'insecure',

    key_value: $ => seq(
      $.key,
      ':',
      $.string,
    ),

    key: $ => choice(
      $.bare_key,
      $.string,
    ),

    bare_key: $ => /[a-zA-Z0-9][a-zA-Z0-9_-]*/,

    string: $ => seq(
      '"',
      optional(/([^"\\]|\\.)+/),
      '"',
    ),
  },
});
