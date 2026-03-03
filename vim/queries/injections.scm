; Inject JSON highlighting into the json_value node
((json_value) @injection.content
  (#set! injection.language "json")
  (#set! injection.include-children true))
