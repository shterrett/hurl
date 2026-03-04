# Coding Guidelines for Hurl

Use stack to build, run, and test the project

## Testing

Code should be tested using the `hspec` library.

Tests should be included for every functio in the public interface of a module.

Property tests are preferred when they are reasonable to write.
Use `hedgehog` for property tests.

Tests for modules should be in test modules that mirror the source structure (suffixed with Spec).

## Coding Styles

Do not duplicate code. Extract shared types and functions into common modules.

Prefer shorter named functions that are composed to form larger functionality.

Make illegal states unrepresentable by constructing types. For example, use `NonEmpty` list and custom sum types
Related: "Parse, don't validate". Ensure data is parsed into a type that encodes all invariants instead of using a looser type and then validating the data.

Be liberal with newtypes for differentiating data types. 

Derive Show. Use `prettyprinter` for printing human-friendly representations.

## Treesitter grammar

Whenever updating the parser, make sure to update the grammar in vim/ as well.
