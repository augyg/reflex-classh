# reflex-classhss

ClasshSS-typed element builders for Reflex, providing a rapid UI development layer with compile-time CSS safety. Instead of passing raw Tailwind class strings to `elClass`, this library accepts `BoxConfig` and `TextConfigTW` values that are validated at compile time via ClasshSS's `classh'` Template Haskell splice.

## Exported API Surface

| Module | Purpose |
|--------|---------|
| `Classh.Reflex` | Re-exports all four leaf modules |
| `Classh.Reflex.El` | Typed `elClass` variants (`elTW`, `elTW'`, `elDynTW`, `elDynTW'`), bare `div_`, responsive `imgResponsive`, TH builders (`divClassh`, `textClassh`, `textPos`) |
| `Classh.Reflex.Layout` | Grid layout: `gridCol`, `col`, `row`, `colDyn`, `colFrom`, `gridColWhen`, `rotatingBox`, `overtopOf` |
| `Classh.Reflex.Text` | Styled text: `textS`, `dynTextS`, `textDynS`, `dynTextDynS`, `intercalate`, `paragraphs`, `paragraphs'`, `textPosition` |
| `Classh.Reflex.Place` | Centering: `centerSimple`, `centerHSimple`, `centerVSimple`, `placeCenterWidth`, `responsiveXPaddedRegion` |

## Core Types and Semantics

- **`BoxConfig`** -- ClasshSS configuration for box-model properties (spacing, colors, borders, shadows, transforms). Used with `.~~`, `.|~`, `.~^` operators.
- **`TextConfigTW`** -- ClasshSS configuration for text properties (font, size, weight, color, decoration). Separate from `BoxConfig`; never mix in the same `classh'` call.
- **`CompiledS`** -- The result of compile-time CSS compilation via `classh'`. A `Text` value guaranteed valid by the ClasshSS compiler.
- **`ColInt`** -- Grid column count (Col1..Col12) for `gridCol`.
- **`Rotation a b`** -- Controls element reordering on small screens: `CounterClockwise` or `Clockwise`.

## Usage Examples

### Responsive grid layout

```haskell
row [y .~~ TWSize 10] $ do
  gridCol Col12 $ do
    col [12, 12, 3] $ textS $(classh' [text_size .~~ XL3]) "hey"
    col [12, 12, 4] $ textS $(classh' [text_size .~~ XL3]) "hello"
    col [12, 12, 5] $ textS $(classh' [text_size .~~ XL3]) "howdy"
```

### Centered content with typed box styles

```haskell
centerSimple $
  elTW "div" (def & bgColor .~~ solidColor (Blue C500) & p .~~ TWSize 4 & br .~~ R_Lg) $
    textS $(classh' [text_color .~~ color White, text_weight .~~ Bold]) "Hello"
```

### Paragraphs with even spacing

```haskell
paragraphs [noTransition (TWSize 6)] $
  [ textS $(classh' [text_size .~~ XL2]) "First paragraph"
  , textS $(classh' [text_size .~~ Base]) "Second paragraph"
  ]
```

## Anti-patterns / Gotchas

- **Never mix `BoxConfig` and `TextConfigTW`** in a single `classh'` call -- they are separate types
- **Never use `custom .~` for properties that have type-safe equivalents** in ClasshSS
- **Never use flexbox** -- use `gridCol`/`col`/`pos` for layout, `centerSimple` for centering
- `imgSrcSet` calls `error` on empty input at runtime -- always pass at least one image

## Build & Test

```bash
nix-shell --run "cabal clean && cabal build"   # build with henforcer checks
nix-shell --run "cabal test"                     # run property tests
nix-shell --run "fourmolu --check src/ test/"    # verify formatting
```
