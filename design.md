# Reflex Component Pattern: Explicit Environment & Semantic Types

## 1. Motivation & Goals

- Address challenges in building larger Reflex applications (managing state, complex wiring, `rec` fragility).
- Goals:
  - Improve code clarity and predictability.
  - Promote componentization and reusability.
  - Establish clear patterns for shared state flow.
  - Reduce cognitive load for developers.
  - Maintain flexibility.

## 2. Core Concepts

- **Interface Records:** Define `AppState t`, `AppEnv t`, `AppEvents t`. Explain the role of each (shared state dynamics, explicit environment container, global event outputs). Show `Monoid` instance for `AppEvents`. Mention Lenses.
- **Type Aliases for Roles:** Introduce the four aliases (`InsularViewWidget`, `InsularActionWidget`, `OpenViewWidget`, `OpenActionWidget`). Explain the naming convention (Insular/Open for environment access, View/Action for output type). Explain the use of `RankNTypes`.
- **Explicit Environment Passing:** Explain why `AppEnv t` is passed explicitly as an argument to "Open" widgets (avoids `ReaderT` complexity in main `rec` loop).
- **Top-Level Wiring:** Describe the role of the main `rec` block in `mainWidget`.

## 3. Implementation Example

- Provide the full, working code example (correctly formatted!).
- Add comments explaining each section:
  - Imports/Extensions
  - Data Types and Records
  - Type Aliases
  - Widget Implementations (showing use of aliases and `AppEnv` input)
  - `routeToWidget`
  - `mainWidget` and the `rec` block (detail the order, state updates, `widgetHold`, and **especially** the event extraction pattern).

## 4. How to Use This Pattern

- **Adding a New Page/Component:**
  1.  Decide its role (Insular/Open, View/Action).
  2.  Choose the correct type alias.
  3.  Define its function, taking `AppEnv` if "Open".
  4.  Implement UI logic.
  5.  Return `AppEvents` if "Action", local output if "View". Use `mempty` for unused `AppEvents` fields.
  6.  Add a case to `routeToWidget` (if it's a page).
- **Adding Shared State:**
  1.  Add `Dynamic t NewState` field to `AppState t`.
  2.  Add corresponding `Event t NewStateUpdate` field to `AppEvents t`.
  3.  Update `Monoid` instance for `AppEvents`.
  4.  Add `holdDyn`/`foldDyn` for the new state in the main `rec` block, wired to the new event field.
  5.  Update `AppEnv` if necessary (usually not if it just contains `AppState`).
  6.  Access new state via `appEnv ^. env_appState . newAppStateField`.

## 5. Handling Other Scenarios

- **Configuration:** Pass a `Config` record as the `input` parameter in the alias.
- **Trigger/Event Inputs:** Pass via a field in the `input` parameter (e.g., using a `WidgetInput` record).
- **Asynchronous Actions (`performEvent`):** Use within component implementations as needed. Resulting events can feed into the returned `AppEvents`.

## 6. Benefits Summarized

- Clear component contracts via types/aliases.
- Explicit and predictable state flow.
- Improved structure and modularity potential.
- Uses standard Reflex features.

## 7. Limitations/Considerations

- Aliases rely on convention (compiler doesn't fully enforce role beyond signature).
- Explicit event extraction is verbose (but necessary for correctness in `rec`).
- Initial setup of records/aliases.
