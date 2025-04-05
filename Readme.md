# Reflex Structured Application Pattern Demo

## Overview

This project serves as a demonstration and reference implementation for building web applications using the [Reflex](https://reflex-frp.org/) framework and [Reflex-DOM](https://github.com/reflex-frp/reflex-dom).

It specifically showcases an architectural pattern designed to improve structure, maintainability, and clarity for larger Reflex applications by focusing on:

- **Explicit State Management:** Using dedicated data records (`AppState`, `AppEnv`) to manage shared application state.
- **Clear Event Flow:** Defining an `AppEvents` record to centralize events that trigger updates to shared state.
- **Semantic Component Signatures:** Employing type aliases (`InsularViewWidget`, `OpenActionWidget`, etc.) to clarify a component's role concerning shared state and event production.
- **Modular Design:** Separating concerns into distinct modules for types, UI helpers, pages, routing, and main application wiring.
- **Explicit Environment Passing:** Passing the shared application environment (`AppEnv`) explicitly to components that require it.
- **Central Wiring:** Using a single top-level `rec` block in `Main.hs` to manage state dynamics and connect event flows.

This example implements a simple multi-page application with a shared counter to illustrate these principles in action.

## Features

- Basic client-side routing using `widgetHold`.
- Shared state (`Dynamic` counter) managed centrally and passed to relevant pages.
- Modular structure (`App.Types`, `Pages/*`, `App.Router`, `UI.Common`, `Main.hs`).
- Interface Records (`AppState`, `AppEnv`, `AppEvents`) defining the application's data contract.
- Semantic Type Aliases (`InsularViewWidget`, `InsularActionWidget`, `OpenViewWidget`, `OpenActionWidget`) indicating component roles.
- Embedded CSS via `<style>` tag using `mainWidgetWithHead`.

## Building and Running

This project is intended to be built using Nix.

**Prerequisites:**

- [Nix](https://nixos.org/download.html) package manager installed.

**Method 1: Using `nix-shell` (Development)**

1.  Navigate to the project's root directory.
2.  Start a Nix shell with the required dependencies:
    ```bash
    nix-shell
    ```
3.  Once inside the shell, build and run the GHC version using Cabal:
    ```bash
    cabal run
    ```
    This will compile the application using GHC and start a web server (likely via `jsaddle-warp` if configured in your Nix setup). Open your browser to the indicated address (usually `http://localhost:3000` or similar).

**Method 2: Using `nix-build` (GHC Backend)**

1.  Navigate to the project's root directory.
2.  Build the GHC executable directly:
    ```bash
    nix-build -A ghc.hello-world
    ```
    (Replace `ghc.hello-world` with the correct attribute path from your Nix configuration if it differs).
3.  This creates a `./result` symlink. The executable will be located at:
    ```bash
    ./result/bin/hello-world
    ```
4.  Run the executable:
    ```bash
    ./result/bin/hello-world
    ```
    This will start the web server. Access it via your browser.

**Method 3: Using `nix-build` (GHCJS Backend)**

1.  Navigate to the project's root directory.
2.  Build the GHCJS assets:
    ```bash
    nix-build -A ghcjs.hello-world
    ```
    (Replace `ghcjs.hello-world` with the correct attribute path from your Nix configuration).
3.  This creates a `./result` symlink. The compiled JavaScript and HTML assets will be inside:
    ```bash
    ./result/bin/hello-world.jsexe/
    ```
4.  These are static assets. You need to serve them using a simple HTTP server. For example, using Python 3:
    ```bash
    cd ./result/bin/hello-world.jsexe/
    python -m http.server 8000
    ```
5.  Open your browser to `http://localhost:8000`.

## Code Overview

- **`App.Types`:** Defines the core data structures (`Route`, `AppState`, `AppEnv`, `AppEvents`) that form the contract for the application. It also defines the semantic type aliases (`OpenActionWidget`, etc.) used to clarify component roles within _this_ application.
- **`UI.Common`:** Contains simple UI utility functions (`tShow`, `css`) and the definition of the embedded CSS (`appStyles`) and the `<head>` content (`headWidget`).
- **`Pages.*`:** Each module implements a specific page widget (`homePage`, `aboutPage`, `profilePage`). They adhere to the type signatures defined using aliases from `App.Types` and take `AppEnv` explicitly if needed.
- **`App.Router`:** Contains the `routeToWidget` function, responsible for mapping a `Route` value to the corresponding page widget function.
- **`Main`:** The application entry point. It uses `mainWidgetWithHead`. The core logic resides in a `rec` block which:
  - Initializes and defines the application's reactive state (`AppState` dynamics).
  - Constructs the `AppEnv` containing this state.
  - Uses `widgetHold` driven by the current route to display the active page, passing the `AppEnv` down via `routeToWidget`.
  - Extracts `AppEvents` output by the active page using the standard `switch . current . fmap` pattern.
  - Wires these events back into the state definitions (`holdDyn`, `foldDyn`) to close the reactive loop.

## Architectural Pattern Details

- **Interface Records:** Using `AppState`, `AppEnv`, and `AppEvents` provides strong typing for the data flowing through the application. `AppEnv` acts as a context bundle passed explicitly. `AppEvents` centralizes outputs that affect shared state.
- **Type Aliases:** The aliases (`InsularViewWidget`, `InsularActionWidget`, `OpenViewWidget`, `OpenActionWidget`) serve as _semantic documentation_ in function signatures, quickly conveying whether a component interacts with the shared `AppEnv` and whether it produces global `AppEvents`. They leverage `RankNTypes` for generality over the specific `MonadWidget` instance.
- **Explicit Environment:** Passing `AppEnv` explicitly avoids the complexities of integrating `ReaderT` directly into the top-level `rec` block, leading to a more standard and often easier-to-debug wiring pattern. Components needing the environment simply declare it as their first argument.
- **Event Extraction:** The pattern `let beh = current dyn; let evBeh = fmap accessor beh; let ev = switch evBeh` is used to robustly extract individual event streams from the `Dynamic` output of `widgetHold` inside the recursive context.

## Potential Next Steps

- Integrate browser history updates (using `reflex-dom-contrib` or FFI).
- Implement asynchronous operations (e.g., data fetching using `performEvent`).
- Add loading and error states to `AppState`.
- Create more sophisticated components (forms, lists).
- Further modularize (e.g., separate state update logic).
