# pms-domain-model

## Positioning of pms-domain-model in the pty-mcp-server Project

`pms-domain-model` is a core internal package within the [`pty-mcp-server`](https://github.com/phoityne/pty-mcp-server) project. It defines the essential domain types — such as value objects, identifiers, parameters, and domain-specific data structures — that precisely capture the semantics of the problem domain.

The package does **not** contain any implementation of behavior, business logic, or state management. Instead, it is strictly responsible for representing the domain context as **purely declarative data**. This clear separation ensures that domain semantics are explicitly encoded and remain stable, independent of application workflows or infrastructure concerns.

The **domain logic itself** — that is, the implementation of business rules, behaviors, and state transitions — is separated and encapsulated in the `domain-service` layer. This design enables a clean architectural separation where the `pms-domain-model` defines the core domain vocabulary and invariants, while the `domain-service` implements the operations and business processes that act upon this data.

In this architectural style, inspired by principles from Onion Architecture, Clean Architecture, and similar approaches, the domain model forms the innermost core and defines the **ubiquitous language** and **invariants** of the business domain. Application logic, state transitions, and side effects are implemented in outer layers, such as the domain service and application layers, which rely on these core types.

---

## Insights into Domain Modeling in Haskell from This Development

During the development of `pty-mcp-server`, important insights were gained into how Haskell’s strong and expressive type system influences the conception and implementation of domain models:

- The domain model is naturally represented as a set of **data types** that encode domain semantics precisely, **without embedding behavior or mutable state**.

- The notion of domain context in Haskell goes beyond just data types. It includes **phantom types**, **newtypes**, and **tagged constructors** to express domain distinctions and constraints in a type-safe manner.

- Application context — including configuration, resources, logging, and effect management — is captured through **monad stacks** and **typeclass-based capabilities**, enabling composable and testable implementations of operational concerns.

This results in a distinct architectural pattern where domain data (the core model) is strictly separated from behavior and side effects, while still fully driving the correctness of program construction.

`pms-domain-model` thus acts as the **semantic center** of the system. Any computation in the application or domain service layers must align with and respect the data types and constraints defined here.

This approach effectively adapts architectural concepts inspired by Onion Architecture, Clean Architecture, and similar methodologies to Haskell. It leverages language-specific features such as strong static typing and monadic effects to maintain purity, testability, and maintainability.

By treating the **context** (implemented as monad stacks) as the bridge between domain data and application logic, this design achieves a clean separation of concerns and supports practical software development workflows in Haskell.

---

## Summary

pms-domain-model represents a Haskell-specific embodiment of architectural concepts inspired by Clean/Onion Architecture and similar approaches:  
a data-centric, behavior-agnostic core that precisely encodes the problem domain and provides the foundation for the system’s semantics, enabling robust, maintainable, and scalable application development.

Adopting and adapting these architectural principles in Haskell aligns well with the language’s characteristics. Haskell’s strong static typing and pure functional nature naturally facilitate clear separation of concerns and enforcement of domain invariants at the type level. This alignment enhances testability, maintainability, and expressiveness of the domain model, making the approach both feasible and beneficial.

---

## Package Structure
![Package Structure](https://raw.githubusercontent.com/phoityne/pms-domain-model/main/docs/51-1.png)

---

## Module Structure
![Package Structure](https://raw.githubusercontent.com/phoityne/pms-domain-model/main/docs/51-2.png)

---
