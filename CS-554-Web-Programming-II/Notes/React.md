# React

- Component Based Architecture

  - Separation of concerns and code resuability at the heart
  - Component classes are the core building block

- Declarative style

  - Developers write how it should be, not what to do step-by-step
  - More readable than imperative style

- Virtual DOM

  - Uses DOM diffing to reconcile between existing and desired, updated view

- It's a **thick client**, rendering, computation, and mostly everything else is done client-side and the page is never reloaded (SPA)
- ![SPA with React - Diagram](./assets/react-diagram.png)

## React Components

### Two types of components

- Function Components
  - Used to be stateless, but can now handle state with React Hooks
- Class Components
  - Not used in modern react applications, but useful to know for legacy systems. No longer necessary as function components can handle states thanks to React Hooks.
