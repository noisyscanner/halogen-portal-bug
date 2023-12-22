## The Bug
Not sure if this is a Halogen bug or a bug in the portal. It's very weird.

In this example we have a modal component with a close button.
The close action is raised up from the modal and handled by the parent component,
which sets some state to `false`, causing the portal to be removed.

1. When the parent component emits the action, it is handled correctly and the portalled HTML is removed from the DOM
2. When the child component emits the action, although the parent handles it ands sets the state, the portalled HTML is _not_ removed from the DOM

In the finaliser for the portal component, the call to `H.get` never binds in the latter case.
You can see in the console that the finaliser is called, as `Finalising portal` is logged,
however the state returned from `H.get` is never logged.

The component is then in a broken state. It appears to no longer be running (as `handleAction` isn't called),
but the subscription which writes those messages to `var` is still active.

## Setup instructions

Uses pnpm but swap for another manager if you want, shouldn't matter.

1. `spago build`
2. `pnpm install`
3. `pnpm dev`
4. Go to localhost:5173
5. Notice that clicking the close button within the parent component works as expected, but the child one does not.


