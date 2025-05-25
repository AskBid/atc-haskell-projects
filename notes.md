# Notes to self

## Obelisk commands

🔧 `ob shell`
Purpose: Drops you into a nix-shell environment with all project dependencies loaded.

Use it when: You want to manually run ghci, ghcid, cabal, or build tools.

You're editing or debugging the project and need access to the dev environment.

Equivalent to: nix-shell but fully set up for your Obelisk project.

🚀 `ob run`
Purpose: Builds and runs your app in development mode, with live reloading (for frontend).

Use it when:

You want to test your app in the browser or on a device.

You want to simulate a live development server.

Note: Automatically sets up the environment (you don’t need to ob shell first).

💬 `ob repl`
Purpose: Launches a GHCi REPL inside the Obelisk project context.

Use it when:

You want to experiment with your app's Haskell code live.

You're testing small snippets or doing exploratory dev work.

`ob hoogle`

To have awareness of types and functions at your disposal, in particular from Reflex and Obelisk libraries


## HLS, LSP errors

at start I was getting this message:

```
Failed to find a HLS version for GHC 8.10.7
Executable names we failed to find: haskell-language-server-8.10.7,haskell-language-server:                                                                                                 
```

means that Neovim with LSP is looking for a version of haskell-language-server (HLS) that matches your project's GHC version (8.10.7), and it’s not finding it.

I have then installed a HLS version that is compatible with the indicated GHC 8.10.7:

```
ghcup install hls 1.6.1.0
```

you can check for all avaliable option with ghcup with `ghcup list`.

and then:

```
ghcup set ghc 8.10.7
```

BOOM!

## Data Families, typeclasses with other data inside

```
class Reflex t where
  data Event t :: * -> *
  data Behavior t :: * -> *
  data Dynamic t :: * -> *
  ...
```
Think of it this way: 
Reflex says: "If you give me a timeline type t, I'll tell you what an Event t, Behavior t, and Dynamic t look like."

## MonadWidget

Event t a is a value that represents a stream of values (of type a) happening over time on a timeline t.

MonadWidget t m is the environment (monad) in which you can create, manipulate, and respond to Events.

So: MonadWidget is the context, and Event is one of the tools you work with inside it.


So this line:
```
type MonadWidgetConstraints t m = (DomBuilder t m, OtherThing t m)
```
is defining a type alias for a tuple of constraints.

You can now write:

```
someWidget :: MonadWidgetConstraints t m => m ()
```
Instead of this:

```
someWidget :: (DomBuilder t m, OtherThing t m) => m ()
```
It’s just for convenience, readability, and sometimes abstraction.

You use DomBuilder when you do things like el "div" or text "Hello".

You use MonadWidget when you also want to hold dynamic state, respond to events, run effects, etc.

```haskell
widget1 :: MonadWidget t m => m (Event t T.Text)
widget1 = do
  btn <- button "Click me 1"
  pure $ "One" <$ btn
-- - **Creates a button** with label "Click me 1".
-- - `button` returns `Event t ()` — an event that fires when the button is clicked.
-- - We use `<$` to **replace the unit (`()`) with "One"**, so it becomes `Event t "One"`.
--
-- Result: this widget gives us an event stream that emits `"One"` whenever its button is clicked.

widget2 :: MonadWidget t m => m (Event t T.Text)
widget2 = do
  btn <- button "Click me 2"
  pure $ "Two" <$ btn
-- Same thing as widget1, but emits "Two" instead.

--             \/  \/   \/   \/
-- - This is your main app widget. It builds and runs in a Reflex environment.
mainWidget'' :: MonadWidget t m => m ()
mainWidget'' = do
  toggleDyn <- toggle False =<< button "Switch Widgets"
  -- This ^ creates a toggleable `Dynamic t Bool`.
  -- It starts with False, and flips every time you click the "Switch Widgets" button.
  -- toggle is a helper that toggles between True and False each time the event fires.
  let widgetDyn = ffor toggleDyn $ \b -> if b then widget1 else widget2
  -- - We're mapping (`ffor`) over `toggleDyn` (which changes between True/False).
  -- - Depending on the current value of `b`, we choose `widget1` or `widget2`.
  -- BUT IMPORTANTLY:  
  -- `widget1` and `widget2` are **actions that build widgets**, so `widgetDyn` has type:
  --  :: Dynamic t (m (Event t Text))
  -- This is a Dynamic containing widget-building actions.
  dynEv <- widgetHold widget1 (updated widgetDyn)
  --   This is where the magic starts.
  --
  -- - `widgetHold` takes:
  --   1. An initial widget to build (here: `widget1`)
  --   2. An `Event t (m a)` — a stream of widget-building actions (which we get from `updated widgetDyn`)
  --
  -- It:
  -- - **Runs the initial widget** immediately.
  -- - Every time `widgetDyn` changes, it:
  --   - Builds the new widget (`widget1` or `widget2`)
  --   - Replaces the DOM
  --   - Gives you back the new event
  --
  -- So `dynEv` is:
  --
  -- ```haskell
  -- Dynamic t (Event t Text)
  -- That is: a dynamic event stream — one that changes every time the active widget changes.
  let switchedEv = switchDyn dynEv
  --   Now we flatten it.
  --
  -- - `dynEv` is a **`Dynamic` containing an `Event`**.
  -- - `switchDyn` pulls out **the current Event inside**, and follows it.
  --
  -- That means:
  -- - Before switching: the event is `"One"`-emitting from `widget1`
  -- - After switching: it's `"Two"`-emitting from `widget2`
  -- - At any time, **`switchedEv` only emits from the currently visible widget**
  --
  -- Type: 
  -- switchedEv :: Event t Text
  el "div" $ do
    text "Last clicked: "
    holdDyn "" switchedEv >>= dynText
  -- - We build a `<div>` and show the **latest string emitted by `switchedEv`**.
  -- - `holdDyn "" switchedEv` creates a `Dynamic t Text` — it holds the latest event value.
  -- - `dynText` renders the text and updates automatically.
  --
  -- So if the current widget is `widget1` and you click its button, the screen shows:
  --
  -- Last clicked: One
  -- Then you toggle to `widget2`, click that button:
  --
  -- Last clicked: Two
  

-- | Line                             | What's it do                                          |
-- |----------------------------------|-------------------------------------------------------|
-- | `widget1`, `widget2`             | Build buttons that emit `"One"` or `"Two"`            |
-- | `toggleDyn`                      | Flip between `True`/`False` with a toggle button      |
-- | `widgetDyn`                      | Choose widget1 or widget2 based on toggle             |
-- | `widgetHold`                     | Dynamically replace widgets as toggle changes         |
-- | `switchDyn`                      | Follow the event from the **currently active** widget |
-- | `holdDyn` + `dynText`            | Show the latest emitted `"One"` or `"Two"`            |
--
-- ---
--
-- Let me know if you want a diagram of the event flow — that can help a lot with internalizing this!
```

is Behavior the value that gets changed when an Event triggers? and the Dynamic the connection between Event and Beahvior? 

So if I have an Event t Int triggered from a buttong, I can create a Dynamic that has that Event and a initial Behavior of 0, when I click the button, the Event t 1 triggers, making the Dynamic change the Behavior to 1 as well?

An Event t a represents discrete points in time when something happens
Think of Event as a stream: [ , , 1, , 2, , , 3 ]

A Behavior t a is like a function from time to a: it represents a value that exists at all times.
Think of Behavior as: a function: time → value

A Dynamic is a combination of:
A Behavior t a (the current value)
An Event t a (the updates)
It’s a time-varying value that you can observe and react to.

```
Time:    ──►────────────────────────────────────────►

Event t Int (e):
          ┌────┐       ┌────┐             ┌────┐
          │ 1  │──────▶│ 2  │──────▶  ... │ 3  │────▶
          └────┘       └────┘             └────┘
          (Button clicked with payload)

holdDyn 0 e
        │
        ▼

Dynamic t Int:
    - Behavior (current value over time)
    - Event (updates when e fires)

    Behavior:
        Value:  0────1────2───────────────3──────▶
                ▲    ▲    ▲               ▲
              init   │    │               │
                   on e  on e           on e

    updated Event:
                ┌────┐ ┌────┐           ┌────┐
                │ 1  │ │ 2  │    ...    │ 3  │
                └────┘ └────┘           └────┘
```

You rarely use Behavior directly
In day-to-day Reflex programming, you mostly deal with:

Event t a – something just happened

Dynamic t a – a value that changes over time

You rarely interact directly with Behavior t a, because:

🔄 Dynamic already wraps a Behavior and an Event.

🧠 Behavior is not reactive by itself — it doesn’t notify you when it changes.

📤 You can only sample Behavior at a specific moment, usually when an event occurs.

Sampling with tag, attach, or sample
You might want to grab the current value of a Behavior at the moment an event fires:

```haskell

-- Event t () fires when button is clicked
-- Dynamic t Text holds current input value
do
  inputDyn <- inputElement def
  submitBtn <- button "Submit"

  let inputBehavior = current (value inputDyn)
  let submitEvent = tag inputBehavior submitBtn
  -- submitEvent :: Event t Text, value of input when button is clicked
```

## `&` Vs `$`

In Haskell, when we say "$ applies the value to the function" or "& applies the function to the value," what we're really saying — more precisely — is:

They change how expressions are grouped, not when or whether anything gets evaluated.

$ → “Put parentheses around everything that comes after”

✅ f $ x + y + z
⟶ f (x + y + z)

& → “Put parentheses around everything that comes before” and inverse it.

✅ x + y + z & f
⟶ f (x + y + z)

So in a very Haskell-y way:

$ says “everything after me belongs to the function on the left.”
& says “everything before me is the argument to the function on the right.”

### `&` In Lenses

the following are all equivalent:

```haskell

set lens newValue structure

(lens .~ newValue) structure

lens .~ newValue $ structure

structure & lens .~ newValue

```

## Associativity

I was following an old Reflex video from Galen (https://youtu.be/7MuX6Z2AB70?si=ULZcQa2CcFPWhWJL) and ended up having a trip on associativity and precedence in Haskell.

this is the line that got me:

```haskell
fmap value $ inputElement $ def & inputElementConfig_initialValue .~ "Red"
```

so the `$` operator basically groups all that is on the right.
while `&` groups all that is on the left and then reverses it to apply the left to the right.

```
(+1) $ 5 + 6 + 7 
>>> 19
5 + 6 + 7 & (+1)
>>> 19
```

given that `.~` has infix value 4, it binds first, then `&` which is 1 and then `$` with 0.

```haskell
--1 
fmap value $ inputElement $ def & inputElementConfig_initialValue .~ "Red"
--2 `.~` application:
fmap value $ inputElement $ def & (inputElementConfig_initialValue .~ "Red")
--3 `&` application:
(fmap value $ inputElement $ def) & (inputElementConfig_initialValue .~ "Red")
-- thinking emoticons.. confusion!
```

and here is a mistake because I was reading it as this example:

```haskell
--1
sum [1,2,3] & (+1)
--2 `&` application:
(sum [1,2,3]) & (+1) 
--3 ... `&`: 
(+1) (sum [1,2,3])
(+1) 6
>>> 7
```

But in reality that's not what is happening given that function application has the highest precedence the following is more accurate on the proceedings: 

```haskell
--1
sum [1,2,3] & (+1)
--2 function application
(sum [1,2,3]) & (+1)
--3 
6 & (+1)
--4 and now `&` application
(+1) 6 
>>> 7
```

going back to the initial line:

```haskell
fmap value $ inputElement $ def & inputElementConfig_initialValue .~ "Red"
```

Also in this case the function application is the first precedence to go, but `fmap` has two arguments so it gets partially applied and, as an intiuition, stands there waiting for the operators on the right to sort their things out.

```haskell
(\x -> fmap value x) $ inputElement $ def & inputElementConfig_initialValue .~ "Red"
```

So first the higher infix value is `.~` so it picks up things stopping at the other opearators (not sure if that's the right intuition to have, but it worked so far)

```
(\x -> fmap value x) $ inputElement $ def & (inputElementConfig_initialValue .~ "Red")
```

Then the following precedence is `&`, and again it stops at the first operator encountered.

```
(\x -> fmap value x) $ inputElement $ (def & (inputElementConfig_initialValue .~ "Red"))
--2 applies the right to the left:
(\x -> fmap value x) $ inputElement $ ((inputElementConfig_initialValue .~ "Red") def)
--3 `$` application:
(\x -> fmap value x) $ (inputElement ((inputElementConfig_initialValue .~ "Red") def))
--4 the other `$` application
(\x -> fmap value x) ((inputElement ((inputElementConfig_initialValue .~ "Red") def)))
```

and you end up with a Lens that modifies one value of a default-InputElement that is then passed to `fmap value`.


## Lenses

Going through TextAreaElement to make sure I have in mind all of the "connections" used in the library

```haskell
data TextAreaElement er d t
  = TextAreaElement { _textAreaElement_value :: Dynamic t Text
               , _textAreaElement_input :: Event t Text
               , _textAreaElement_hasFocus :: Dynamic t Bool
               , _textAreaElement_element :: Element er d t
               , _textAreaElement_raw :: RawTextAreaElement d
               }

textAreaElement :: TextAreaElementConfig er t (DomBuilderSpace m) -> m (TextAreaElement er (DomBuilderSpace m) t)
  default textAreaElement :: ( MonadTransControl f
                             , m ~ f m'
                             , DomBuilderSpace m' ~ DomBuilderSpace m
                             , DomBuilder t m'
                             )
                          => TextAreaElementConfig er t (DomBuilderSpace m) -> m (TextAreaElement er (DomBuilderSpace m) t)
  textAreaElement = lift . textAreaElement
```

This function is defined via DomBuilder, which abstracts over DOM creation. The implementation you're seeing:
`textAreaElement = lift . textAreaElement`
means you're looking at a default instance for when `m` is a monad transformer — i.e., it just lifts to the underlying monad. You are not seeing the real implementation.

The actual implementation of textAreaElement is defined deeper in Reflex-DOM — specifically in a module like `Reflex.Dom.Widget.Input`. 
You don’t see it here because this file only defines the types and default lifted version.
In the real implementation, it likely:

1. Uses element (a function from Reflex-DOM) to create the DOM node.
2. Applies the ElementConfig to set up the attributes and events.
3. Sets the initial value into the DOM.

```haskell
data TextAreaElementConfig er t m = TextAreaElementConfig 
  { _textAreaElementConfig_initialValue :: Text
  , _textAreaElementConfig_setValue :: Maybe (Event t Text)
  , _textAreaElementConfig_elementConfig :: ElementConfig er t m
  }

textAreaElementConfig_initialValue :: Lens' (TextAreaElementConfig er t m) Text
textAreaElementConfig_initialValue f (TextAreaElementConfig a b c) = (\a' -> TextAreaElementConfig a' b c) <$> f a

textAreaElementConfig_elementConfig :: Lens (TextAreaElementConfig er1 t m1) (TextAreaElementConfig er2 t m2) (ElementConfig er1 t m1) (ElementConfig er2 t m2)
textAreaElementConfig_elementConfig f (TextAreaElementConfig a b c) = (\c' -> TextAreaElementConfig a b c') <$> f c

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (TextAreaElementConfig er t s) where
  def = TextAreaElementConfig 
    { _textAreaElementConfig_initialValue = ""
    , _textAreaElementConfig_setValue = Nothing
    , _textAreaElementConfig_elementConfig = def
    }
```

Above we can see how the main config for the Area is defined, the lenses set up for its modification, and the default implementation.

And below the same for the nested ElementConfig.

```
data ElementConfig er t s = ElementConfig 
  { _elementConfig_namespace :: Maybe Namespace
  , _elementConfig_initialAttributes :: Map AttributeName Text
  , _elementConfig_modifyAttributes :: Maybe (Event t (Map AttributeName (Maybe Text)))
  , _elementConfig_eventSpec :: EventSpec s er
  }

elementConfig_namespace :: Lens' (ElementConfig er t s) (Maybe Namespace)
elementConfig_namespace f (ElementConfig a b c d) = (\a' -> ElementConfig a' b c d) <$> f a

elementConfig_initialAttributes :: Lens' (ElementConfig er t s) (Map AttributeName Text)
elementConfig_initialAttributes f (ElementConfig a b c d) = (\b' -> ElementConfig a b' c d) <$> f b

elementConfig_eventSpec :: Lens (ElementConfig er1 t s1) (ElementConfig er2 t s2) (EventSpec s1 er1) (EventSpec s2 er2)
elementConfig_eventSpec f (ElementConfig a b c d) = (\d' -> ElementConfig a b c d') <$> f d

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (ElementConfig er t s) wher
  def = ElementConfig
    { _elementConfig_namespace = Nothing
    , _elementConfig_initialAttributes = mempty
    , _elementConfig_modifyAttributes = Nothing
    , _elementConfig_eventSpec = def
    }
```

Now imagine you need to create a textArea with both setting a value in the the outer config and in the nested config:

```
area <- textareaelement $ def 
  & textAreaElementConfig_initialValue .~ "ciao"
  & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~ "class" =: "classius"
```

You can combine the lenses to achieve the wanted result.

But here below is a technique used with class instances to avoid even the more verbose lenses combination:

```haskell
class InitialAttributes a where
  initialAttributes :: Lens' a (Map AttributeName Text)

instance InitialAttributes (ElementConfig er t m) where
  initialAttributes = elementConfig_initialAttributes

instance InitialAttributes (TextAreaElementConfig er t m) where
  initialAttributes = textAreaElementConfig_elementConfig . elementConfig_initialAttributes
```

With this InitialAttributes class we can now do something like this:

```
area <- textAreaElement $ def 
        & textAreaElementConfig_initialValue .~ "ciao"
        & initialAttributes .~ "class" =: "classius"
```

Same concept is used for this other ElementConfig attribute:

```
class ModifyAttributes t a | a -> t where
  modifyAttributes :: Reflex t => Lens' a (Event t (Map AttributeName (Maybe Text)))

instance ModifyAttributes t (ElementConfig er t m) where
  modifyAttributes = elementConfig_modifyAttributes

instance ModifyAttributes t (TextAreaElementConfig er t m) where
  modifyAttributes = textAreaElementConfig_elementConfig . elementConfig_modifyAttributes
```
