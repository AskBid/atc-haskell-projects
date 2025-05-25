```
button :: (...) => Text -> m (Event t ())

display :: (Show a, ... ) => Dynamic t a -> m ()

count :: (Num b, ...) => Event t a -> m (Dynamic t b)

text :: (...) => Text -> m ()

dynText :: (...) => Dynamic t Text -> m ()

el :: (...) => Text -> m a -> m a

blank :: Monad m => m ()

elAttr :: (...) => Text -> Map Text Text -> m a -> m a

elClass :: (...) => Text -> Text -> m a -> m a

elDynAttr  :: (...) => Text -> Dynamic t (Map Text Text) -> m a -> m a

mdo -- to make recursive the whole `do` block

rec -- to make recursive only a portion of the `do` block

toggle :: (...) => Bool -> Event t a -> m (Dynamic t Bool)

elDynClass :: (...) => Text -> Dynamic t Text -> m a -> m a

foldDyn :: (...) => (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)

leftmost :: Reflex t => [Event t a] -> Event t a

mergeWith :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a

textInput :: (...) => TextInputConfig t -> m (TextInput t)

```
