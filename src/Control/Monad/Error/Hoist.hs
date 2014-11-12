{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Error.Hoist
( HoistError(..)
, (<%?>)
, (<%!?>)
, (<?>)
, (<!?>)
) where

import Control.Monad.Error.Class

-- | A tricky class for easily hoisting errors out of partiality types (e.g.
-- 'Maybe', @'Either' e@) into a monad. The parameter @e@ represents the error
-- information carried by the partiality type @t@, and @e'@ represents the type
-- of error expected in the monad @m@.
--
class Monad m ⇒ HoistError m t e e' | t → e where

  -- | Given a conversion from the error in @t α@ to @e'@, we can hoist the
  -- computation into @m@.
  --
  hoistError
    ∷ (e → e')
    → t α
    → m α

instance MonadError e m ⇒ HoistError m Maybe () e where
  hoistError f = maybe (throwError $ f ()) return

instance MonadError e' m ⇒ HoistError m (Either e) e e' where
  hoistError f = either (throwError . f) return

-- | A flipped synonym for 'hoistError'.
(<%?>)
  ∷ HoistError m t e e'
  ⇒ t α
  → (e → e')
  → m α
(<%?>) = flip hoistError

infixr 9 <%?>
{-# INLINE (<%?>) #-}

-- | A version of '<%?>' that operates on values already in the monad.
--
(<%!?>)
  ∷ HoistError m t e e'
  ⇒ m (t α)
  → (e → e')
  → m α
m <%!?> e = do
  x ← m
  x <%?> e

infixr 9 <%!?>
{-# INLINE (<%!?>) #-}

-- | A version of @hoistError@ that ignores the error in @t α@ and replaces it
-- with a new one in @e'@.
--
(<?>)
  ∷ HoistError m t e e'
  ⇒ t α
  → e'
  → m α
m <?> e = m <%?> const e

infixr 9 <?>
{-# INLINE (<?>) #-}

-- | A version of @<?>@ that operates on values already in the monad.
(<!?>)
  ∷ HoistError m t e e'
  ⇒ m (t α)
  → e'
  → m α
m <!?> e = do
  x ← m
  x <?> e

infixr 9 <!?>
{-# INLINE (<!?>) #-}
