{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TypeErrors where

import Lib
import GHC.Generics (Generic (..))

-- $setup
-- >>> :m +Data.Kind
-- >>> :m +Data.Proxy


data NoRep  = NoRep
data HasRep = HasRep deriving Generic


------------------------------------------------------------------------------
-- | >>> observe_phantom Proxy
-- ()
--
-- >>> observe_phantom (Just 5)
-- ...
-- ... It's not phantom!
-- ...
--
-- >>> observe_phantom
-- ...
-- ... No instance for (Show...
-- ...
observe_phantom
    :: UnlessStuck
         f
         (UnlessPhantomFcf
           (f PHANTOM)
           ('Text "It's not phantom!"))
    => f p
    -> ()
observe_phantom _ = ()


------------------------------------------------------------------------------
-- | >>> observe_no_type_instance HasRep
-- ()
--
-- >>> observe_no_type_instance NoRep
-- ...
-- ... No Rep instance for 'NoRep'
-- ...
observe_no_type_instance
    :: WhenStuck
         (Rep t)
         (DelayError ('Text "No Rep instance for " ':<>: ShowTypeQuoted t))
    => t
    -> ()
observe_no_type_instance _ = ()


------------------------------------------------------------------------------
-- | >>> pretty_print_list @'[1, 2]
-- ...
-- ... '1', and '2'
-- ...
--
-- >>> pretty_print_list @'["hello", "world", "cool"]
-- ...
-- ... "hello", "world", and "cool"
-- ...
--
-- >>> pretty_print_list @'[Bool]
-- ...
-- ... 'Bool'
-- ...
pretty_print_list :: DelayError (PrettyPrintList t) => ()
pretty_print_list = ()


------------------------------------------------------------------------------
-- | >>> delay_error
-- ...
-- ... do error!
-- ...
delay_error :: DelayError ('Text "do error!") => ()
delay_error = ()


------------------------------------------------------------------------------
-- | >>> :kind! Subst (Either Int Int) Int Bool
-- ...
-- = Either Bool Bool
sub_either_two_ints = ()


------------------------------------------------------------------------------
-- | >>> :kind! Subst (Either Int Bool) Int [Char]
-- ...
-- = Either [Char] Bool
sub_either_one_ints = ()


------------------------------------------------------------------------------
-- | >>> :kind! Subst (Either Int Bool) Either (->)
-- ...
-- = Int -> Bool
sub_arrow = ()


------------------------------------------------------------------------------
-- | >>> :kind! SubstVar (Either VAR Bool) [Char]
-- ...
-- = Either [Char] Bool
subv_either_one_ints = ()


------------------------------------------------------------------------------
-- | >>> :kind! SubstVar ((VAR :: Type -> Type -> Type) Int Bool) (->)
-- ...
-- = Int -> Bool
subv_arrow = ()

