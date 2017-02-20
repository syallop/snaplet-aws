{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , OverloadedStrings
           , Rank2Types
           , TemplateHaskell
           , UndecidableInstances
  #-}
{-|
Module     : Snap.Snaplet.AWS
Copyright  : (c) Samuel Yallop
Maintainer : syallop@gmail.com
Stability  : experimental

This module defines a 'Snaplet' for amazonka's bindings to AWS for use with the 'Snap' web framework.

To use this module:

1. Add the AWSEnv to your app's core state:

 @
   data App = App
   {...
   ,_awsEnv :: Snaplet AWSEnv
   ,...
   }
 @

2. Derive (or manually create) lenses:

@ makeLenses ''App @

3. Implement a 'HasEnv' instance:

@
  instance HasEnv App where
    environment = awsEnv . environment
@

4. In your initialisation function, intialise like so, providing the desired credential discovery method:

@
  app :: SnapletInit App App
  app = makeSnaplet "app" "Your application" Nothing $ do
    ...
    awsSnaplet <- nestSnaplet "aws" awsEnv $ awsEnvInit Discover
    ...
    return $ App ... awsSnaplet ...
@

5. Ensure your 'Handler b v' has a 'MonadCatch' instance. E.G.

@
  -- Type-golf implementation...
  instance MonadCatch (Handler App App) where
    catch m f = join $ liftIO $ catch (return m) (\x -> return $ f x)
@

6. You should now be able to call 'liftAWS' to lift aws functions into your handler. E.G.:

@
 -- Send a test email to a given address, returning whether sending was deemed successful by AWS.
 sendTestEmail :: Text -> Handler App App Bool
 sendTestEmail emailTo = do
  let emailBodyText = "<h1>Test</h1>"

      emailFrom     = "test@ourAddress.com"
      emailTo       = destination & dToAddresses .~ [emailTo]

      emailSubject  = content "Test email"
      emailBody     = body & bHTML .~ (Just $ content $ emailBodyText)
      emailMessage  = message emailSubject emailBody

      awsEmail      = sendEmail emailFrom emailTo emailMessage

  resp <- liftAWS $ within Ireland $ send awsEmail
  case resp&.sersResponseStatus of
    200 -> return True
    _   -> return False
@

-}
module Snap.Snaplet.AWS
  ( AWSEnv
  , awsEnvInit
  , environment
  ) where

import Control.Lens
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.State
import Network.AWS
import Snap.Snaplet

-- | State required for aws functions: a wrapped'Env'.
newtype AWSEnv= AWSEnv
  { _awsEnv :: Env
  }
-- Generate a lens into the AWSEnv's env for convenience.
makeLenses ''AWSEnv

-- | The AWSEnv state has a lens into it's 'Env'.
instance HasEnv AWSEnv where
  environment = awsEnv

-- | A Snaplet of a AWSEnv has a lens into it's 'Env'.
instance HasEnv (Snaplet AWSEnv) where
  environment = snapletValue . environment

-- | If A Handler has a MonadCatch instance and 'v' has a 'HasEnv' instance, then
-- aws functions can be lifted with 'liftAWS'

-- | If a 'Handler' has a 'MonadCatch' instance and 'v' is an instance of
-- 'HasEnv' (Is 'Snaplet AWSEnv'/'AWSEnv'/...), then aws functions can
-- be lifted with 'liftAWS'.
instance (MonadCatch (Handler b v),HasEnv v) => MonadAWS (Handler b v) where
  liftAWS aws = do
    env <- view (snapletValue . environment) <$> getSnapletState
    runResourceT $ runAWS env aws

-- | Initialise an 'AWSEnv' Snaplet by specifying how to find 'Credentials'.
awsEnvInit:: Credentials -> SnapletInit b AWSEnv
awsEnvInit credentials = makeSnaplet "aws-env" "AWS snaplet using amazonka." Nothing $ do
  env <- liftIO $ newEnv credentials
  return $ AWSEnv env

