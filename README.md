#snaplet-aws
A boilerplate [snaplet](https://github.com/snapframework/snap) to conveniently allow [amazonka](https://github.com/brendanhay/amazonka)
[aws](https://aws.amazon.com/) functions to be lifted into snap web handlers.

##Usage
1. Add the `AWSEnv` to your app's core state:

  ```haskell
    import Snap.Snaplet.AWS

    data App = App
    {...
    ,_awsEnv :: Snaplet AWSEnv
    ,...
    }
  ```
2. Derive (or manually create) lenses:

  ```haskell
    makeLenses ''App
  ```
3. Implement a `HasEnv` instance:

  ```haskell
    instance HasEnv App where
      environment = awsEnv . environment
  ```
4. In your initialisation function, intialise like so, providing the desired credential discovery method:

  ```haskell
    app :: SnapletInit App App
    app = makeSnaplet "app" "Your application" Nothing $ do
      ...
      awsSnaplet <- nestSnaplet "aws" awsEnv $ awsEnvInit Discover
      ...
      return $ App ... awsSnaplet ...
  ```
5. Ensure your `Handler b v` has a `MonadCatch` instance. E.G.

  ```haskell
    -- Type-golf implementation...
    instance MonadCatch (Handler App App) where
      catch m f = join $ liftIO $ catch (return m) (\x -> return $ f x)
  ```
6. You should now be able to call `liftAWS` to lift aws functions into your handler. E.G.:

  ```haskell
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
  ```

