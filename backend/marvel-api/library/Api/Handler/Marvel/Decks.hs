module Api.Handler.Marvel.Decks (
  getApiV1MarvelDecksR,
  postApiV1MarvelDecksR,
  deleteApiV1MarvelDeckR,
) where

import Import hiding (delete, (==.))

import Data.Text (Text)
import Data.Text qualified as T
import Database.Esqueleto.Experimental
import GHC.Generics (Generic)
import Json
import Network.HTTP.Conduit (simpleHttp)

getApiV1MarvelDecksR :: Handler [Entity MarvelDeck]
getApiV1MarvelDecksR = do
  userId <- requireUserId
  runDB $ select $ do
    decks <- from $ table @MarvelDeck
    where_ (decks ^. MarvelDeckUserId ==. val userId)
    pure decks

data CreateDeckPost = CreateDeckPost
  { deckId :: Text
  , deckName :: Text
  , deckUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype UpgradeDeckPost = UpgradeDeckPost
  { udpDeckUrl :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON UpgradeDeckPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "udp"

postApiV1MarvelDecksR :: Handler (Entity MarvelDeck)
postApiV1MarvelDecksR = do
  userId <- requireUserId
  postData <- requireCheckJsonBody
  edeck <- fromPostData userId postData
  runDB $ either (error . show) insertEntity edeck

fromPostData ::
  (MonadIO m) => UserId -> CreateDeckPost -> m (Either String MarvelDeck)
fromPostData userId CreateDeckPost {..} = do
  edecklist <- getDeckList deckUrl
  pure $ do
    decklist <- edecklist
    pure $
      MarvelDeck
        { marvelDeckUserId = userId
        , marvelDeckInvestigatorName = "Undefined"
        , marvelDeckName = deckName
        , marvelDeckList = decklist
        }

getDeckList :: (MonadIO m) => Text -> m (Either String MarvelDBDecklist)
getDeckList url = liftIO $ eitherDecode <$> simpleHttp (T.unpack url)

deleteApiV1MarvelDeckR :: MarvelDeckId -> Handler ()
deleteApiV1MarvelDeckR deckId = do
  userId <- requireUserId
  runDB $ delete $ do
    decks <- from $ table @MarvelDeck
    where_ $ decks ^. persistIdField ==. val deckId
    where_ $ decks ^. MarvelDeckUserId ==. val userId
