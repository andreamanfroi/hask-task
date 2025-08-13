{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Web.Scotty
import Data.Aeson
    ( FromJSON
    , ToJSON
    , object
    , (.=)
    , genericToJSON
    , genericParseJSON
    , fieldLabelModifier
    )
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.HTTP.Types.Status (status404)

data Todo = Todo
  { todoId      :: Text
  , description :: Text
  , completed   :: Bool
  } deriving (Show, Generic)

instance ToJSON Todo where
  toJSON = genericToJSON Aeson.defaultOptions
    { fieldLabelModifier = \f -> if f == "todoId" then "id" else f }

data NewTodo = NewTodo
  { description :: Text
  , completed   :: Bool
  } deriving (Show, Generic)

instance FromJSON NewTodo

type TodoDB = TVar (Map Text Todo)

createTodo :: NewTodo -> IO Todo
createTodo (NewTodo desc comp) = do
    uuid <- UUIDv4.nextRandom
    let uuidText = T.pack (UUID.toString uuid)
    return $ Todo uuidText desc comp

main :: IO ()
main = do
    todoDB <- newTVarIO Map.empty

    scotty 3000 $ do
        
        -- GET /todos
        get "/todos" $ do
            todos <- liftIO $ readTVarIO todoDB
            json (Map.elems todos)

        -- GET /todos/:id
        get "/todos/:id" $ do
            tid <- param "id"
            todos <- liftIO $ readTVarIO todoDB
            case Map.lookup tid todos of
                Just t  -> json t
                Nothing -> do
                    status status404
                    json $ object ["error" .= ("Todo not found" :: Text)]

        -- POST /todos
        post "/todos" $ do
            newTodoData <- jsonData :: ActionM NewTodo
            newT <- liftIO $ createTodo newTodoData
            liftIO $ atomically $ modifyTVar' todoDB (Map.insert (todoId newT) newT)
            json newT

        -- PUT /todos/:id
        put "/todos/:id" $ do
            tid <- param "id"
            NewTodo desc comp <- jsonData
            let updatedTodo = Todo tid desc comp
            liftIO $ atomically $ modifyTVar' todoDB (Map.adjust (const updatedTodo) tid)
            json updatedTodo

        -- DELETE /todos/:id
        delete "/todos/:id" $ do
            tid <- param "id"
            liftIO $ atomically $ modifyTVar' todoDB (Map.delete tid)
            json $ object ["message" .= ("Todo deleted" :: Text)]
