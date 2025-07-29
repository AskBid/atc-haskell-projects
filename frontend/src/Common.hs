{-# LANGUAGE OverloadedStrings #-} 

module Common where

import Data.Text
import Common.Api
import Schema

buttonStyle :: Text  
buttonStyle = "mt-4 bg-blue-600 text-white py-2 px-4 rounded hover:bg-blue-700"

inputStyle :: Text  
inputStyle = "border border-gray-300 rounded px-3 py-2"

labelStyle :: Text
labelStyle = "text-sm font-medium"

divVerticalStyle :: Text
divVerticalStyle = "flex flex-col gap-4 p-4"

feMessage :: Maybe WSMessage -> Text
feMessage wsm = case wsm of
  Nothing -> "*Non Valid Message*"
  Just m -> case m of
    NewMessage m -> "TODO: user: " <> messageText m
    ConnectedClients _ -> "TODO: List of connections message."
    otherwise -> "TODO: unknown message."
