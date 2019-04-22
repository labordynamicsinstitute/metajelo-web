module Metajelo.XPaths where

import Prelude

import Data.Int                          (toNumber)
import Data.Maybe                        (Maybe(..), fromMaybe)
import Data.Natural                      (intToNat)
import Effect                            (Effect)
import Effect.Aff                        (Aff)
import Effect.Class                      (liftEffect)
import Effect.Console                    (logShow)
import Effect.Exception                  (error, throwException)
import Foreign                           (isUndefined, isNull, unsafeToForeign)

import Metajelo.Types

import Test.Data                         as TD
import Test.Unit                         (suite, test)
import Test.Unit.Main                    (runTest)
import Test.Unit.Assert                  as Assert

import Web.DOM.Document                  (Document, getElementsByTagName, toNode)
import Web.DOM.DOMParser                 (DOMParser, makeDOMParser, parseXMLFromString)
import Web.DOM.Document.XPath            (NSResolver)
import Web.DOM.Document.XPath            as XP
import Web.DOM.Document.XPath.ResultType as RT
import Web.DOM.Element                   (Element, fromNode, getAttribute)
import Web.DOM.Element                   as Ele
import Web.DOM.HTMLCollection            (item)
import Web.DOM.Node                      (Node, nodeName)

import Unsafe.Coerce (unsafeCoerce)
import Prim.TypeError (QuoteLabel, class Warn)

-- | The current Metajelo namespace URI, provided as a fallback
defaultMetajeloNS :: String
defaultMetajeloNS = "http://ourdomain.cornell.edu/reuse/v.01"

-- | Resolver that returns `defaultMetajeloNS` as a fallback
getMetajeloResolver :: Node -> Document -> Effect NSResolver
getMetajeloResolver node doc = do
  nsResolver <- XP.defaultNSResolver node doc
  -- traceM nsResolver
  nodeEleMay :: Maybe Element <- pure $ fromNode node
  defaultNS :: String <- getDefaultNS nodeEleMay
  pure $ XP.customNSResolver $ makeMjNSResFun nsResolver defaultNS
  where
    getDefaultNS :: Maybe Element -> Effect String
    getDefaultNS mayElem = do
      case mayElem of
        Nothing -> pure $ defaultMetajeloNS
        Just elem -> map nsOrGuess (getAttribute "xmlns" elem)
    nsOrGuess :: Maybe String -> String
    nsOrGuess nsMay = fromMaybe defaultMetajeloNS nsMay
    makeMjNSResFun :: NSResolver -> String -> String -> String
    makeMjNSResFun nsr defNS prefix = case XP.lookupNamespaceURI nsr prefix of
      Nothing -> defNS
      Just ns -> ns


recordOfDoc :: Document -> Effect (Maybe Node)
recordOfDoc doc = do
  recCollection <- getElementsByTagName "record" doc
  recordMay <- item 0 recCollection
  pure $ map Ele.toNode recordMay

elemXmlns :: Element -> Effect (Maybe String)
elemXmlns elem = getAttribute "xmlns" elem

nodeXmlns :: Node -> Effect (Maybe String)
nodeXmlns node = case fromNode node of
  Nothing -> pure Nothing
  Just elem -> elemXmlns elem

type ParseEnv = {
  doc :: Document
, recNode :: Node
, xeval :: String -> RT.ResultType -> Effect XP.XPathResult
}

getDefaultParseEnv :: String -> Effect ParseEnv
getDefaultParseEnv xmlDocStr = do
  dp <- makeDOMParser
  recDoc <- parseXMLFromString xmlDocStr dp
  recNodeMay <- recordOfDoc recDoc
  recNode <- case recNodeMay of
    Nothing -> throwException $ error "Could not find <record> element!"
    Just nd -> pure nd
  nsRes <- getMetajeloResolver recNode recDoc
  defEval <- pure $ mkMetajeloXpathEval recDoc recNode (Just nsRes)
  pure $ {
      doc: recDoc
    , recNode: recNode
    , xeval : defEval
  }

mkMetajeloXpathEval :: Document -> Node -> Maybe NSResolver->
  String -> RT.ResultType  -> Effect XP.XPathResult
mkMetajeloXpathEval doc rnode nsResMay xpath resType =
  XP.evaluate xpath rnode nsResMay resType Nothing doc

readRecord :: ParseEnv -> Effect MetajeloRecord
readRecord env = do
  recId <- readIdentifier env
  recDate  <- pure undefined
  recModDate <- pure undefined
  recRelId <- pure undefined
  recProds <- pure undefined
  pure $ {
      identifier: recId
    , date: recDate
    , lastModified: recModDate
    , relatedIdentifier: recRelId
    , supplementaryProducts: recProds
  }

readIdentifier :: ParseEnv -> Effect Identifier
readIdentifier env = do
  idRes <- env.xeval "/x:record/x:identifier" RT.string_type
  recId <- XP.stringValue idRes
  idType <- pure undefined
  pure $ {id: recId, idType: idType}


undefined :: forall a. Warn (QuoteLabel "undefined in use") => a
undefined = unsafeCoerce unit

