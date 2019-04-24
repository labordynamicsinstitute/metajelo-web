module Metajelo.XPaths where

import Prelude

import Data.Int                          (toNumber)
import Data.Maybe                        (Maybe(..), fromMaybe)
import Data.Natural                      (intToNat)
import Data.Traversable                  (sequence)
import Effect                            (Effect)
import Effect.Aff                        (Aff)
import Effect.Class                      (liftEffect)
import Effect.Console                    (logShow, warn)
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
, xeval :: MJXpathEvals
, xevalRoot :: MJXpathRootEvals
}

getDefaultParseEnv :: String -> Effect ParseEnv
getDefaultParseEnv xmlDocStr = do
  dp <- makeDOMParser
  recDoc <- parseXMLFromString xmlDocStr dp
  recNodeMay <- recordOfDoc recDoc
  recNode <- case recNodeMay of
    Nothing -> throwErr "Could not find <record> element!"
    Just nd -> pure nd
  nsRes <- getMetajeloResolver recNode recDoc
  defEvals <- pure $ mkMetajeloXpathEval recDoc (Just nsRes)
  pure $ {
      doc: recDoc
    , recNode: recNode
    , xeval : defEvals
    , xevalRoot : {
        any : defEvals.any recNode
      , num : defEvals.num recNode
      , str : defEvals.str recNode
      , bool : defEvals.bool recNode
    }
  }

type MJXpathEvals = {
    any  :: Node -> String -> RT.ResultType -> Effect XP.XPathResult
  , num  :: Node -> String -> Effect Number
  , str  :: Node -> String -> Effect String
  , bool :: Node -> String -> Effect Boolean
}
type MJXpathRootEvals = {
    any  :: String -> RT.ResultType -> Effect XP.XPathResult
  , num  :: String -> Effect Number
  , str  :: String -> Effect String
  , bool :: String -> Effect Boolean
}

mkMetajeloXpathEval :: Document -> Maybe NSResolver -> MJXpathEvals
mkMetajeloXpathEval doc nsResMay = {
    any : (\n x r -> XP.evaluate x n nsResMay r Nothing doc)
  , num : (\n x -> XP.evaluateNumber x n nsResMay Nothing doc)
  , str : (\n x -> XP.evaluateString x n nsResMay Nothing doc)
  , bool : (\n x -> XP.evaluateBoolean x n nsResMay Nothing doc)
}

readRecord :: ParseEnv -> Effect MetajeloRecord
readRecord env = do
  recId <- readIdentifier env
  recDate  <- readDate env
  recModDate <- readModDate env
  recRelIds <- readRelIdentifiers env
  recProds <- pure undefined
  pure $ {
      identifier: recId
    , date: recDate
    , lastModified: recModDate
    , relatedIdentifiers: recRelIds
    , supplementaryProducts: recProds
  }

readIdentifier :: ParseEnv -> Effect Identifier
readIdentifier env = do
  recId <- env.xevalRoot.str "/x:record/x:identifier"
  idTypeStr <- env.xevalRoot.str "/x:record/x:identifier/@identifierType"
  idType <- readIdentifierType $ idTypeStr
  pure {id: recId, idType: idType}

readIdentifierType :: String -> Effect IdentifierType
readIdentifierType "ARK" = pure ARK
readIdentifierType "ArXiv" = pure ArXiv
readIdentifierType "Bibcode" = pure Bibcode
readIdentifierType "DOI" = pure DOI
readIdentifierType "EAN13" = pure EAN13
readIdentifierType "EISSN" = pure EISSN
readIdentifierType "Handle" = pure Handle
readIdentifierType "IGSN" = pure IGSN
readIdentifierType "ISBN" = pure ISBN
readIdentifierType "ISSN" = pure ISSN
readIdentifierType "ISTC" = pure ISTC
readIdentifierType "LISSN" = pure LISSN
readIdentifierType "LSID" = pure LSID
readIdentifierType "PMID" = pure PMID
readIdentifierType "PURL" = pure PURL
readIdentifierType "UPC" = pure UPC
readIdentifierType "URL" = pure URL
readIdentifierType "URN" = pure URN
readIdentifierType unknown =
  throwErr $ "Unknown IdentifierType: '" <> unknown <> "'"

readDate :: ParseEnv -> Effect XsdDate
readDate env = env.xevalRoot.str "/x:record/x:date"

readModDate :: ParseEnv -> Effect XsdDate
readModDate env = env.xevalRoot.str "/x:record/x:lastModified"

readRelIdentifiers :: ParseEnv -> Effect (Array RelatedIdentifier)
readRelIdentifiers env = do
  idRes <- env.xevalRoot.any
    "/x:record/x:relatedIdentifier" RT.ordered_node_snapshot_type
  idNodes <- XP.snapshot idRes
  sequence $ map getRelIdentifier idNodes
  where
    getRelId :: Node -> Effect String
    getRelId nd = env.xeval.str nd "."
    getRelIdType :: Node -> Effect IdentifierType
    getRelIdType nd = do
      idTypeStr <- env.xeval.str nd "@relatedIdentifierType"
      readIdentifierType idTypeStr
    getRelRelType :: Node -> Effect RelationType
    getRelRelType nd = do
      idRelStr <- env.xeval.str nd "@relationType"
      readRelationType idRelStr
    getRelIdentifier :: Node -> Effect RelatedIdentifier
    getRelIdentifier nd = do
      recId <- getRelId nd
      idType <- getRelIdType nd
      relType <- getRelRelType nd
      pure {id: recId, idType: idType, relType: relType}

readRelationType :: String -> Effect RelationType
readRelationType "IsCitedBy" = pure IsCitedBy
readRelationType "Cites" = pure Cites
readRelationType "IsSupplementTo" = pure IsSupplementTo
readRelationType "IsSupplementedBy" = pure IsSupplementedBy
readRelationType "IsContinuedBy" = pure IsContinuedBy
readRelationType "Continues" = pure Continues
readRelationType "IsNewVersionOf" = pure IsNewVersionOf
readRelationType "IsPreviousVersionOf" = pure IsPreviousVersionOf
readRelationType "IsPartOf" = pure IsPartOf
readRelationType "HasPart" = pure HasPart
readRelationType "IsReferencedBy" = pure IsReferencedBy
readRelationType "References" = pure References
readRelationType "IsDocumentedBy" = pure IsDocumentedBy
readRelationType "Documents" = pure Documents
readRelationType "IsCompiledBy" = pure IsCompiledBy
readRelationType "Compiles" = pure Compiles
readRelationType "IsVariantFormOf" = pure IsVariantFormOf
readRelationType "IsOriginalFormOf" = pure IsOriginalFormOf
readRelationType "IsIdenticalTo" = pure IsIdenticalTo
readRelationType "HasMetadata" = pure HasMetadata
readRelationType "IsMetadataFor" = pure IsMetadataFor
readRelationType "Reviews" = pure Reviews
readRelationType "IsReviewedBy" = pure IsReviewedBy
readRelationType "IsDerivedFrom" = pure IsDerivedFrom
readRelationType "IsSourceOf" = pure IsSourceOf
readRelationType unknown =
  throwErr $ "Unknown RelationType: '" <> unknown <> "'"

throwErr :: forall a. String -> Effect a
throwErr = throwException <<< error

undefined :: forall a. Warn (QuoteLabel "undefined in use") => a
undefined = unsafeCoerce unit

