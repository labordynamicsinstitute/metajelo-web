module Test.Main where

import Prelude

import Data.Maybe                        (Maybe(..), isJust)
-- import Data.Natural                      (intToNat)
-- import Debug.Trace                       (traceM)
import Effect                            (Effect)
import Effect.Aff                        (Aff)
import Effect.Class                      (liftEffect)
import Effect.Console                    (logShow)
-- import Foreign                           (isUndefined, isNull, unsafeToForeign)
import Test.Data                         as TD
import Test.Unit                         (suite, test)
import Test.Unit.Main                    (runTest)
import Test.Unit.Assert                  as Assert

import Web.DOM.Document                  (Document, toNode)
import Web.DOM.DOMParser                 (DOMParser, makeDOMParser, parseXMLFromString)
import Web.DOM.Document.XPath            as XP
import Web.DOM.Document.XPath.ResultType as RT
import Web.DOM.Node                      (Node)

import Metajelo.Types                    as MJ
import Metajelo.XPaths                   as MXP

parseMetajeloDoc :: DOMParser -> Effect Document
parseMetajeloDoc dp = parseXMLFromString TD.metajeloXml dp

parseRecXmlnsFakeXmlDoc :: DOMParser -> Effect Document
parseRecXmlnsFakeXmlDoc dp = parseXMLFromString TD.recXmlnsFakeXml dp

main :: Effect Unit
main = runTest do
  suite "Metajelo.XPaths" do
    test "getMetajeloResolver finds xmlns of record" do
      domParser <- liftEffect $ makeDOMParser

      metajeloDoc <- liftEffect $ parseRecXmlnsFakeXmlDoc domParser
      metajeloMay :: Maybe Node <- liftEffect $ MXP.recordOfDoc metajeloDoc
      Assert.assert "found record element" (isJust metajeloMay)
      metajelo :: Node <- pure $ case metajeloMay of
        Nothing -> toNode metajeloDoc
        Just nd -> nd

      mjNSresolver <- liftEffect $ MXP.getMetajeloResolver metajelo metajeloDoc

      retrievedNSMay <- pure $ XP.lookupNamespaceURI mjNSresolver "dummy"
      retrievedNS <- pure $ case retrievedNSMay of
        Nothing -> "Failure"
        Just ns -> ns

      Assert.assertFalse
        "defaultMetajeloNS should not equal fakeXmlns or test won't work"
        (MXP.defaultMetajeloNS == TD.fakeXmlns)
      Assert.equal TD.fakeXmlns retrievedNS

    test "Metajelo Parsing" do
      parseEnv <- liftEffect $ MXP.getDefaultParseEnv TD.metajeloXml
      record <- liftEffect $ MXP.readRecord parseEnv
      Assert.equal "OjlTjf" record.identifier.id
      Assert.equal MJ.EISSN record.identifier.idType

  suite "namespaced tests" do
    test "metajelo.xml" do
      domParser <- liftEffect $ makeDOMParser

      metajeloDoc <- liftEffect $ parseMetajeloDoc domParser
      metajelo <- pure $ toNode metajeloDoc

      mjNSresolver <- liftEffect $ MXP.getMetajeloResolver metajelo metajeloDoc

      metajeloIdRes <- liftEffect $ XP.evaluate
        "/foo:record/foo:identifier"
        metajelo
        (Just mjNSresolver)
        RT.string_type
        Nothing
        metajeloDoc
      metajeloId <- liftEffect $ XP.stringValue metajeloIdRes
      tlog $ "got metajelo id" <> metajeloId
      Assert.equal RT.string_type (XP.resultType metajeloIdRes)
      Assert.equal "OjlTjf" metajeloId

tlog :: forall a. Show a => a -> Aff Unit
tlog = liftEffect <<< logShow
