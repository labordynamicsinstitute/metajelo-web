module Metajelo.View where

import Prelude

import Concur.Core                          (Widget)
import Concur.Core.Types                    (andd)
import Concur.Core.FRP                      (Signal, display, dyn, hold)
import Concur.React                         (HTML)
import Concur.React.DOM                     (El', a, br', button, button', cite',
                                             div, div_, div', em',
                                             input, li, nav, p',
                                             span, span', text, ul
                                             )
import Concur.React.Props                   (_type, checked, classList,
                                             href, className, onChange,
                                             onClick, onFocus,
                                             unsafeTargetValue, value)
import Control.Alt                          ((<|>))
import Control.Monad.Rec.Class              (forever)
import Control.MultiAlternative             (orr)
import Data.Maybe                           (Maybe(..), isNothing)
import Data.Traversable                     (traverse)
import Data.Array                           ((..), (!!), length, init, zip)
import Data.Foldable                        (any, fold, foldMap, intercalate)
import Data.Function                        ((#))
import Data.Semigroup                       (class Semigroup)
import Data.String                          as S
import Data.String.Utils                    (endsWith, fromCharArray)
import Data.Tuple                           (Tuple, fst, snd)
import Data.Unfoldable                      (fromMaybe)
import Data.Unfoldable1                     (class Unfoldable1, singleton)
import Effect.Exception                     as EX
import Foreign.Object                       as FO
import Metajelo.Types
import React.SyntheticEvent                 (SyntheticMouseEvent)
import Text.Email.Validate                  as EA
import URL.Validator                        (urlToString)

-- Temp imports for group -- TODO: remove
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Data.Foldable (class Foldable)
import Data.Profunctor.Strong ((&&&))

mjCssPfx :: String -> String
mjCssPfx cname = "metajelo_" <> cname

spc :: forall a. Widget HTML a
spc = span' [text " "]

spacify :: forall a. Array (Widget HTML a) -> Array (Widget HTML a)
spacify  = intercalate [spc] <<< (map singleton)

toCharArray :: String -> Array String
toCharArray = map S.singleton <<< S.toCodePointArray

initMonoid :: forall m. Monoid m => Array m -> Array m
initMonoid elems = foldMap identity (init elems)

-- | Adds punctuation at the end of a text fragement; useful
-- | for dealing with optional values (set skip = false if not
-- | an issue) and cases where epunctuation may not be known
-- | at compile time. For instance, frequently we may want to
-- | set skip to `isNothing val` where val is of type `Maybe a`.
addEndPunct :: String -> Boolean -> String -> String
addEndPunct input skip punct =
  if skip then input
  else if any (\p -> endsWith p input) pctsToReplace then
    let inChars = toCharArray input in
      (fromCharArray $ initMonoid inChars) <> punct
  else input <> punct
  where
    pctsToReplace = [";", ".", ","]


mkRecordWidget :: MetajeloRecord -> forall a. Widget HTML a
mkRecordWidget rec = div [className $ mjCssPfx "record"] [
  span [className $ mjCssPfx "productsHeader"] [
    text $ "Supplementary materials for "
    , idToWidg rec.identifier
  ]
  , ul [className $ mjCssPfx "productGroupList"] $ map
      (\k -> li [className $ mjCssPfx "productGroup"]
        [div' $ [text(k), br', prodGrpWidg k]])
      (FO.keys prodGroups)
  , span [className $ mjCssPfx "relatedIdentifiersHeader"]
      [text $ "Related Identifiers"]
  , relIdInfo
  ]
  where
    recId = rec.identifier.id
    idTyp = show rec.identifier.idType
    prodGroups :: FO.Object (NonEmptyArray SupplementaryProduct)
    prodGroups = group
      (\p -> printResTyp p)
      rec.supplementaryProducts
    printResTyp prod = show prod.resourceType.generalType <> ": " <>
      prod.resourceType.description
    prodGrpWidg :: String -> forall a. Widget HTML a
    prodGrpWidg key = div' $ map mkSupplementaryProductWidget grpWidgs
      where
        grpWidgs :: Array SupplementaryProduct
        grpWidgs = FO.lookup key prodGroups # map NA.toArray
          # fromMaybe # join
    relIdInfo = ul [className $ mjCssPfx "relatedIdList"] $
      NA.toArray relIdItems
      where
        relIdItems = map relIdToWidg rec.relatedIdentifiers
          # map (\i -> li [className $ mjCssPfx "relatedIdItem"] [i] )

mkSupplementaryProductWidget :: SupplementaryProduct -> forall a. Widget HTML a
mkSupplementaryProductWidget prod = div [className $ mjCssPfx "product"] $
  spacify $ [
    span [className $ mjCssPfx "productCitation"]
      [cite' $ spacify $ citeElems]
  ] <> locElems
  where
    citeElems = basicMeta <> [span' [instNameElem, text "."], resIdElem]
    basicMeta = [
      span [className $ mjCssPfx "creator"]
        [text $ prod.basicMetadata.creator]
    , span [className $ mjCssPfx "pubyear"]
        [text $ prod.basicMetadata.publicationYear]
    , span [className $ mjCssPfx "title"]
        [text $ addEndPunct
          prod.basicMetadata.title
          (isNothing prod.resourceID)
          ","
        ]
    ]
    resIdElem = case prod.resourceID of
      Just resID -> span' [idToWidg resID, (text ".")]
      Nothing -> mempty

    loc = prod.location
    sust = loc.institutionSustainability
    instNameElem = span [className $ mjCssPfx "institutionName"]
      [text $ loc.institutionName]
    locElems = spacify $ [
        instNameElem
      , span' [
        text "("
      , span [className $ mjCssPfx "institutionId"]
          [idToWidg loc.institutionID]
      , text "; "
      , span [className $ mjCssPfx "institutionType"]
          [text $ show loc.institutionType]
      , text $ addEndPunct ")" (isNothing loc.superOrganizationName) ","
      ]
      , case loc.superOrganizationName of
          Nothing -> mempty
          Just so -> span' [
            text "a member of "
          , span [className $ mjCssPfx "superOrg"]
              [text $ addEndPunct so false "."]
          ]
    , contactWidg loc.institutionContact
    , span' [
        a [className $ mjCssPfx "missionStatement"
          , href $ urlToString sust.missionStatementURL]
          [text "Mission Statement"]
      , text "; "
      , a [className $ mjCssPfx "fundingStatement"
          , href $ urlToString sust.fundingStatementURL]
          [text "Funding Statement"]
      , text "."
      ]
    ]

contactWidg :: InstitutionContact -> forall a. Widget HTML a
contactWidg contact = span' $  [
  text "Institution Contact: "
, a [className $ mjCssPfx "institutionContact", href $ "mailto:" <> ea] [text ea]
] <> [contactType]
  where
    ea = EA.toString contact.emailAddress
    contactType = text case contact.contactType of
      Nothing -> "."
      Just ct -> " (" <> show ct <> ")."

relIdToWidg :: RelatedIdentifier -> forall a. Widget HTML a
relIdToWidg {id, idType, relType} = span [className $ mjCssPfx "relatedId"] [
  text $ (show relType)
  , spc
  , idToWidg {id, idType}
]

idToWidg :: Identifier -> forall a. Widget HTML a
idToWidg fullId@{id, idType} = span [className $ mjCssPfx "identifier"] [
  text $ (show idType) <> ": "
, idUrl fullId
]

citeId :: String -> forall a. Widget HTML a
citeId idStr = cite' [text idStr]

-- | Returns a URL if one can be constructed from the identifier.
-- | Otherwise, just returns the identifier as text.
idUrl :: Identifier -> forall a. Widget HTML a
idUrl {id, idType: ARK} = a [href id] [citeId id]
idUrl {id, idType: ArXiv} = a [href url] [citeId id]
  where url = "https://arxiv.org/abs/" <> id
idUrl {id, idType: Bibcode} = a [href url] [citeId id]
  where url = "https://ui.adsabs.harvard.edu/abs/" <> id <> "/abstract"
idUrl {id, idType: DOI} = a [href url] [citeId id]
  where url = "https://doi.org/" <> id
idUrl {id, idType: EAN13} = citeId id
idUrl {id, idType: EISSN} = a [href url] [citeId id]
  where url = "https://www.worldcat.org/ISSN/" <> id
idUrl {id, idType: Handle} = a [href url] [citeId id]
  where url = "http://hdl.handle.net/" <> id
idUrl {id, idType: IGSN} = a [href url] [citeId id]
  where url = "http://igsn.org/" <> id
idUrl {id, idType: ISBN} = citeId id
idUrl {id, idType: ISSN} = a [href url] [citeId id]
  where url = "https://www.worldcat.org/ISSN/" <> id
idUrl {id, idType: ISTC} = citeId id
idUrl {id, idType: LISSN} = a [href url] [citeId id]
  where url = "https://www.worldcat.org/ISSN/" <> id
idUrl {id, idType: LSID} = a [href url] [citeId id]
  where url = "http://www.lsid.info/resolver/?lsid=" <> id
idUrl {id, idType: PMID} = a [href url] [citeId id]
  where url = "https://www.ncbi.nlm.nih.gov/pubmed/" <> id
idUrl {id, idType: PURL} = a [href id] [citeId id]
idUrl {id, idType: UPC} = citeId id
idUrl {id, idType: URL} = a [href id] [citeId id]
idUrl {id, idType: URN} = citeId id

policyWidg :: InstitutionPolicy -> forall a. Widget HTML a
policyWidg ipol = div [className $ mjCssPfx "institutionPolicy"] [
  foldMap showPolicyType ipol.policyType
]
  where
    showPolicyType :: PolicyType -> forall a. Widget HTML a
    showPolicyType polType = span' [
      span [className $ mjCssPfx "policyType"] [text $ show polType]
    , text $ " Policy"
    ]

-- mkProdArrayWidg :: Array SupplementaryProduct ->  forall a. Widget HTML a
-- mkProdArrayWidg ra = div' $ map mkRecordWidget ra

--TODO: use upstream when merged
group :: forall a f u. Foldable f => Functor f => Unfoldable1 u => Semigroup (u a) =>
  (a -> String) -> f a -> FO.Object (u a)
group toStr = FO.fromFoldableWith (<>) <<< map (toStr &&& singleton)
