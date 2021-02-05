module Metajelo.View where

import Prelude                             (class Functor, class Monoid,
                                            class Semigroup,
                                            identity, join, map, mempty, show,
                                            (#), (<#>), ($), (<<<), (<>))


import Concur.Core                          (Widget)
import Concur.React                         (HTML)
import Concur.React.DOM                     ( ElLeafFunc', a, br', cite',
                                             div, div',
                                             li, li', li_,
                                             span, span', span_, text, ul
                                             )
import Concur.React.Props                   (ReactProps, href)
import Control.Alt                          ((<|>))
import Data.Maybe                           (Maybe(..), isNothing)
import Data.Array                           ((:), head, init)
import Data.Foldable                        (class Foldable, any,
                                             fold, foldMap, intercalate)
import Data.Natural                         (natToInt)
import Data.String                          as S
import Data.String.NonEmpty                 (NonEmptyString, toString)
import Data.String.Utils                    (endsWith, fromCharArray)
import Data.Unfoldable                      (fromMaybe)
import Data.Unfoldable1                     (class Unfoldable1, singleton)
import Foreign.Object                       as FO
import Metajelo.CSS.Web.ClassNames          as MCN
import Metajelo.CSS.Web.ClassProps          as MC
import Metajelo.CSS.Web.Util                (cList)
import Metajelo.Types
import Text.Email.Validate                  as EA
import Text.URL.Validate                   (urlToString)

-- Temp imports for group -- TODO: remove
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Data.Profunctor.Strong ((&&&))

spc :: forall a. Widget HTML a
spc = span' [text " "]

spacify :: forall a. Array (Widget HTML a) -> Array (Widget HTML a)
spacify  = intercalate [spc] <<< (map singleton)


comma :: forall a. Widget HTML a
comma = span' [text ", "]

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
mkRecordWidget rec = div [MC.record] [
    span [MC.relatedIdsHeader] []
  , relIdInfo
  , span [MC.productsHeader] [
      span_ [MC.recordId] $ idToWidg rec.identifier
    ]
  , ul [MC.productList] $ map
      (\k -> li_ [MC.productGroup] $ prodGrpWidg k)
      (FO.keys prodGroups)
  ]
  where
    recId = rec.identifier.identifier
    idTyp = show rec.identifier.identifierType
    prodGroups :: FO.Object (NonEmptyArray SupplementaryProduct)
    prodGroups = group
      (\p -> printResTyp p)
      rec.supplementaryProducts
    printResTyp prod = show prod.resourceType.generalType <> ": " <>
      prod.resourceType.description
    prodGrpWidg :: String -> forall a. Widget HTML a
    prodGrpWidg key = div' $ grpHeader : map mkSupplementaryProductWidget prodGrp
      where
        prodGrp :: Array SupplementaryProduct
        prodGrp = FO.lookup key prodGroups # map NA.toArray
          # fromMaybe # join
        grpHeader :: forall a. Widget HTML a
        grpHeader = span_ [MC.resourceType] $ (head prodGrp) <#> (\prod ->
              (span_ [MC.resourceTypeGen] $ text $ show prod.resourceType.generalType)
          <|> (span_ [MC.resourceTypeDescr] $ text prod.resourceType.description)
          <|> br'
          ) # fold
    relIdInfo = ul [MC.relatedIdList] $
      NA.toArray relIdItems
      where
        relIdItems = map relIdToWidg rec.relatedIdentifiers
          # map (\i -> li [MC.relatedId] [i] )

mkSupplementaryProductWidget :: SupplementaryProduct -> forall a. Widget HTML a
mkSupplementaryProductWidget prod = div [MC.product] $
  spacify $ [
    span [MC.productCitation]
      [cite' $ spacify $ citeElems]
  ] <> [formatsWidget prod.format] <> (locElems loc)
  where
    citeElems = basicMeta <> [span' [instNameElem loc, text "."], resIdElem]
    basicMeta = [
      creatorsWidget prod.basicMetadata.creators
    , span [MC.basicMetadata, MC.pubyear]
        [text $ show $ natToInt prod.basicMetadata.publicationYear]
    , span [MC.basicMetadata, MC.title]
        [   titlesWidget prod.basicMetadata.titles
          , text $ addEndPunct "" (isNothing prod.resourceID) ","
        ]
    ]
    resIdElem = case prod.resourceID of
      Just resID -> span [MC.resourceId] [idToWidg resID, (text ".")]
      Nothing -> mempty

    loc = prod.location

creatorsWidget :: NonEmptyArray NonEmptyString -> forall a. Widget HTML a
creatorsWidget ctors = span_ [MC.basicMetadata, MC.creatorList]
  $ intercalate comma
    $ (ctors # NA.toArray <#> (\c -> span_ [MC.creator] $ textNE c))

titlesWidget :: NonEmptyArray NonEmptyString -> forall a. Widget HTML a
titlesWidget titles = span_ [] $ intercalate comma
  $ (titles # NA.toArray <#> (\t -> span_ [MC.title] $ textNE t))

formatsWidget :: Array Format -> forall a. Widget HTML a
formatsWidget fmts = span_ [MC.formatList] $ intercalate comma
  $ (fmts <#> (\f -> span_ [MC.format] $ textNE f))

instNameElem :: Location -> forall a. Widget HTML a
instNameElem loc = span [MC.institutionName]
  [textNE $ loc.institutionName]

locElems :: Location -> forall a. Array (Widget HTML a)
locElems loc = spacify $ [
      instNameElem loc
    , span' [
      text "("
    , span [MC.institutionId]
        [idToWidg loc.institutionID]
    , text "; "
    , span [MC.institutionType]
        [text $ show loc.institutionType]
    , text $ addEndPunct ")" (isNothing loc.superOrganizationName) ","
    ]
    , case loc.superOrganizationName of
        Nothing -> mempty
        Just so -> span' [
          text "a member of "
        , span [MC.superOrg]
            [text $ addEndPunct (toString so) false "."]
        ]
  , contactWidg loc.institutionContact
  , span [MC.sustainability] [
      a [MC.missionStatement
        , href $ urlToString sust.missionStatementURL]
        [text "Mission Statement"]
    , text "; "
    , a [MC.fundingStatement
        , href $ urlToString sust.fundingStatementURL]
        [text "Funding Statement"]
    , text "."
    ]
  , ul [MC.institutionPolicies] $
      map (\ip -> li' [ipolicyWidg ip]) $ NA.toArray loc.institutionPolicies
  , versioningWidg loc.versioning
  ]
    where
    sust = loc.institutionSustainability
    versioningWidg :: Boolean -> forall a. Widget HTML a
    versioningWidg versioning = span [MC.versioning] [text vTxt]
      where
      vTxt = case versioning of
        true -> "Versioned"
        false -> "Unversioned"

contactWidg :: InstitutionContact -> forall a. Widget HTML a
contactWidg contact = span_ [MC.institutionContact] $
      (span' $ [text "Institution Contact: "])
  <|> (a [MC.contactEmail, href $ "mailto:" <> ea] [text ea])
  <|> (span_ [MC.contactType] contactType)
  where
    ea = EA.toString contact.emailAddress
    contactType = text case contact.contactType of
      Nothing -> "."
      Just ct -> " (" <> show ct <> ")."

relIdToWidg :: RelatedIdentifier -> forall a. Widget HTML a
relIdToWidg {identifier , identifierType, relationType} = span [MC.relatedId] [
    span_ [MC.relType] $ text $ (show relationType)
  , spc
  , idToWidg {identifier , identifierType}
]

idToWidg :: Identifier -> forall a. Widget HTML a
idToWidg fullId@{identifier , identifierType} = span [MC.identifier] [
  span_ [MC.idType] $ text $ (show identifierType)
, span_ [MC.idUrl] $ idUrl fullId
]

citeId :: NonEmptyString -> forall a. Widget HTML a
citeId idStr = cite' [textNE idStr]

-- | Returns a URL if one can be constructed from the identifier.
-- | Otherwise, just returns the identifier as text.
idUrl :: Identifier -> forall a. Widget HTML a
idUrl {identifier , identifierType: ARK} = a [hrefNE identifier] [citeId identifier]
idUrl {identifier , identifierType: ArXiv} = a [href url] [citeId identifier]
  where url = "https://arxiv.org/abs/" <> toString identifier
idUrl {identifier , identifierType: Bibcode} = a [href url] [citeId identifier]
  where url = "https://ui.adsabs.harvard.edu/abs/" <> toString identifier <> "/abstract"
idUrl {identifier , identifierType: DOI} = a [href url] [citeId identifier]
  where url = "https://doi.org/" <> toString identifier
idUrl {identifier , identifierType: EAN13} = citeId identifier
idUrl {identifier , identifierType: EISSN} = a [href url] [citeId identifier]
  where url = "https://www.worldcat.org/ISSN/" <> toString identifier
idUrl {identifier , identifierType: Handle} = a [href url] [citeId identifier]
  where url = "http://hdl.handle.net/" <> toString identifier
idUrl {identifier , identifierType: IGSN} = a [href url] [citeId identifier]
  where url = "http://igsn.org/" <> toString identifier
idUrl {identifier , identifierType: ISBN} = citeId identifier
idUrl {identifier , identifierType: ISSN} = a [href url] [citeId identifier]
  where url = "https://www.worldcat.org/ISSN/" <> toString identifier
idUrl {identifier , identifierType: ISTC} = citeId identifier
idUrl {identifier , identifierType: LISSN} = a [href url] [citeId identifier]
  where url = "https://www.worldcat.org/ISSN/" <> toString identifier
idUrl {identifier , identifierType: LSID} = a [href url] [citeId identifier]
  where url = "http://www.lsid.info/resolver/?lsid=" <> toString identifier
idUrl {identifier , identifierType: PMID} = a [href url] [citeId identifier]
  where url = "https://www.ncbi.nlm.nih.gov/pubmed/" <> toString identifier
idUrl {identifier , identifierType: PURL} = a [hrefNE identifier] [citeId identifier]
idUrl {identifier , identifierType: UPC} = citeId identifier
idUrl {identifier , identifierType: URL} = a [hrefNE identifier] [citeId identifier]
idUrl {identifier , identifierType: URN} = citeId identifier

ipolicyWidg :: InstitutionPolicy -> forall a. Widget HTML a
ipolicyWidg ipol = div [MC.institutionPolicy] $ spacify $ [
  appliesWidg ipol.appliesToProduct
, foldMap policyTypeWidg ipol.policyType
, policyWidg ipol.policy
]
  where
  policyTypeWidg :: PolicyType -> forall a. Widget HTML a
  policyTypeWidg polType = span' [
    span [MC.policyType] [text $ show polType]
  , text $ " Policy:"
  ]
  policyWidg :: Policy -> forall a. Widget HTML a
  policyWidg pol = span [MC.policy] $ singleton
    case pol of
      FreeTextPolicy txt -> textNE txt
      RefPolicy url -> let urlStr = urlToString url in
        a [href $ urlStr] [text urlStr]
  appliesWidg :: Maybe Boolean -> forall a. Widget HTML a
  appliesWidg appliesMay = span [cList [MCN.applies, sq.cls]] [info sq.text]
    where
    sq = case appliesMay of
      Nothing -> {text: "May apply to product (unverified)", cls: MCN.appliesMaybe}
      Just true -> {text: "Applies to product", cls: MCN.appliesYes}
      Just false ->{text: "Does not apply to product", cls: MCN.appliesNo}
    info txt = span [MC.appliesInfo] [text txt]

--TODO: use upstream when merged
group :: forall a f u. Foldable f => Functor f => Unfoldable1 u => Semigroup (u a) =>
  (a -> String) -> f a -> FO.Object (u a)
group toStr = FO.fromFoldableWith (<>) <<< map (toStr &&& singleton)

hrefNE :: forall a. NonEmptyString -> ReactProps a
hrefNE = href <<< toString

textNE ::  ElLeafFunc' NonEmptyString
textNE = text <<< toString
