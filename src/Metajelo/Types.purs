-- | This module is derived from the schema of metajelo,
-- | and more directly from the output of XsdToHaskell
-- | being run on the schema.
module Metajelo.Types where

import Prelude

import Data.Array.NonEmpty
import Data.Maybe                           (Maybe(..))

import Data.Generic.Rep                     (class Generic)
import Data.Generic.Rep.Eq                  (genericEq)
import Data.Generic.Rep.Show                (genericShow)

-- | Stand in for xs:date
type XsdDate = String

type Format = String

type URI = String -- TODO: use URI package?

type EmailAddress = String

-- | metadata about the publication and links to unlimited
-- | number of suppementary products
type MetajeloRecord = {
  identifier :: Identifier
, date :: XsdDate
  -- ^ The date of the original creation of this metadata record
, lastModified :: XsdDate
  -- ^ The date of the most recent modification of this recocrd
, relatedIdentifier :: Array Identifier
, supplementaryProducts :: Array SupplementaryProduct
  -- ^ The link to the set of supplemenary products
}
--derive instance eqRecord :: Eq MetajeloRecord

type Identifier = {
  id :: String
, idType :: IdentifierType
}

-- | The type of the Identifier and RelatedIdentifier.
data IdentifierType
  = ARK
  | ArXiv
  | Bibcode
  | DOI
  | EAN13
  | EISSN
  | Handle
  | IGSN
  | ISBN
  | ISSN
  | ISTC
  | LISSN
  | LSID
  | PMID
  | PURL
  | UPC
  | URL
  | URN
--derive instance eqIdentifierType :: Eq IdentifierType
derive instance genericIdentifierType :: Generic IdentifierType _
instance showIdentifierType :: Show IdentifierType where
  show = genericShow
instance eqIdentifierType :: Eq IdentifierType where
  eq = genericEq

type SupplementaryProduct = {
  basicMetadata :: BasicMetadata
, resourceID :: Maybe ResourceID
, resourceType :: ResourceType
, format :: Maybe Format
, resourceMetadataSource :: Maybe ResourceMetadataSource
, location :: LocationType
}
--derive instance eqSupplementaryProduct :: Eq SupplementaryProduct

type BasicMetadata = {
  title :: String
, creator :: String
, publicationYear :: XsdDate
}

type ResourceID = {
  id :: String
, idType :: IdentifierType
}

-- | The general type of a resource.
data ResourceType =
  Audiovisual
  | Dataset
  | Event
  | Image
  | InteractiveResource
  | Model
  | PhysicalObject
  | ResourceCollection
  | Service
  | Software
  | Sound
  | Text
  | Workflow
  | Other
--derive instance EqResourceType :: Eq ResourceType

type ResourceMetadataSource = {
  uri :: URI
, relationType :: RelationType
}

-- | Description of the relationship of the resource being
--   registered (A) and the related resource (B).
data RelationType =
  IsCitedBy
  | Cites
  | IsSupplementTo
  | IsSupplementedBy
  | IsContinuedBy
  | Continues
  | IsNewVersionOf
  | IsPreviousVersionOf
  | IsPartOf
  | HasPart
  | IsReferencedBy
  | References
  | IsDocumentedBy
  | Documents
  | IsCompiledBy
  | Compiles
  | IsVariantFormOf
  | IsOriginalFormOf
  | IsIdenticalTo
  | HasMetadata
  | IsMetadataFor
  | Reviews
  | IsReviewedBy
  | IsDerivedFrom
  | IsSourceOf

type LocationType = {
  institutionID :: IdentifierType
, institutionName :: String
, institutionType :: InstitutionType
, superOrganizationName :: Maybe String
, institutionContact :: InstitutionContact
, institutionSustainability :: InstitutionSustainability
, institutionPolicies :: NonEmptyArray InstitutionPolicy
  -- ^ set of possible policies for this location
, versioning :: Boolean
}

data InstitutionType =
    Commercial
  | NonProfit
  | Governmental

type InstitutionContact = {
  emailAddress :: EmailAddress
, contactType :: InstitutionContactType
}

ictShow :: InstitutionContactType -> String
ictShow DataCustodian = "DataCustodian"

data InstitutionContactType = DataCustodian
instance showInstitutionContactType :: Show InstitutionContactType where
  show = ictShow

type InstitutionSustainability = {
  missionStatementURL :: URI
, fundingStatementURL :: URI
}

data PolicyType
    = Access
    | Collection
    | Data
    | Metadata
    | Preservation
    | Submission
    | Quality
    | TermsOfUse

type InstitutionPolicy = {
  policy :: String
, policyType :: PolicyType
}
