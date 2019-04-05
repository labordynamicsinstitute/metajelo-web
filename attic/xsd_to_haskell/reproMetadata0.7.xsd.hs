{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module V'
  ( module V'
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
-- | This is the root element. Defined as metadata describing 
--   the linkage of a pulication to supplementary products 
--   (data, software, etc.)
elementRecord :: XMLParser RecordType
elementRecord = parseSchemaType "record"
elementToXMLRecord :: RecordType -> [Content ()]
elementToXMLRecord = schemaTypeToXML "record"
 
-- | metadata about the publication and links to unlimited 
--   number of suppementary products
data RecordType = RecordType
        { recordType_identifier :: Identifier
        , recordType_date :: Xsd.Date
          -- ^ The date of the original creation of this metadata record
        , recordType_lastModified :: Xsd.Date
          -- ^ The date of the most recent modification of this recocrd
        , recordType_relatedIdentifier :: [RelatedIdentifier]
        , recordType_supplementaryProducts :: SupplementaryProducts
          -- ^ The link to the set of supplemenary products
        }
        deriving (Eq,Show)
instance SchemaType RecordType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RecordType
            `apply` parseSchemaType "identifier"
            `apply` parseSchemaType "date"
            `apply` parseSchemaType "lastModified"
            `apply` many1 (parseSchemaType "relatedIdentifier")
            `apply` parseSchemaType "supplementaryProducts"
    schemaTypeToXML s x@RecordType{} =
        toXMLElement s []
            [ schemaTypeToXML "identifier" $ recordType_identifier x
            , schemaTypeToXML "date" $ recordType_date x
            , schemaTypeToXML "lastModified" $ recordType_lastModified x
            , concatMap (schemaTypeToXML "relatedIdentifier") $ recordType_relatedIdentifier x
            , schemaTypeToXML "supplementaryProducts" $ recordType_supplementaryProducts x
            ]
 
data InstitutionTypes
    = InstitutionTypes_Commercial
    | InstitutionTypes_Non'profit
    | InstitutionTypes_Governmental
    deriving (Eq,Show,Enum)
instance SchemaType InstitutionTypes where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InstitutionTypes where
    acceptingParser =  do literal "commercial"; return InstitutionTypes_Commercial
                      `onFail` do literal "non-profit"; return InstitutionTypes_Non'profit
                      `onFail` do literal "governmental"; return InstitutionTypes_Governmental
                      
    simpleTypeText InstitutionTypes_Commercial = "commercial"
    simpleTypeText InstitutionTypes_Non'profit = "non-profit"
    simpleTypeText InstitutionTypes_Governmental = "governmental"
 
data Yesno
    = Yesno_Yes
    | Yesno_No
    deriving (Eq,Show,Enum)
instance SchemaType Yesno where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Yesno where
    acceptingParser =  do literal "yes"; return Yesno_Yes
                      `onFail` do literal "no"; return Yesno_No
                      
    simpleTypeText Yesno_Yes = "yes"
    simpleTypeText Yesno_No = "no"
 
-- | The type of the RelatedIdentifier.
data IdentifierType
    = IdentifierType_ARK
    | IdentifierType_ArXiv
    | IdentifierType_Bibcode
    | IdentifierType_DOI
    | IdentifierType_EAN13
    | IdentifierType_EISSN
    | IdentifierType_Handle
    | IdentifierType_IGSN
    | IdentifierType_ISBN
    | IdentifierType_ISSN
    | IdentifierType_ISTC
    | IdentifierType_LISSN
    | IdentifierType_LSID
    | IdentifierType_PMID
    | IdentifierType_PURL
    | IdentifierType_UPC
    | IdentifierType_URL
    | IdentifierType_URN
    deriving (Eq,Show,Enum)
instance SchemaType IdentifierType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType IdentifierType where
    acceptingParser =  do literal "ARK"; return IdentifierType_ARK
                      `onFail` do literal "arXiv"; return IdentifierType_ArXiv
                      `onFail` do literal "bibcode"; return IdentifierType_Bibcode
                      `onFail` do literal "DOI"; return IdentifierType_DOI
                      `onFail` do literal "EAN13"; return IdentifierType_EAN13
                      `onFail` do literal "EISSN"; return IdentifierType_EISSN
                      `onFail` do literal "Handle"; return IdentifierType_Handle
                      `onFail` do literal "IGSN"; return IdentifierType_IGSN
                      `onFail` do literal "ISBN"; return IdentifierType_ISBN
                      `onFail` do literal "ISSN"; return IdentifierType_ISSN
                      `onFail` do literal "ISTC"; return IdentifierType_ISTC
                      `onFail` do literal "LISSN"; return IdentifierType_LISSN
                      `onFail` do literal "LSID"; return IdentifierType_LSID
                      `onFail` do literal "PMID"; return IdentifierType_PMID
                      `onFail` do literal "PURL"; return IdentifierType_PURL
                      `onFail` do literal "UPC"; return IdentifierType_UPC
                      `onFail` do literal "URL"; return IdentifierType_URL
                      `onFail` do literal "URN"; return IdentifierType_URN
                      
    simpleTypeText IdentifierType_ARK = "ARK"
    simpleTypeText IdentifierType_ArXiv = "arXiv"
    simpleTypeText IdentifierType_Bibcode = "bibcode"
    simpleTypeText IdentifierType_DOI = "DOI"
    simpleTypeText IdentifierType_EAN13 = "EAN13"
    simpleTypeText IdentifierType_EISSN = "EISSN"
    simpleTypeText IdentifierType_Handle = "Handle"
    simpleTypeText IdentifierType_IGSN = "IGSN"
    simpleTypeText IdentifierType_ISBN = "ISBN"
    simpleTypeText IdentifierType_ISSN = "ISSN"
    simpleTypeText IdentifierType_ISTC = "ISTC"
    simpleTypeText IdentifierType_LISSN = "LISSN"
    simpleTypeText IdentifierType_LSID = "LSID"
    simpleTypeText IdentifierType_PMID = "PMID"
    simpleTypeText IdentifierType_PURL = "PURL"
    simpleTypeText IdentifierType_UPC = "UPC"
    simpleTypeText IdentifierType_URL = "URL"
    simpleTypeText IdentifierType_URN = "URN"
 
-- | Description of the relationship of the resource being 
--   registered (A) and the related resource (B).
data RelationType
    = RelationType_IsCitedBy
    | RelationType_Cites
    | RelationType_IsSupplementTo
    | RelationType_IsSupplementedBy
    | RelationType_IsContinuedBy
    | RelationType_Continues
    | RelationType_IsNewVersionOf
    | RelationType_IsPreviousVersionOf
    | RelationType_IsPartOf
    | RelationType_HasPart
    | RelationType_IsReferencedBy
    | RelationType_References
    | RelationType_IsDocumentedBy
    | RelationType_Documents
    | RelationType_IsCompiledBy
    | RelationType_Compiles
    | RelationType_IsVariantFormOf
    | RelationType_IsOriginalFormOf
    | RelationType_IsIdenticalTo
    | RelationType_HasMetadata
    | RelationType_IsMetadataFor
    | RelationType_Reviews
    | RelationType_IsReviewedBy
    | RelationType_IsDerivedFrom
    | RelationType_IsSourceOf
    deriving (Eq,Show,Enum)
instance SchemaType RelationType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RelationType where
    acceptingParser =  do literal "IsCitedBy"; return RelationType_IsCitedBy
                      `onFail` do literal "Cites"; return RelationType_Cites
                      `onFail` do literal "IsSupplementTo"; return RelationType_IsSupplementTo
                      `onFail` do literal "IsSupplementedBy"; return RelationType_IsSupplementedBy
                      `onFail` do literal "IsContinuedBy"; return RelationType_IsContinuedBy
                      `onFail` do literal "Continues"; return RelationType_Continues
                      `onFail` do literal "IsNewVersionOf"; return RelationType_IsNewVersionOf
                      `onFail` do literal "IsPreviousVersionOf"; return RelationType_IsPreviousVersionOf
                      `onFail` do literal "IsPartOf"; return RelationType_IsPartOf
                      `onFail` do literal "HasPart"; return RelationType_HasPart
                      `onFail` do literal "IsReferencedBy"; return RelationType_IsReferencedBy
                      `onFail` do literal "References"; return RelationType_References
                      `onFail` do literal "IsDocumentedBy"; return RelationType_IsDocumentedBy
                      `onFail` do literal "Documents"; return RelationType_Documents
                      `onFail` do literal "IsCompiledBy"; return RelationType_IsCompiledBy
                      `onFail` do literal "Compiles"; return RelationType_Compiles
                      `onFail` do literal "IsVariantFormOf"; return RelationType_IsVariantFormOf
                      `onFail` do literal "IsOriginalFormOf"; return RelationType_IsOriginalFormOf
                      `onFail` do literal "IsIdenticalTo"; return RelationType_IsIdenticalTo
                      `onFail` do literal "HasMetadata"; return RelationType_HasMetadata
                      `onFail` do literal "IsMetadataFor"; return RelationType_IsMetadataFor
                      `onFail` do literal "Reviews"; return RelationType_Reviews
                      `onFail` do literal "IsReviewedBy"; return RelationType_IsReviewedBy
                      `onFail` do literal "IsDerivedFrom"; return RelationType_IsDerivedFrom
                      `onFail` do literal "IsSourceOf"; return RelationType_IsSourceOf
                      
    simpleTypeText RelationType_IsCitedBy = "IsCitedBy"
    simpleTypeText RelationType_Cites = "Cites"
    simpleTypeText RelationType_IsSupplementTo = "IsSupplementTo"
    simpleTypeText RelationType_IsSupplementedBy = "IsSupplementedBy"
    simpleTypeText RelationType_IsContinuedBy = "IsContinuedBy"
    simpleTypeText RelationType_Continues = "Continues"
    simpleTypeText RelationType_IsNewVersionOf = "IsNewVersionOf"
    simpleTypeText RelationType_IsPreviousVersionOf = "IsPreviousVersionOf"
    simpleTypeText RelationType_IsPartOf = "IsPartOf"
    simpleTypeText RelationType_HasPart = "HasPart"
    simpleTypeText RelationType_IsReferencedBy = "IsReferencedBy"
    simpleTypeText RelationType_References = "References"
    simpleTypeText RelationType_IsDocumentedBy = "IsDocumentedBy"
    simpleTypeText RelationType_Documents = "Documents"
    simpleTypeText RelationType_IsCompiledBy = "IsCompiledBy"
    simpleTypeText RelationType_Compiles = "Compiles"
    simpleTypeText RelationType_IsVariantFormOf = "IsVariantFormOf"
    simpleTypeText RelationType_IsOriginalFormOf = "IsOriginalFormOf"
    simpleTypeText RelationType_IsIdenticalTo = "IsIdenticalTo"
    simpleTypeText RelationType_HasMetadata = "HasMetadata"
    simpleTypeText RelationType_IsMetadataFor = "IsMetadataFor"
    simpleTypeText RelationType_Reviews = "Reviews"
    simpleTypeText RelationType_IsReviewedBy = "IsReviewedBy"
    simpleTypeText RelationType_IsDerivedFrom = "IsDerivedFrom"
    simpleTypeText RelationType_IsSourceOf = "IsSourceOf"
 
-- | The general type of a resource.
data ResourceType
    = ResourceType_Audiovisual
    | ResourceType_Collection
    | ResourceType_Dataset
    | ResourceType_Event
    | ResourceType_Image
    | ResourceType_InteractiveResource
    | ResourceType_Model
    | ResourceType_PhysicalObject
    | ResourceType_Service
    | ResourceType_Software
    | ResourceType_Sound
    | ResourceType_Text
    | ResourceType_Workflow
    | ResourceType_Other
    deriving (Eq,Show,Enum)
instance SchemaType ResourceType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ResourceType where
    acceptingParser =  do literal "Audiovisual"; return ResourceType_Audiovisual
                      `onFail` do literal "Collection"; return ResourceType_Collection
                      `onFail` do literal "Dataset"; return ResourceType_Dataset
                      `onFail` do literal "Event"; return ResourceType_Event
                      `onFail` do literal "Image"; return ResourceType_Image
                      `onFail` do literal "InteractiveResource"; return ResourceType_InteractiveResource
                      `onFail` do literal "Model"; return ResourceType_Model
                      `onFail` do literal "PhysicalObject"; return ResourceType_PhysicalObject
                      `onFail` do literal "Service"; return ResourceType_Service
                      `onFail` do literal "Software"; return ResourceType_Software
                      `onFail` do literal "Sound"; return ResourceType_Sound
                      `onFail` do literal "Text"; return ResourceType_Text
                      `onFail` do literal "Workflow"; return ResourceType_Workflow
                      `onFail` do literal "Other"; return ResourceType_Other
                      
    simpleTypeText ResourceType_Audiovisual = "Audiovisual"
    simpleTypeText ResourceType_Collection = "Collection"
    simpleTypeText ResourceType_Dataset = "Dataset"
    simpleTypeText ResourceType_Event = "Event"
    simpleTypeText ResourceType_Image = "Image"
    simpleTypeText ResourceType_InteractiveResource = "InteractiveResource"
    simpleTypeText ResourceType_Model = "Model"
    simpleTypeText ResourceType_PhysicalObject = "PhysicalObject"
    simpleTypeText ResourceType_Service = "Service"
    simpleTypeText ResourceType_Software = "Software"
    simpleTypeText ResourceType_Sound = "Sound"
    simpleTypeText ResourceType_Text = "Text"
    simpleTypeText ResourceType_Workflow = "Workflow"
    simpleTypeText ResourceType_Other = "Other"
 
newtype DoiType = DoiType Xs.Token deriving (Eq,Show)
instance Restricts DoiType Xs.Token where
    restricts (DoiType x) = x
instance SchemaType DoiType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (DoiType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DoiType where
    acceptingParser = fmap DoiType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern 10\..+/.+)
    simpleTypeText (DoiType x) = simpleTypeText x
 
data MyInstitutionType
    = MyInstitutionType_Governmental
    deriving (Eq,Show,Enum)
instance SchemaType MyInstitutionType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MyInstitutionType where
    acceptingParser =  do literal "governmental"; return MyInstitutionType_Governmental
                      
    simpleTypeText MyInstitutionType_Governmental = "governmental"
 
newtype EmailAddress = EmailAddress Xsd.XsdString deriving (Eq,Show)
instance Restricts EmailAddress Xsd.XsdString where
    restricts (EmailAddress x) = x
instance SchemaType EmailAddress where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EmailAddress x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EmailAddress where
    acceptingParser = fmap EmailAddress acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern [^@]+@[^\.]+\..+)
    simpleTypeText (EmailAddress x) = simpleTypeText x
 
data InstitutionContactTypes
    = InstitutionContactTypes_DataCustodian
    deriving (Eq,Show,Enum)
instance SchemaType InstitutionContactTypes where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InstitutionContactTypes where
    acceptingParser =  do literal "dataCustodian"; return InstitutionContactTypes_DataCustodian
                      
    simpleTypeText InstitutionContactTypes_DataCustodian = "dataCustodian"
 
data PolicyTypes
    = PolicyTypes_Access
    | PolicyTypes_Collection
    | PolicyTypes_Data
    | PolicyTypes_Metadata
    | PolicyTypes_Preservation
    | PolicyTypes_Submission
    | PolicyTypes_Quality
    | PolicyTypes_Terms_of_Use
    deriving (Eq,Show,Enum)
instance SchemaType PolicyTypes where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PolicyTypes where
    acceptingParser =  do literal "Access"; return PolicyTypes_Access
                      `onFail` do literal "Collection"; return PolicyTypes_Collection
                      `onFail` do literal "Data"; return PolicyTypes_Data
                      `onFail` do literal "Metadata"; return PolicyTypes_Metadata
                      `onFail` do literal "Preservation"; return PolicyTypes_Preservation
                      `onFail` do literal "Submission"; return PolicyTypes_Submission
                      `onFail` do literal "Quality"; return PolicyTypes_Quality
                      `onFail` do literal "Terms of Use"; return PolicyTypes_Terms_of_Use
                      
    simpleTypeText PolicyTypes_Access = "Access"
    simpleTypeText PolicyTypes_Collection = "Collection"
    simpleTypeText PolicyTypes_Data = "Data"
    simpleTypeText PolicyTypes_Metadata = "Metadata"
    simpleTypeText PolicyTypes_Preservation = "Preservation"
    simpleTypeText PolicyTypes_Submission = "Submission"
    simpleTypeText PolicyTypes_Quality = "Quality"
    simpleTypeText PolicyTypes_Terms_of_Use = "Terms of Use"
 
data LocationType = LocationType
        { locationType_institutionID :: IdentifierType
        , locationType_institutionName :: Xsd.XsdString
        , locationType_institutionType :: InstitutionTypes
        , locationType_superOrganizationName :: Maybe Xsd.XsdString
        , locationType_institutionContact :: InstitutionContact
        , locationType_institutionSustainability :: InstitutionSustainability
        , locationType_institutionPolicies :: InstitutionPolicies
          -- ^ set of possible policies for this location
        , locationType_versioning :: Yesno
        }
        deriving (Eq,Show)
instance SchemaType LocationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return LocationType
            `apply` parseSchemaType "institutionID"
            `apply` parseSchemaType "institutionName"
            `apply` parseSchemaType "institutionType"
            `apply` optional (parseSchemaType "superOrganizationName")
            `apply` parseSchemaType "institutionContact"
            `apply` parseSchemaType "institutionSustainability"
            `apply` parseSchemaType "institutionPolicies"
            `apply` parseSchemaType "versioning"
    schemaTypeToXML s x@LocationType{} =
        toXMLElement s []
            [ schemaTypeToXML "institutionID" $ locationType_institutionID x
            , schemaTypeToXML "institutionName" $ locationType_institutionName x
            , schemaTypeToXML "institutionType" $ locationType_institutionType x
            , maybe [] (schemaTypeToXML "superOrganizationName") $ locationType_superOrganizationName x
            , schemaTypeToXML "institutionContact" $ locationType_institutionContact x
            , schemaTypeToXML "institutionSustainability" $ locationType_institutionSustainability x
            , schemaTypeToXML "institutionPolicies" $ locationType_institutionPolicies x
            , schemaTypeToXML "versioning" $ locationType_versioning x
            ]
 
data PolicyType = PolicyType
        { policyType_policyType :: Maybe PolicyTypes
        , policyType_appliesToProduct :: Maybe Xsd.Boolean
          -- ^ appliesToProduct is true for policies that apply to this 
          --   product
        , policyType_choice0 :: OneOf2 Xsd.XsdString Xs.AnyURI
          -- ^ Choice between:
          --   
          --   (1) freeTextPolicy
          --   
          --   (2) refPolicy
        }
        deriving (Eq,Show)
instance SchemaType PolicyType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "policyType" e pos
        a1 <- optional $ getAttribute "appliesToProduct" e pos
        commit $ interior e $ return (PolicyType a0 a1)
            `apply` oneOf' [ ("Xsd.XsdString", fmap OneOf2 (parseSchemaType "freeTextPolicy"))
                           , ("Xs.AnyURI", fmap TwoOf2 (parseSchemaType "refPolicy"))
                           ]
    schemaTypeToXML s x@PolicyType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "policyType") $ policyType_policyType x
                       , maybe [] (toXMLAttribute "appliesToProduct") $ policyType_appliesToProduct x
                       ]
            [ foldOneOf2  (schemaTypeToXML "freeTextPolicy")
                          (schemaTypeToXML "refPolicy")
                          $ policyType_choice0 x
            ]
 
data SupplementaryProductType = SupplementaryProductType
        { supplementaryProductType_basicMetadata :: BasicMetadata
        , supplementaryProductType_resourceID :: Maybe ResourceID
        , supplementaryProductType_resourceType :: ResourceType
        , supplementaryProductType_format :: Maybe Format
        , supplementaryProductType_resourceMetadataSource :: Maybe ResourceMetadataSource
        , supplementaryProductType_location :: LocationType
        }
        deriving (Eq,Show)
instance SchemaType SupplementaryProductType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SupplementaryProductType
            `apply` parseSchemaType "basicMetadata"
            `apply` optional (parseSchemaType "resourceID")
            `apply` parseSchemaType "resourceType"
            `apply` optional (parseSchemaType "Format")
            `apply` optional (parseSchemaType "resourceMetadataSource")
            `apply` parseSchemaType "location"
    schemaTypeToXML s x@SupplementaryProductType{} =
        toXMLElement s []
            [ schemaTypeToXML "basicMetadata" $ supplementaryProductType_basicMetadata x
            , maybe [] (schemaTypeToXML "resourceID") $ supplementaryProductType_resourceID x
            , schemaTypeToXML "resourceType" $ supplementaryProductType_resourceType x
            , maybe [] (schemaTypeToXML "Format") $ supplementaryProductType_format x
            , maybe [] (schemaTypeToXML "resourceMetadataSource") $ supplementaryProductType_resourceMetadataSource x
            , schemaTypeToXML "location" $ supplementaryProductType_location x
            ]
