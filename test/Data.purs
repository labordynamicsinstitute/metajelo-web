module Test.Data where

import Prelude

fakeXmlns :: String
fakeXmlns = "http://fake.xmlns.edu"

recXmlnsFakeXml :: String
recXmlnsFakeXml = """<?xml version="1.0" encoding="UTF-8"?>
<record xmlns:re3="http://www.re3data.org/schema/2-2"
 xmlns:datacite="http://datacite.org/schema/kernel-4"
 xmlns="""
  <> "\"" <> fakeXmlns <> "\"" <> """
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://ourdomain.cornell.edu/reuse/v.01 file:/Users/clagoze/Downloads/metajelo-master/schema/xsd/reproMetadata0.7.xsd">
    <identifier identifierType="EISSN">XYZW</identifier>
</record>
"""

metajeloXml :: String
metajeloXml = """<?xml version="1.0" encoding="UTF-8"?>
<record xmlns:re3="http://www.re3data.org/schema/2-2"
 xmlns:datacite="http://datacite.org/schema/kernel-4"
 xmlns="http://ourdomain.cornell.edu/reuse/v.01"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://ourdomain.cornell.edu/reuse/v.01 file:/Users/clagoze/Downloads/metajelo-master/schema/xsd/reproMetadata0.7.xsd">
    <identifier identifierType="EISSN">OjlTjf</identifier>
    <date>2020-04-04</date>
    <lastModified>2019-05-04Z</lastModified>
    <relatedIdentifier relatedIdentifierType="LSID" relationType="IsDerivedFrom">v7Ra9f_</relatedIdentifier>
    <relatedIdentifier relatedIdentifierType="PMID" relationType="IsNewVersionOf">sm3AM1NbOSx</relatedIdentifier>
    <supplementaryProducts>
        <supplementaryProduct>
            <basicMetadata>
                <Title>niBi6PpDgbhM3</Title>
                <Creator>cbK1</Creator>
                <PublicationYear>2019-08-11Z</PublicationYear>
            </basicMetadata>
            <resourceID relatedIdentifierType="IGSN">bW8w2m5bzZ0WoKj7SBI_</resourceID>
            <resourceType resourceTypeGeneral="Event">cNMAxYjF0j0k</resourceType>
            <Format>
                <format>aPd4QER93hRARj3HudkWUwratMGEd</format>
                <format>Vf5ti6</format>
            </Format>
            <resourceMetadataSource relationType="HasMetadata">http://HgMuxvbx.au/</resourceMetadataSource>
            <location>
                <institutionID identifierType="ARK">institutionID0</institutionID>
                <institutionName>pKhb</institutionName>
                <institutionType>commercial</institutionType>
                <superOrganizationName>DHv5J4LquWfN42iu1a</superOrganizationName>
                <institutionContact institutionContactType="dataCustodian">foo@baz.edu</institutionContact>
                <institutionSustainability>
                    <missionStatementURL>http://akbNcujU.fz/</missionStatementURL>
                    <fundingStatementURL
>http://tdjmeVUQ.lm/</fundingStatementURL>
                </institutionSustainability>
                <institutionPolicies>
                    <institutionPolicy policyType="Quality" appliesToProduct="0">
                        <refPolicy>http://skGHargw.com/</refPolicy>
                    </institutionPolicy>
                    <institutionPolicy policyType="Preservation" appliesToProduct="1">
                        <freeTextPolicy>fqxRlcso3</freeTextPolicy>
                    </institutionPolicy>
                </institutionPolicies>
                <versioning>true</versioning>
            </location>
        </supplementaryProduct>
        <supplementaryProduct>
            <basicMetadata>
                <Title>M._y</Title>
                <Creator>T4nUil6</Creator>
                <PublicationYear>2020-09-16Z</PublicationYear>
            </basicMetadata>
            <resourceID relatedIdentifierType="IGSN">lCi7-M50qjeFNhiAt</resourceID>
            <resourceType resourceTypeGeneral="Sound">ewNM9_1KtEgas9spr8PEY</resourceType>
            <Format>
                <format>S2Zq5</format>
                <format>JmjVZzqUrJ653r4_9Y8ex6RpZ</format>
            </Format>
            <resourceMetadataSource relationType="HasMetadata">http://iEhiBPjr.foo/</resourceMetadataSource>
            <location>
                <institutionID identifierType="ARK">institutionID1</institutionID>
                <institutionID>URL</institutionID>
                <institutionName>m0-XHPS</institutionName>
                <institutionType>commercial</institutionType>
                <superOrganizationName>Ld0KhpgrA_LdvGgp-WDVZgeIgtJkM</superOrganizationName>
                <institutionContact institutionContactType="dataCustodian">foo@bar.edu</institutionContact>
                <institutionSustainability>
                    <missionStatementURL>http://RadUMcWC.baz/</missionStatementURL>
                    <fundingStatementURL>http://YWYwhJyz.bar/</fundingStatementURL>
                </institutionSustainability>
                <institutionPolicies>
                    <institutionPolicy policyType="Data" appliesToProduct="0">
                        <freeTextPolicy>lo8H7YsHaOEYf4BvtW_RXXHFZ</freeTextPolicy>
                    </institutionPolicy>
                    <institutionPolicy policyType="Data" appliesToProduct="true">
                        <freeTextPolicy>eRNBB2</freeTextPolicy>
                    </institutionPolicy>
                </institutionPolicies>
                <versioning>1</versioning>
            </location>
        </supplementaryProduct>
    </supplementaryProducts>
</record>
"""

metajeloXmlPrefixed :: String
metajeloXmlPrefixed = """<?xml version="1.0" encoding="UTF-8"?>
<v.01:record xmlns:re3="http://www.re3data.org/schema/2-2"
 xmlns:datacite="http://datacite.org/schema/kernel-4"
 xmlns:v.01="http://ourdomain.cornell.edu/reuse/v.01"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://ourdomain.cornell.edu/reuse/v.01 file:/Users/clagoze/Documents/Work/PAPERS/metajelo%20idcc%202018/reproMetadata0.7.xsd">
    <v.01:identifier identifierType="ARK">identifier0</v.01:identifier>
    <v.01:date>2006-05-04</v.01:date>
    <v.01:lastModified>2006-05-04</v.01:lastModified>
    <v.01:relatedIdentifier relatedIdentifierType="ARK" relationType="IsCitedBy">relatedIdentifier0</v.01:relatedIdentifier>
    <v.01:relatedIdentifier relatedIdentifierType="ARK" relationType="IsCitedBy">relatedIdentifier1</v.01:relatedIdentifier>
    <v.01:supplementaryProducts>
        <v.01:supplementaryProduct>
            <v.01:basicMetadata>
                <v.01:Title>Title0</v.01:Title>
                <v.01:Creator>Creator0</v.01:Creator>
                <v.01:PublicationYear>2006-05-04</v.01:PublicationYear>
            </v.01:basicMetadata>
            <v.01:resourceType resourceTypeGeneral="Audiovisual">resourceType0</v.01:resourceType>
            <v.01:location>
                <v.01:institutionID identifierType="ARK">institutionID0</v.01:institutionID>
                <v.01:institutionName>institutionName0</v.01:institutionName>
                <v.01:institutionType>commercial</v.01:institutionType>
                <v.01:institutionContact>foo@bar.edu</v.01:institutionContact>
                <v.01:institutionSustainability>
                    <v.01:missionStatementURL>http://www.oxygenxml.com/</v.01:missionStatementURL>
                    <v.01:fundingStatementURL>http://www.oxygenxml.com/</v.01:fundingStatementURL>
                </v.01:institutionSustainability>
                <v.01:institutionPolicies>
                    <v.01:institutionPolicy>
                        <v.01:refPolicy>http://www.oxygenxml.com/</v.01:refPolicy>
                    </v.01:institutionPolicy>
                    <v.01:institutionPolicy>
                        <v.01:freeTextPolicy>freeTextPolicy0</v.01:freeTextPolicy>
                    </v.01:institutionPolicy>
                </v.01:institutionPolicies>
                <v.01:versioning>false</v.01:versioning>
            </v.01:location>
        </v.01:supplementaryProduct>
        <v.01:supplementaryProduct>
            <v.01:basicMetadata>
                <v.01:Title>Title1</v.01:Title>
                <v.01:Creator>Creator1</v.01:Creator>
                <v.01:PublicationYear>2006-05-04</v.01:PublicationYear>
            </v.01:basicMetadata>
            <v.01:resourceType resourceTypeGeneral="Audiovisual">resourceType1</v.01:resourceType>
            <v.01:location>
                <v.01:institutionID identifierType="ARK">institutionID1</v.01:institutionID>
                <v.01:institutionName>institutionName1</v.01:institutionName>
                <v.01:institutionType>commercial</v.01:institutionType>
                <v.01:institutionContact>foo@baz.au</v.01:institutionContact>
                <v.01:institutionSustainability>
                    <v.01:missionStatementURL>http://www.oxygenxml.com/</v.01:missionStatementURL>
                    <v.01:fundingStatementURL>http://www.oxygenxml.com/</v.01:fundingStatementURL>
                </v.01:institutionSustainability>
                <v.01:institutionPolicies>
                    <v.01:institutionPolicy>
                        <v.01:freeTextPolicy>freeTextPolicy1</v.01:freeTextPolicy>
                    </v.01:institutionPolicy>
                    <v.01:institutionPolicy>
                        <v.01:freeTextPolicy>freeTextPolicy2</v.01:freeTextPolicy>
                    </v.01:institutionPolicy>
                </v.01:institutionPolicies>
                <v.01:versioning>false</v.01:versioning>
            </v.01:location>
        </v.01:supplementaryProduct>
    </v.01:supplementaryProducts>
</v.01:record>
"""
