

bundle <- xml2::read_xml("
<Bundle>
  <type value='transaction'/>
  <entry>
    <resource>
      <Patient>
        <address>
          <use value='home'/>
          <city value='Amsterdam'/>
          <type value='physical'/>
          <country value='Netherlands'/>
        </address>
        <name>
          <given value='Marie'/>
        </name>
      </Patient>
    </resource>
    <request>
      <method value='POST'/>
      <url value='Patient'/>
    </request>
  </entry>
  <entry>
    <resource>
      <Patient>
        <address>
          <use value='home'/>
          <city value='Berlin'/>
        </address>
        <address>
          <use value='work'/>
          <city value='London'/>
          <type value='postal'/>
          <country value='England'/>
        </address>
        <address>
          <type value='postal'/>
          <country value='France'/>
        </address>
        <name>
          <given value='Frank'/>
        </name>
        <name>
          <given value='Max'/>
        </name>
      </Patient>
    </resource>
    <request>
      <method value='POST'/>
      <url value='Patient'/>
    </request>
  </entry>
</Bundle>")

bundle <- fhir_bundle_xml(bundle)

bundle <- fhir_serialize(bundle)

transaction_bundle_example <- fhir_bundle_list(list(bundle))

usethis::use_data(transaction_bundle_example, overwrite = TRUE)
