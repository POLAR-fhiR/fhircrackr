% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.R
\docType{data}
\name{transaction_bundle_example}
\alias{transaction_bundle_example}
\alias{example_resource1}
\alias{example_resource2}
\alias{example_resource3}
\title{Toy examples to POST/PUT on a server}
\format{
An object of class \code{fhir_bundle_serialized} of length 1277.

An object of class \code{fhir_resource_serialized} of length 267.

An object of class \code{fhir_resource_serialized} of length 290.

An object of class \code{fhir_resource_serialized} of length 608.
}
\source{
\strong{transaction_bundle_example}

\if{html}{\out{<div class="sourceCode">}}\preformatted{<Bundle>
   <type value='transaction'/>
	  <entry>
	     <resource>
	        <Patient>
	           <id value='id1'/>
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
	            <id value='id3'/>
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
</Bundle>
}\if{html}{\out{</div>}}

\strong{example_resource1}

\if{html}{\out{<div class="sourceCode">}}\preformatted{<Patient>
   <name>
	     <given value = 'Marie'/>
	  </name>
	  <gender value = 'female'/>
	  <birthDate value = '1970-01-01'/>
</Patient>
}\if{html}{\out{</div>}}

\strong{example_resource2}

\if{html}{\out{<div class="sourceCode">}}\preformatted{<Patient>
   <id value = '1a2b3c'/>
   <name>
	     <given value = 'Marie'/>
	  </name>
	  <gender value = 'female'/>
	  <birthDate value = '1970-01-01'/>
</Patient>
}\if{html}{\out{</div>}}

\strong{example_resource3}

\if{html}{\out{<div class="sourceCode">}}\preformatted{<Medication>
    <code>
        <coding>
            <system value="http://www.nlm.nih.gov/research/umls/rxnorm"/>
            <code value="1594660"/>
            <display value="Alemtuzumab 10mg/ml (Lemtrada)"/>
        </coding>
    </code>
    <ingredient id="1">
    	<itemReference>
     		<reference value="Substance/5463"/>
   		</itemReference>
    </ingredient>
    <ingredient id="2">
    	<itemReference>
     		<reference value="Substance/3401"/>
 		</itemReference>
    </ingredient>
</Medication>
}\if{html}{\out{</div>}}
}
\usage{
transaction_bundle_example

example_resource1

example_resource2

example_resource3
}
\description{
These data examples are simple examples to try out POSTing/PUTing resources
to a server. See \strong{Source} for how the xml versions look.
}
\details{
\code{transaction_bundle_example} contains 1 transaction bundle with 2 Patient resources.

\code{example_resource1} contains 1 patient resource without id for POSTing

\code{example_resource2} contains 1 patient resource with id for PUTing

\code{example_resource3} contains 1 Medication resource with an id xml attribute
}
\examples{
#unserialize xml objects before doing anything else with them!
fhir_unserialize(bundles = transaction_bundle_example)
#unserialize xml objects before doing anything else with them!
fhir_unserialize(example_resource1)
#unserialize xml objects before doing anything else with them!
fhir_unserialize(example_resource2)
#unserialize xml objects before doing anything else with them!
fhir_unserialize(example_resource3)
}
\keyword{datasets}
