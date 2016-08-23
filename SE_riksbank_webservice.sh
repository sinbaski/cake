#!/bin/bash

curl -X POST -H "Content-Type: text/xml" \
    -H "SOAPAction: \"http://swea.riksbank.se/xsd\"" \
    --data-binary @request.xml \
    http://swea.riksbank.se/xsd




